{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Curry.ConvertToHs
  ( compileToHs, haskellName
  , mkCurryCtxt, mkFlatPattern
  , HsEquivalent
  , ToHsName(..), ToMonadicHsName(..), UnqualName(..)
  , convertQualNameToFlatName, convertQualNameToFlatQualName
  ) where

import Control.Arrow (first, second)
import Control.Monad (void, replicateM, when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader (..), runReaderT)
import Control.Monad.State (StateT, MonadState (..), evalStateT, modify)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Coerce (coerce)
import Data.Char (toLower, toUpper)
import Data.List (isPrefixOf, sort, find, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts hiding (Literal, Cons, Kind, QName)
import qualified Language.Haskell.Exts as Hs
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension, replaceBaseName, takeBaseName)
import System.IO (openFile, IOMode (..), utf8, hSetEncoding, hPutStr, hClose, hGetContents')

import Base.Messages (status, abortWithMessages, message)
import Curry.Base.Message (Message(..))
import Curry.Base.Ident (ModuleIdent)
import Curry.Base.Pretty (text)
import Curry.Base.SpanInfo (SpanInfo(..))
import Curry.FlatCurry hiding (Let)
import Curry.FlatCurry.Annotated.Type (APattern(..), ABranchExpr(..), AExpr(..))
import Curry.FlatCurry.Typed.Type (TRule(..), TFuncDecl(..), TProg(..), TExpr (..))
import Curry.Files.Filenames (addOutDirModule)
import Generators.GenTypedFlatCurry (genTypedExpr)
import CompilerOpts (Options(..))
import CurryBuilder (smake, compMessage)

import Options (KMCCOpts(..), dumpMessage)
import Curry.Analysis (NDAnalysisResult, NDInfo (..))
import Curry.Annotate (annotateND, isFunFree, exprAnn, annotateND')
import Curry.GenInstances (genInstances)
import Curry.ConvertUtils

newtype CM a = CM {
    runCM :: ReaderT NDAnalysisResult (ExceptT [Message] (StateT Int IO)) a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO
                     , MonadReader NDAnalysisResult
                     , MonadError [Message]
                     , MonadState Int
                     )

evalCM :: CM a -> NDAnalysisResult -> IO a
evalCM a analysis = do
  eith <- evalStateT (runExceptT (runReaderT (runCM a) analysis)) 0
  case eith of
    Left msgs -> abortWithMessages msgs
    Right res -> return res

freshVarName :: CM (Hs.Name ())
freshVarName = do
  i <- get
  modify succ
  return (Ident () $  "x_" ++ show i)

compileToHs :: Maybe TypeExpr -> [(TProg, ModuleIdent, FilePath)] -> NDAnalysisResult -> KMCCOpts
            -> IO ()
compileToHs mainType mdls ndInfo opts =
  evalCM (mapM_ process' (zip [1 ..] (addMainInfo mdls))) ndInfo
  where
    addMainInfo [] = []
    addMainInfo [(x, y, z)] = [(x, y, z, mainType)]
    addMainInfo ((x, y, z):xs) = (x, y, z, Nothing) : addMainInfo xs
    total = length mdls
    process' (n, (prog, m, fp, mi)) = process opts (n, total) prog m fp mi deps
      where deps = map (\(_, _, dep) -> dep) (take n mdls)

process :: KMCCOpts -> (Int, Int) -> TProg
        -> ModuleIdent -> FilePath -> Maybe TypeExpr -> [FilePath] -> CM ()
process kopts idx@(thisIdx,maxIdx) tprog m fn mi deps
  | optForce opts = compile
  | otherwise     = smake [destFile] deps compile skip
  where
    destFile = tgtDir (haskellName fn)
    skip = do
      status opts $ compMessage idx (11, 16) "Skipping" m (fn, destFile)
      when (optCompilerVerbosity kopts > 2) $ do -- TODO: implement check file when dumping stuff only
        eithRes <- liftIO (parseFile' destFile)
        case eithRes of
          ParseFailed loc err -> do
            liftIO $ putStr $ unlines
              [ "Haskell file is corrupted."
              , "For the file \"" ++ fn ++ "\"."
              , "Parsing failed with:"
              , err
              , "At location:"
              , prettyPrintStyleMode hsPrettyPrintStyle hsPrettyPrintMode loc
              , "Retrying compilation from analyzed flat curry ..." ]
            compile
          ParseOk res ->
            if thisIdx == maxIdx
              then liftIO $ dumpMessage kopts $ "Read cached Haskell file:\n" ++
                        prettyPrintStyleMode hsPrettyPrintStyle hsPrettyPrintMode res
              else liftIO $ dumpMessage kopts "Read cached Haskell file"
    compile = do
      status opts $ compMessage idx (11, 16) "Translating" m (fn, destFile)
      res <- convertToHs (TWFP (tprog, externalName fn, mi, kopts))
      let printed = prettyPrintStyleMode hsPrettyPrintStyle hsPrettyPrintMode res
      if thisIdx == maxIdx
        then liftIO $ dumpMessage kopts $ "Generated Haskell file:\n" ++ printed
        else liftIO $ dumpMessage kopts $ "Generated Haskell file."
      liftIO $ writeUTF8File' (tgtDir (haskellName fn)) printed
      return ()

    opts = frontendOpts kopts

    tgtDir = addOutDirModule (optUseOutDir opts) (optOutDir opts) m

hsPrettyPrintMode :: PPHsMode
hsPrettyPrintMode = PPHsMode 2 2 2 2 2 2 2 False PPOffsideRule False

hsPrettyPrintStyle :: Style
hsPrettyPrintStyle = Style PageMode 500 2

class ToHs a where
  convertToHs :: a -> CM (HsEquivalent a)

class ToMonadicHs a where
  convertToMonadicHs :: a -> CM (HsEquivalent a)

newtype TProgWithFilePath = TWFP (TProg, FilePath, Maybe TypeExpr, KMCCOpts)
type instance HsEquivalent TProgWithFilePath = Module ()
instance ToHs TProgWithFilePath where
  convertToHs (TWFP (TProg nm im tys fs _, fp, mi, opts)) = do
    (header, curryImports, curryDs) <- do
        im' <- mapM (convertToHs . IS) im
        tyds <- mapM convertToHs tys
        funds <- mapM convertToHs fs
        tydsM <- mapM convertToMonadicHs tys
        fs' <- case mi of
          Just ty -> patchMainPre ty opts fs
          Nothing -> return fs
        fundsM <- mapM convertToMonadicHs fs'
        let visT = mapMaybe getVisT tys
        let visF = mapMaybe getVisF fs
        header <- convertToHs (MS (nm, visT, visF))
        let extract (Just (x, y)) = [x,y]
            extract Nothing = []
        let insts = concatMap genInstances tys
        let ds = insts ++ coerce (tyds ++ tydsM) ++ concatMap @[] extract (coerce (funds ++ fundsM))
        return (header, im', ds)
    (extPragmas, extImports, extDs) <- liftIO $ doesFileExist fp >>= \case
      True -> do
        ext <- liftIO $ parseFile' fp
        case fmap void ext of
          ParseOk (Module _ _ p i d) -> return (p, i, d)
          ParseOk _           -> return ([] , [], [])
          ParseFailed loc err -> do
            liftIO $ fail $ unlines
              [ "External definitions file is corrupted."
              , "For the file \"" ++ fp ++ "\"."
              , "Parsing failed with:"
              , err
              , "At location:"
              , prettyPrint loc
              , "Aborting compilation ..." ]
      False -> return ([], [], [])
    let ps' = defaultPragmas ++ extPragmas
    let im' = defaultImports ++ extImports ++ curryImports
    let ds' = extDs ++ curryDs
    (header', ds'') <- case mi of
          Just ty -> patchMainPost ty opts header ds'
          Nothing -> return (header, ds')
    return (Module () (Just header') ps' im' ds'')
    where
      getVisT (Type qname Public _ cs) = Just (qname, mapMaybe getVisC cs)
      getVisT (TypeSyn qname Public _ _) = Just (qname, [])
      getVisT (TypeNew qname Public _ c) = Just (qname, maybe [] return (getVisN c))
      getVisT _ = Nothing

      getVisC (Cons qname _ Public _vs) = Just qname
      getVisC _ = Nothing

      getVisN (NewCons qname Public _) = Just qname
      getVisN _ = Nothing

      getVisF (TFunc qname _ Public _ _) = Just qname
      getVisF _ = Nothing


-- after pre-patching:
-- main still has the same name, but it gets all variables specified on the cmd as arguments.
-- However, if main is recursive, the recursive call is unchanged, i.e., wrong for now.
patchMainPre :: TypeExpr -> KMCCOpts -> [TFuncDecl] -> CM [TFuncDecl]
patchMainPre ty opts fs = case ty of
  TCons ("Prelude","IO") _ -> return fs
  _                        -> do
    mapM patchMainDecl fs
    where
      patchMainDecl (TFunc qname _ vis fty (TRule [] e))
        | snd qname == "main" = do
          let (vs, e') = splitFreeVars e
          let check [] = return []
              check ((n, i):xs)
                | Just v <- find ((==i) . fst) vs = (v:) <$> check xs
                | otherwise         = throwError $ return @[] $ Message NoSpanInfo $ text $
                  "Variable specified on the command line with name and index " ++
                  show (n, i) ++
                  " is not free in the expression."
          specifiedVs <- check (optVarNames opts)
          let ty' = foldr (FuncType . snd) fty specifiedVs
          return (TFunc qname (length specifiedVs) vis ty' (TRule specifiedVs (TFree (vs \\ specifiedVs) e')))
      patchMainDecl f = return f

-- Now we need to
-- generate the replacement for the function that was extended with arguments (gets the mainND name)
-- update the name of the function that was extended with arguments to mainND##
-- call the wrapper on the mainND## function, or if main is actually deterministic or IO, call the respective wrapper
patchMainPost :: TypeExpr -> KMCCOpts -> ModuleHead () -> [Decl ()] -> CM (ModuleHead (), [Decl ()])
patchMainPost ty opts (ModuleHead _ nm w (Just (ExportSpecList _ es))) ds = do -- TODO: pass options to wrapper
  let hasDetMain = EVar () (Qual () nm (Ident () "main_Det")) `elem` es
  let mainExport = EVar () (Qual () nm (Ident () "main##"))

  (mainExpr, ds') <- case ty of
      TCons ("Prelude","IO") _
        | hasDetMain -> return (App () (Hs.Var () mainWrapperDetQualName) (Hs.Var () (Qual () nm (Ident () "main_Det"))), ds)
        | otherwise  -> return (App () (Hs.Var () mainWrapperNDetQualName) (Hs.Var () (Qual () nm (Ident () "main_ND"))), ds)
      _
        | hasDetMain -> return (App () (Hs.Var () exprWrapperDetQualName) (Hs.Var () (Qual () nm (Ident () "main_Det"))), ds)
        | otherwise  -> do
          let findMainDecl [] = throwError $ return @[] $ Message NoSpanInfo $ text "Main function not found"
              findMainDecl ((FunBind _ [Match _ (Ident () "main_ND") [] (UnGuardedRhs () e) Nothing]):bs) = return (e, bs)
              findMainDecl (b:bs) = second (b:) <$> findMainDecl bs
              findMainSig [] = throwError $ return @[] $ Message NoSpanInfo $ text "Main type signature not found"
              findMainSig ((TypeSig _ [Ident () "main_ND"] mainTy):bs) = return (mainTy, bs)
              findMainSig (b:bs) = second (b:) <$> findMainSig bs
          (mainRhsE, withoutMainDecl) <- findMainDecl ds
          (mainType, rest) <- findMainSig withoutMainDecl
          let mainPats = getLiftedPats mainRhsE
          let getVar (PVar _ v) = Just v
              getVar _          = Nothing
          let mainVs = mapMaybe getVar mainPats

          let mainNDHashDecl = FunBind () [Match () (Ident () "mainND##") [] (UnGuardedRhs () mainRhsE) Nothing]
          let mainNDHashType = TypeSig () [Ident () "mainND##"] mainType

          let e' = foldl (\e -> mkMonadicApp e . Hs.Var () . UnQual ()) (Hs.Var () (Qual () nm (Ident () "mainND##"))) mainVs
          let mainNDExpr = foldr mkShareBind e' (zip3 mainVs (repeat mkFree) (repeat One))
          let mainNDDecl = FunBind () [Match () (Ident () "main_ND") [] (UnGuardedRhs () mainNDExpr) Nothing]

          let mainE = mkVarReturn opts mainVs (Hs.Var () (Qual () nm (Ident () "mainND##")))
          let varInfos = List () $ map (\(s,i) -> Tuple () Boxed [Hs.Lit () (String () s s), Hs.Lit () (Int () (toInteger i) (show i))]) (optVarNames opts)
          let bindingOpt = Hs.Var () $ if optShowBindings opts then trueQualName else falseQualName
          return (App () (App () (App () (Hs.Var () exprWrapperNDetQualName) varInfos) bindingOpt) mainE, mainNDDecl:mainNDHashDecl:mainNDHashType:rest)

  let mainDecl = PatBind () (PVar () (Ident () "main##")) (UnGuardedRhs () mainExpr) Nothing
  return (ModuleHead () nm w (Just (ExportSpecList () (mainExport:es))), mainDecl:ds')
patchMainPost _ _ h ds = return (h, ds)

getLiftedPats :: Exp () -> [Pat ()]
getLiftedPats (Hs.App _ _ (Lambda _ [p] e)) = p : getLiftedPats e
getLiftedPats _ = []

splitFreeVars :: TExpr -> ([(VarIndex, TypeExpr)], TExpr)
splitFreeVars (TFree vs e) = first (vs++) (splitFreeVars e)
splitFreeVars e = ([], e)

mkVarReturn :: KMCCOpts -> [Name ()] -> Exp () -> Exp ()
mkVarReturn opts fvs = go (map snd (optVarNames opts))
  where
    go xs e =
      let mainE = foldl (\e' -> mkMonadicApp e' . Hs.Var () . UnQual ()) e fvs
          eWithInfo = mkAddVarIds mainE (map (mkGetVarId . Hs.Var () . UnQual () . indexToName) (sort xs))
      in foldr mkShareBind eWithInfo (zip3 fvs (repeat mkFree) (replicate (length fvs) Many))

newtype ModuleHeadStuff = MS (String, [(QName, [QName])], [QName])
type instance HsEquivalent ModuleHeadStuff = ModuleHead ()
instance ToHs ModuleHeadStuff where
  convertToHs (MS (s, qnamesT, qnamesF)) = do
    let tyEx = concatMap typeItem qnamesT
    fnEx <- concat <$> mapM funcItem qnamesF
    return $ ModuleHead () (ModuleName () ("Curry_" ++ s)) Nothing $
      Just (ExportSpecList () (tyEx ++ fnEx))
    where
      typeItem (qname, [])      = map (EAbs () (NoNamespace ()))
        [convertTypeNameToHs qname, convertTypeNameToMonadicHs qname]
      typeItem (qname, csNames) = map (uncurry (EThingWith () (NoWildcard ())))
        [ (convertTypeNameToHs qname, concatMap conItem csNames)
        , (convertTypeNameToMonadicHs qname, ConName () (convertQualNameToFlatName qname) : map conItemMonadic csNames) ]

      conItem qname = [ConName () (convertTypeNameToHs (Unqual qname))]
      conItemMonadic qname = ConName () (convertTypeNameToMonadicHs (Unqual qname))

      funcItem qname = do
        analysis <- ask
        return (EVar () (convertFuncNameToMonadicHs qname) : case Map.lookup qname analysis of
          Just Det -> [EVar () $ convertFuncNameToHs qname]
          _        -> [])

newtype ImportString = IS String
type instance HsEquivalent ImportString = ImportDecl ()
instance ToHs ImportString where
  convertToHs (IS s) = return $ ImportDecl () (ModuleName () ("Curry_" ++ s)) False False False Nothing Nothing Nothing

newtype HsTypeDecl = HTD (Decl ())
type instance HsEquivalent TypeDecl = HsTypeDecl
instance ToHs TypeDecl where
  convertToHs (Type qname@(mdl, nm) _ vs cs)
    | [] <- cs = return $ HTD $ TypeDecl () (mkTypeHead qname []) $ -- eta reduce -> ignore vars
      TyCon () (Qual () (ModuleName () ("Curry_" ++ mdl)) (Ident () (escapeTypeName nm ++ "_Det#")))
    | otherwise = do
      cs' <- mapM convertToHs cs
      return $ HTD $
        DataDecl () (DataType ()) Nothing (mkTypeHead qname vs) cs' []
  convertToHs (TypeSyn qname _ vs texpr) = do
    ty <- convertToHs texpr
    return $ HTD $
      TypeDecl () (mkTypeHead qname vs) ty
  convertToHs (TypeNew qname _ vs (NewCons cname cvis texpr)) = do
    c' <- convertToHs (Cons cname 1 cvis [texpr])
    return $ HTD $
      DataDecl () (DataType ()) Nothing (mkTypeHead qname vs) [c'] []

instance ToMonadicHs TypeDecl where
  convertToMonadicHs (Type qname@(mdl, nm) _ vs cs)
    | [] <- cs = return $ HTD $ TypeDecl () (mkMonadicTypeHead qname []) $ -- eta reduce -> ignore vars
      TyCon () (Qual () (ModuleName () ("Curry_" ++ mdl)) (Ident () (escapeTypeName nm ++ "_ND#")))
    | otherwise = do
      cs' <- (mkFlatConstr qname vs :) <$> mapM convertToMonadicHs cs
      return $ HTD $
        DataDecl () (DataType ()) Nothing (mkMonadicTypeHead qname vs) cs' []
  convertToMonadicHs (TypeSyn qname _ vs texpr) =  do
    ty <- convertToMonadicHs texpr
    return $ HTD $
      TypeDecl () (mkMonadicTypeHead qname vs) ty
  convertToMonadicHs (TypeNew qname _ vs (NewCons cname cvis texpr)) = do
    c' <- convertToMonadicHs (Cons cname 1 cvis [texpr])
    let cs' = [mkFlatConstr qname vs, c']
    return $ HTD $
      DataDecl () (DataType ()) Nothing (mkMonadicTypeHead qname vs) cs' []

mkFlatConstr :: QName -> [TVarWithKind] -> QualConDecl ()
mkFlatConstr qname vs =
  let hsEquiv = TyApp () (TyCon () hsEquivQualName) $
                foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname)) $
                map (TyVar () . indexToName . fst) vs
  in QualConDecl () Nothing Nothing $ ConDecl () (convertQualNameToFlatName qname) [hsEquiv]

type instance HsEquivalent ConsDecl = QualConDecl ()
instance ToHs ConsDecl where
  convertToHs (Cons cname _ _ texprs) = do
    tys <- mapM convertToHs texprs
    return $ QualConDecl () Nothing Nothing (ConDecl () (convertTypeNameToHs (Unqual cname)) tys)

instance ToMonadicHs ConsDecl where
  convertToMonadicHs (Cons cname _ _ texprs) = do
    tys <- mapM convertQualType texprs
    return $ QualConDecl () Nothing Nothing (ConDecl () (convertTypeNameToMonadicHs (Unqual cname)) tys)

type instance HsEquivalent TypeExpr = Type ()
instance ToHs TypeExpr where
  convertToHs (TVar idx) = return $ TyVar () (indexToName idx)
  convertToHs (FuncType t1 t2) = TyFun () <$> convertToHs t1 <*> convertToHs t2
  convertToHs (TCons qn tys) = foldl (TyApp ()) (TyCon () (convertTypeNameToHs qn)) <$> mapM convertToHs tys
  convertToHs (ForallType vs t) = TyForall () (Just $ map toTyVarBndr vs) Nothing <$> convertToHs t

instance ToMonadicHs TypeExpr where
  convertToMonadicHs (TVar idx) = return $ TyVar () (indexToName idx)
  convertToMonadicHs (FuncType t1 t2) = mkLiftedFunc <$> convertToMonadicHs t1 <*> convertToMonadicHs t2
  convertToMonadicHs (TCons qn tys) = foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qn)) <$> mapM convertToMonadicHs tys
  convertToMonadicHs (ForallType vs t) = TyForall () (Just (map toTyVarBndr vs)) (mkCurryCtxt vs) <$> convertQualType t

convertQualType :: TypeExpr -> CM (Type ())
convertQualType ty
  | ForallType _ _ <- ty = ty'
  | otherwise            = mkCurry <$> ty'
  where
    ty' = convertToMonadicHs ty

mkCurryCtxt :: [TVarWithKind] -> Maybe (Context ())
mkCurryCtxt = mkQuantifiedCtxt mkCurryClassType

mkQuantifiedCtxt :: (Type () -> Type ()) -> [TVarWithKind] -> Maybe (Context ())
mkQuantifiedCtxt _      [] = Nothing
mkQuantifiedCtxt mkType vs = Just $ CxTuple () $ map (mkQuantifiedFor mkType) vs

mkQuantifiedFor :: (Type () -> Type ()) -> TVarWithKind -> Asst ()
mkQuantifiedFor mkType (i, KStar) = TypeA () $ mkType (TyVar () (indexToName i))
mkQuantifiedFor mkType (i, k) = construct $ map (mkQuantifiedFor mkType) argsWithTVar
  where
    (args, _) = splitKindArrow k
    argTVars = take (length args) [i+1..]
    argsWithTVar = zip argTVars args
    construct args' = TypeA () $ TyForall () (Just (map toTyVarBndr argsWithTVar))
      (Just (CxTuple () args'))
      (mkType (foldl (TyApp ()) (TyVar () (indexToName i)) (map (TyVar () . indexToName) argTVars)))

newtype HsFuncDecl = HFD (Maybe (Decl (), Decl ()))
type instance HsEquivalent TFuncDecl = HsFuncDecl
instance ToHs TFuncDecl where
  convertToHs (TFunc qname _ _ texpr rule) = do
    analysis <- ask
    case Map.lookup qname analysis of
      Just Det -> do
        ty <- convertToHs texpr
        let tsig = TypeSig () [convertFuncNameToHs (Unqual qname)] ty
        match <- convertToHs (RI (qname, rule))
        let f = FunBind () [match]
        return $ HFD $ Just (tsig, f)
      _ -> return $ HFD Nothing

instance ToMonadicHs TFuncDecl where
  convertToMonadicHs (TFunc qname _ _ texpr rule) = do
    ty <- convertQualType (normalizeCurryType texpr)
    let tsig = TypeSig () [convertFuncNameToMonadicHs (Unqual qname)] ty
    match <- convertToMonadicHs (RI (qname, rule))
    let f = FunBind () [match]
    return $ HFD $ Just (tsig, f)

mkTypeHead :: QName -> [TVarWithKind] -> DeclHead ()
mkTypeHead qname vs = foldl (DHApp ()) (DHead () (convertTypeNameToHs (Unqual qname))) $ map toTyVarBndr vs

mkMonadicTypeHead :: QName -> [TVarWithKind] -> DeclHead ()
mkMonadicTypeHead qname vs = foldl (DHApp ()) (DHead () (convertTypeNameToMonadicHs (Unqual qname))) $ map toTyVarBndr vs

normalizeCurryType :: TypeExpr -> TypeExpr
normalizeCurryType (FuncType ty1 ty2) = case normalizeCurryType ty2 of
  ForallType vs' ty2' -> ForallType vs' (FuncType ty1' ty2')
  ty2' -> FuncType ty1' ty2'
  where
    ty1' = normalizeCurryType ty1
normalizeCurryType (ForallType vs ty) = case normalizeCurryType ty of
  ForallType vs' ty' -> ForallType (vs ++ vs') ty'
  ty' -> ForallType vs ty'
normalizeCurryType ty = ty

newtype RuleInfo = RI (QName, TRule)
type instance HsEquivalent RuleInfo = Match ()
instance ToHs RuleInfo where
  convertToHs (RI (qname, TRule args expr)) = do
    analysis <- ask
    e <- convertToHs (annotateND analysis expr)
    return $ Match () (convertFuncNameToHs (Unqual qname)) (map (PVar () . indexToName . fst) args)
      (UnGuardedRhs () e) Nothing
  convertToHs (RI (qname, TExternal _ str)) = return $
    Match () (convertFuncNameToHs (Unqual qname)) []
      (UnGuardedRhs () (Hs.Var () (UnQual () (Ident () (escapeFuncName unqualStr ++ "_Det#"))))) Nothing
    where unqualStr = case dropWhile (/= '.') str of
            ""  -> str
            "." -> str
            _:s -> s

instance ToMonadicHs RuleInfo where
  convertToMonadicHs (RI (qname, TRule args expr)) = do
    analysis <- ask
    freshNames <- replicateM (length args) freshVarName
    let annExpr = annotateND analysis expr
    e' <- convertToMonadicHs (annotateND analysis expr)
    let argInfo = zipWith (\(v, _) n -> (v, n, countVarUse annExpr v)) args freshNames
    let e'' = foldr (\(v, n, occ) -> mkReturnFunc .  Lambda () [PVar () n] . mkShareBind (indexToName v, Hs.Var () (UnQual () n), occ)) e' argInfo
    return $ Match () (convertFuncNameToMonadicHs (Unqual qname)) []
      (UnGuardedRhs () e'') Nothing
  convertToMonadicHs (RI (qname, TExternal _ str)) = return $
    Match () (convertFuncNameToMonadicHs (Unqual qname)) []
      (UnGuardedRhs () (Hs.Var () (UnQual () (Ident () (escapeFuncName unqualStr ++ "_ND#"))))) Nothing
    where unqualStr = case dropWhile (/= '.') str of
            ""  -> str
            "." -> str
            _:s -> s

type instance HsEquivalent (AExpr (TypeExpr, NDInfo)) = Exp ()
instance ToHs (AExpr (TypeExpr, NDInfo)) where
  convertToHs (AVar _ idx) = return $ Hs.Var () (UnQual () (indexToName idx))
  convertToHs (ALit _ lit) = return $ Hs.Lit () (convertLit lit)
  convertToHs (AComb _ ct (qname, _) args) = do
    args' <- mapM convertToHs args
    let convertNameToHs = case ct of
          ConsCall -> convertTypeNameToHs
          ConsPartCall _ -> convertTypeNameToHs
          FuncCall -> convertFuncNameToHs
          FuncPartCall _ -> convertFuncNameToHs
    return $ foldl (App ()) (Hs.Var () (convertNameToHs qname)) args'
  convertToHs (ALet _ bs e) = do
    e' <- convertToHs e
    bs' <- mapM (\((v, _), lclE) -> PatBind () (PVar () (indexToName v))
                     <$> (UnGuardedRhs () <$> convertToHs lclE)
                     <*> pure Nothing) bs
    return $ Let () (BDecls () bs') e'
  convertToHs AFree {} = throwError [message "Encountered a free variable in an expression inferred to be deterministic"]
  convertToHs AOr {} = throwError [message "Encountered an 'or' in an expression inferred to be deterministic"]
  convertToHs (ACase _ _ e bs) = do
    e' <- convertToHs e
    bs' <- mapM convertToHs bs
    return $ Hs.Case () e' (bs' ++ [failedBranch])
  convertToHs (ATyped _ e ty) = ExpTypeSig () <$> convertToHs e <*> convertToHs ty

failedBranch :: Alt ()
failedBranch = Alt () (PWildCard ())
  (UnGuardedRhs ()
    (Hs.Var () (Qual () (ModuleName () "Curry_Prelude") (Ident () "failed_Det")))) Nothing

instance ToMonadicHs (AExpr (TypeExpr, NDInfo)) where
  convertToMonadicHs = convertExprToMonadicHs Set.empty

convertExprToMonadicHs :: Set.Set Int -> AExpr (TypeExpr, NDInfo) -> CM (Exp ())
convertExprToMonadicHs vset (AVar _ idx) = if idx `elem` vset
  then return $ Hs.Var () (UnQual () (appendName "_nd" (indexToName idx)))
  else return $ Hs.Var () (UnQual () (indexToName idx))
convertExprToMonadicHs _ (ALit _ lit) = return $ mkReturn $ Hs.Lit () (convertLit lit)
convertExprToMonadicHs _ ex@(AComb (ty, Det) FuncCall _ _)
  | isFunFree ty = mkFromHaskell <$> convertToHs ex
convertExprToMonadicHs _ ex@(AComb (ty, Det) ConsCall  _ _)
  | isFunFree ty = mkFromHaskell <$> convertToHs ex
convertExprToMonadicHs vset (AComb _ ConsCall (qname, _) args) = do
  args' <- mapM (convertExprToMonadicHs vset) args
  return $ mkReturn $ foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname)) args'
convertExprToMonadicHs vset (AComb _ (ConsPartCall missing) (qname, _) args) = do
  args' <- mapM (convertExprToMonadicHs vset) args
  missingVs <- replicateM missing freshVarName
  let mkLam e = foldr (\v -> mkReturnFunc .  Lambda () [PVar () v]) e missingVs
  return $ mkLam $ mkReturn $ foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname)) (args' ++ map (Hs.Var () . UnQual ()) missingVs)
convertExprToMonadicHs vset (AComb _ _ (qname, _) args) = do -- (Partial) FuncCall
  args' <- mapM (convertExprToMonadicHs vset) args
  return $ foldl mkMonadicApp (Hs.Var () (convertFuncNameToMonadicHs qname)) args'
convertExprToMonadicHs _ ex@(ALet (ty, Det) _ _)
  | isFunFree ty = mkFromHaskell <$> convertToHs ex
convertExprToMonadicHs vset ex@(ALet _ bs e) = do
  e' <- convertExprToMonadicHs vset e
  bs' <- mapM (\((a, _), b) -> (indexToName a, , countVarUse ex a) <$> convertExprToMonadicHs vset b) bs
  return $ mkShareLet e' bs'
convertExprToMonadicHs vset (AFree _ vs e) = do
  e' <- convertExprToMonadicHs vset e
  return $ foldr mkShareBind e' (zip3 (map (indexToName . fst) vs) (repeat mkFree) (map (countVarUse e . fst) vs))
convertExprToMonadicHs vset (AOr _ e1 e2) = mkMplus <$> convertExprToMonadicHs vset e1 <*> convertExprToMonadicHs vset e2
convertExprToMonadicHs _ ex@(ACase (ty, Det) _ _ _)
  | isFunFree ty = mkFromHaskell <$> convertToHs ex
convertExprToMonadicHs vset (ACase (_, _) _  e bs)
  | (ty, Det) <- exprAnn e,
    isFunFree ty = do
      e' <- convertToHs e
      bs' <- mapM (convertDetBranchToMonadic vset) bs
      return $ Hs.Case () e' (bs' ++ [failedMonadicBranch])
  | otherwise = do
  e' <- convertExprToMonadicHs vset e
  bs' <- concat <$> mapM (convertBranchToMonadicHs vset) bs
  return $ mkBind e' $ Hs.LCase () (bs' ++ [failedMonadicBranch])
convertExprToMonadicHs _ ex@(ATyped (ty, Det) _ _)
  | isFunFree ty = mkFromHaskell <$> convertToHs ex
convertExprToMonadicHs vset (ATyped _ e ty) = ExpTypeSig () <$> convertExprToMonadicHs vset e <*> convertQualType ty

failedMonadicBranch :: Alt ()
failedMonadicBranch = Alt () (PWildCard ())
  (UnGuardedRhs ()
    (Hs.Var () (Qual () (ModuleName () "Curry_Prelude") (Ident () "failed_ND")))) Nothing

type instance HsEquivalent (ABranchExpr (TypeExpr, NDInfo)) = Alt ()
instance ToHs (ABranchExpr (TypeExpr, NDInfo)) where
  convertToHs (ABranch pat e) = do
    e' <- convertToHs e
    pat' <- convertToHs pat
    return $ Alt () pat' (UnGuardedRhs () e') Nothing

convertDetBranchToMonadic :: Set.Set Int -> ABranchExpr (TypeExpr, NDInfo) -> CM (Alt ())
convertDetBranchToMonadic vSet (ABranch pat e)
  | (ty, annot) <- exprAnn e,
    Det <- annot,
    isFunFree ty = do
      e' <- convertToHs e
      pat' <- convertToHs pat
      return (Alt () pat' (UnGuardedRhs () (mkFromHaskell e')) Nothing)
  | otherwise = do
      pat' <- convertToHs pat
      let vsNames = case pat of
                      APattern _ _ args -> map fst args
                      _                 -> []
      let vSet' = Set.union vSet (Set.fromList vsNames)
      e' <- convertExprToMonadicHs vSet' e
      return (Alt () pat' (UnGuardedRhs () (foldr mkFromHaskellBind e' vsNames)) Nothing)

convertBranchToMonadicHs :: Set.Set Int -> ABranchExpr (TypeExpr, NDInfo) -> CM [Alt ()]
convertBranchToMonadicHs vSet (ABranch pat e)
  | let (ty, annot) = exprAnn e,
    Det <- annot,
    isFunFree ty = do
      e' <- convertToHs e
      let pat' = case pat of
                   APattern _ (qname, _) [] ->
                    PApp () (convertTypeNameToMonadicHs qname) []
                   APattern _ (qname, (ty', _)) args ->
                    mkFlatPattern qname ty' (map fst args)
                   ALPattern _ lit ->
                    PLit () (litSign lit) (convertLit lit)
      return [Alt () pat' (UnGuardedRhs () (mkFromHaskell e')) Nothing]
  | otherwise = do
      alt1 <- do
        e' <- convertExprToMonadicHs vSet e
        (pat', vs) <- convertPatToMonadic pat e
        return $ Alt () pat' (UnGuardedRhs () (foldr mkShareBind e' vs)) Nothing
      case pat of
        APattern _ (qname@(_, baseName), (ty', _)) vs
          | not ("_Dict#" `isPrefixOf` baseName) -> do
          let vsNames = map fst vs
          let vSet' = Set.union vSet (Set.fromList vsNames)
          analysis <- ask
          let annE = annotateND' analysis (Map.fromSet (const Det) vSet') (genTypedExpr (fmap fst e))
          e' <- convertExprToMonadicHs vSet' annE
          let pat' = mkFlatPattern qname ty' vsNames
          return [alt1, Alt () pat' (UnGuardedRhs () (foldr mkFromHaskellBind e' vsNames)) Nothing]
        _ -> return [alt1]

mkFlatPattern :: QName -> TypeExpr -> [Int] -> Pat ()
mkFlatPattern qname ty args =
  PApp () (convertQualNameToFlatQualName (typeExprQualName ty)) $
  return $
  PApp () (convertTypeNameToHs qname) $
  map (PVar () . indexToName) args
  where
   typeExprQualName x = case x of
     TCons qname' _   -> qname'
     ForallType _ ty' -> typeExprQualName ty'
     _                -> error "mkFlatPattern: typeExprQualName"

type instance HsEquivalent (APattern (TypeExpr, NDInfo)) = Pat ()
instance ToHs (APattern (TypeExpr, NDInfo)) where
  convertToHs (APattern _ (qname, _) args) = return $ PApp () (convertTypeNameToHs qname) (map (PVar () . indexToName . fst) args)
  convertToHs (ALPattern _ lit) = return $ PLit () (litSign lit) (convertLit lit)
  -- TODO: remove sign?

convertPatToMonadic :: APattern (TypeExpr, NDInfo) -> AExpr (TypeExpr, NDInfo) -> CM (Pat (), [(Hs.Name (), Exp (), VarUse)])
convertPatToMonadic p@(ALPattern _ _) _ = (,[]) <$> convertToHs p
convertPatToMonadic (APattern _ (qname, _) args) e = do
  vs <- replicateM (length args) freshVarName
  return ( PApp () (convertTypeNameToMonadicHs qname) (map (PVar ()) vs)
         , zip3 (map (indexToName . fst) args) (map (Hs.Var () . UnQual ()) vs) (map (countVarUse e . fst) args))

litSign :: Literal -> Sign ()
litSign (Intc i)
  | i < 0 = Negative ()
litSign (Floatc f)
  | f < 0 = Negative ()
litSign _ = Signless ()

convertLit :: Literal -> Hs.Literal ()
convertLit (Intc i) = Hs.Int () i (show i)
convertLit (Floatc d) = Hs.Frac () (toRational d) (show d)
convertLit (Charc c) = Hs.Char () c (show c)

class ToHsName a where
  convertTypeNameToHs :: a -> HsEquivalent a
  convertFuncNameToHs :: a -> HsEquivalent a

class ToMonadicHsName a where
  convertTypeNameToMonadicHs :: a -> HsEquivalent a
  convertFuncNameToMonadicHs :: a -> HsEquivalent a

instance ToHsName UnqualName where
  convertTypeNameToHs (Unqual (_, s)) = Ident () $ escapeTypeName s ++ "_Det"
  convertFuncNameToHs (Unqual (_, s)) = Ident () $ escapeFuncName s ++ "_Det"

instance ToMonadicHsName UnqualName where
  convertTypeNameToMonadicHs (Unqual (_, s)) = Ident () $ escapeTypeName s ++ "_ND"
  convertFuncNameToMonadicHs :: UnqualName -> HsEquivalent UnqualName
  convertFuncNameToMonadicHs (Unqual (_, s)) = Ident () $ escapeFuncName s ++ "_ND"

instance ToHsName QName where
  convertTypeNameToHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertTypeNameToHs (Unqual n))
  convertFuncNameToHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertFuncNameToHs (Unqual n))

instance ToMonadicHsName QName where
  convertTypeNameToMonadicHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertTypeNameToMonadicHs (Unqual n))
  convertFuncNameToMonadicHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertFuncNameToMonadicHs (Unqual n))

convertQualNameToFlatName :: QName -> Name ()
convertQualNameToFlatName (_, s) =
  Ident () (escapeTypeName s ++ "Flat#")

convertQualNameToFlatQualName :: QName -> Hs.QName ()
convertQualNameToFlatQualName qname@(m, _) =
  Qual () (ModuleName () ("Curry_" ++ m)) $ convertQualNameToFlatName qname

defaultPragmas :: [ModulePragma ()]
defaultPragmas =
  [ LanguagePragma () [Ident () "NoImplicitPrelude"]
  , LanguagePragma () [Ident () "KindSignatures"]
  , LanguagePragma () [Ident () "TypeOperators"]
  , LanguagePragma () [Ident () "TypeFamilies"]
  , LanguagePragma () [Ident () "ExplicitForAll"]
  , LanguagePragma () [Ident () "ImpredicativeTypes"]
  , LanguagePragma () [Ident () "QuantifiedConstraints"]
  , LanguagePragma () [Ident () "LambdaCase"]
  , LanguagePragma () [Ident () "MagicHash"]
  , LanguagePragma () [Ident () "MultiParamTypeClasses"]
  , LanguagePragma () [Ident () "UnboxedTuples"]
  , LanguagePragma () [Ident () "UndecidableInstances"]
  , OptionsPragma () (Just GHC) "-w " -- Space is important here
  ]

defaultImports :: [ImportDecl ()]
defaultImports =
  [ ImportDecl () (ModuleName () "Data.Kind") True False False Nothing Nothing Nothing
  , ImportDecl () (ModuleName () "BasicDefinitions") True False False Nothing Nothing Nothing
  , ImportDecl () (ModuleName () "Control.Monad") True False False Nothing (Just (ModuleName () "M")) Nothing
  , ImportDecl () (ModuleName () "Control.Monad.Fix") True False False Nothing (Just (ModuleName () "M")) Nothing
  , ImportDecl () (ModuleName () "Prelude") True False False Nothing (Just (ModuleName () "P")) Nothing
  ]

-- |Compute the filename of the Haskell file for a source file
haskellName :: FilePath -> FilePath
haskellName = flip updateBaseName ("Curry_"++) . flip replaceExtension haskellExt

updateBaseName :: FilePath -> (String -> String) -> FilePath
updateBaseName p f = replaceBaseName p (f (takeBaseName p))

-- |Compute the filename of the external definition file for a source file
externalName :: FilePath -> FilePath
externalName = flip replaceExtension externalExt

-- |Filename extension for Haskell files
haskellExt :: String
haskellExt = ".hs"

-- |Filename extension for external definition files
externalExt :: String
externalExt = ".ext.hs"

escapeFuncName :: String -> String
escapeFuncName s = case escapeName s of
  "" -> ""
  c:cs -> toLower c : cs

escapeTypeName :: String -> String
escapeTypeName s = case escapeName s of
  "" -> ""
  c:cs -> toUpper c : cs

escapeName :: String -> String
escapeName "[]" = "CList"
escapeName "(->)" = "CArrow"
escapeName "()" = "CUnit"
escapeName ":" = "CCons"
escapeName s
  | "_Dict" `isPrefixOf` s = concatMap replaceChar (drop 1 s)
  | Just arity <- tupleStringArity s = "CTuple" ++ show arity
  | otherwise = concatMap replaceChar s
  where
    replaceChar c | Just r <- lookup c opRenaming = r
                  | otherwise = return c

opRenaming :: [(Char, String)]
opRenaming =
  [ ('_' , "uscore"   )
  , ('~' , "tilde"    )
  , ('!' , "bang"     )
  , ('@' , "at"       )
  , ('#' , "hash"     )
  , ('$' , "dollar"   )
  , ('%' , "percent"  )
  , ('^' , "caret"    )
  , ('&' , "amp"      )
  , ('*' , "star"     )
  , ('+' , "plus"     )
  , ('-' , "minus"    )
  , ('=' , "eq"       )
  , ('<' , "lt"       )
  , ('>' , "gt"       )
  , ('?' , "qmark"    )
  , ('.' , "dot"      )
  , ('/' , "slash"    )
  , ('|' , "bar"      )
  , ('\\', "backslash")
  , (':' , "colon"    )
  , ('(' , "lparen"   )
  , (')' , "rparen"   )
  , ('[' , "lbracket" )
  , (']' , "rbracket" )
  , (',' , "comma"    )
  , ('\'', "tick"     )
  ]

parseFile' :: FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFile' fp = parseFileWithMode' (defaultParseMode { parseFilename = fp }) fp

-- | Parse a source file on disk, supplying a custom parse mode.
parseFileWithMode' :: ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFileWithMode' p fp = parseFileContentsWithMode p <$> readUTF8File' fp

readUTF8File' :: FilePath -> IO String
readUTF8File' fp = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  hGetContents' h

writeUTF8File' :: FilePath -> String -> IO ()
writeUTF8File' fp s = do
  h <- openFile fp WriteMode
  hSetEncoding h utf8
  hPutStr h s
  hClose h

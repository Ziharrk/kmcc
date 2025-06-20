{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans        #-}
module Curry.ConvertToHs
  ( compileToHs, haskellName
  , mkCurryCtxt, mkQuantifiedCtxt, mkQuantifiedFor, mkFlatPattern
  , HsEquivalent
  , ToHsName(..), ToMonadicHsName(..), UnqualName(..)
  , convertQualNameToFlatName, convertQualNameToFlatQualName
  , convertTypeToMonadicHs
  ) where

import Control.Arrow (first, second)
import Control.Monad (void, replicateM, when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader (..), runReaderT, asks)
import Control.Monad.State (StateT, MonadState (..), evalStateT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Coerce (coerce)
import Data.Char (toLower, toUpper)
import Data.Generics.Schemes (everything)
import Data.Generics.Aliases (mkQ)
import Data.List (isPrefixOf, find, (\\), intercalate, unfoldr, partition)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts hiding (Literal, Cons, Kind, QName)
import qualified Language.Haskell.Exts as Hs
import System.Directory (doesFileExist, getModificationTime)
import System.FilePath (replaceExtension, replaceFileName, takeFileName, (<.>), splitExtension, takeExtension)
import System.IO (openFile, IOMode (..), utf8, hSetEncoding, hPutStr, hClose, hGetContents')

import Curry.Frontend.Base.Messages (status, abortWithMessages, message)
import Curry.Base.Message (Message(..))
import Curry.Base.Ident (ModuleIdent)
import Curry.Base.Pretty (text)
import Curry.Base.SpanInfo (SpanInfo(..))
import Curry.FlatCurry hiding (Let)
import Curry.FlatCurry.Annotated.Type (APattern(..), ABranchExpr(..), AExpr(..))
import Curry.FlatCurry.Typed.Type (TRule(..), TFuncDecl(..), TProg(..), TExpr (..))
import Curry.Files.Filenames (addOutDirModule)
import Curry.Frontend.Generators.GenTypedFlatCurry (genTypedExpr)
import Curry.Frontend.CompilerOpts (Options(..))
import Curry.Frontend.CurryBuilder (compMessage)

import Options (KMCCOpts(..), dumpMessage)
import Curry.Analysis (NDAnalysisResult, NDInfo (..))
import Curry.Annotate (annotateND, exprAnn, annotateND')
import Curry.CompileToFlat (externalName)
import Curry.ConvertUtils
import Curry.Default (defaultAmbiguousDecl, anyQName)
import Curry.GenInstances (genInstances)

data CMState = CMState
  { freshVarIdx :: Integer
  , caseDepth   :: Integer
  }

data CMRead = CMRead
  { newtypeNames     :: Set QName
  , dataWithFunNames :: Set QName
  , ndAnalysisResult :: NDAnalysisResult
  }

newtype CM a = CM {
    runCM :: ReaderT CMRead (ExceptT [Message] (StateT CMState IO)) a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO
                     , MonadReader CMRead
                     , MonadError [Message]
                     , MonadState CMState
                     )

evalCM :: CM a -> NDAnalysisResult -> Set QName -> Set QName -> IO a
evalCM a analysis types funTypes = do
  eith <- evalStateT (runExceptT (runReaderT (runCM a) (CMRead types funTypes analysis)))
            (CMState 0 0)
  case eith of
    Left msgs -> abortWithMessages msgs
    Right res -> return res

freshVarName :: CM (Hs.Name ())
freshVarName = do
  CMState { freshVarIdx = i, caseDepth = j } <- get
  put (CMState (succ i) j)
  return (Ident () $  "x_" ++ show i)

resetCaseDepth :: CM a -> CM a
resetCaseDepth act = do
  CMState { freshVarIdx = i, caseDepth = j } <- get
  put (CMState i 0)
  act <* put (CMState i j)

increaseCaseDepth :: CM a -> CM a
increaseCaseDepth act = do
  CMState { freshVarIdx = i, caseDepth = j } <- get
  put (CMState i (succ j))
  act <* put (CMState i j)

maxCaseDepth :: Integer
maxCaseDepth = 5

compileToHs :: Maybe TypeExpr -> [((TProg, Bool), ModuleIdent, FilePath)]
            -> NDAnalysisResult -> Set QName -> Set QName -> KMCCOpts
            -> IO ()
compileToHs mainType mdls ndInfo tyEnv funTypes opts =
  evalCM (mapM_ process' (zip [1 ..] (addMainInfo mdls))) ndInfo tyEnv funTypes
  where
    addMainInfo [] = []
    addMainInfo [(x, y, z)] = [(x, y, z, mainType)]
    addMainInfo ((x, y, z):xs) = (x, y, z, Nothing) : addMainInfo xs
    total = length mdls
    process' (n, ((prog, comp), m, fp, mi)) = process opts (n, total) prog comp m fp mi

process :: KMCCOpts -> (Int, Int) -> TProg -> Bool
        -> ModuleIdent -> FilePath -> Maybe TypeExpr -> CM ()
process kopts idx@(thisIdx,maxIdx) tprog comp m fn mi
  | optForce opts ||
    comp      = compile
  | otherwise = do
    existsA <- liftIO (doesFileExist destFile)
    existsB <- liftIO (doesFileExist externalFile)
    if existsA && existsB
      then do
        t1 <- liftIO $ getModificationTime destFile
        t2 <- liftIO $ getModificationTime externalFile
        if t1 > t2
          then skip
          else compile
      else if existsA
        then skip
        else compile
  where
    destFile = tgtDir (haskellName fn)
    externalFile = externalName fn
    skip = do
      status opts $ compMessage idx (11, 16) "Skipping" m (fn, destFile)
      when (optCompilerVerbosity kopts > 2) $ do
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
      res <- convertToHs (TWFP (tprog, externalFile, mi, kopts))
      let printed = prettyPrintStyleMode hsPrettyPrintStyle hsPrettyPrintMode res
      liftIO $ writeUTF8File' (tgtDir (haskellName fn)) printed
      if thisIdx == maxIdx
        then liftIO $ dumpMessage kopts ("Generated Haskell file:\n" ++ printed)
        else liftIO $ dumpMessage kopts "Generated Haskell file."
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
        let defaultedFs = map defaultAmbiguousDecl fs
        funds <- mapM convertToHs defaultedFs
        tydsM <- mapM convertToMonadicHs tys
        fs' <- case mi of
          Just ty -> patchMainPre ty opts defaultedFs
          Nothing -> return defaultedFs
        fundsM <- mapM convertToMonadicHs fs'
        let visT = mapMaybe getVisT tys
        let visF = mapMaybe getVisF defaultedFs
        header <- convertToHs (MS (nm, visT, visF))
        let extract (Just (x, y)) = [x,y]
            extract Nothing = []
        let insts = concatMap genInstances tys
        let ds = insts ++ coerce (tyds ++ tydsM) ++ concatMap @[] extract (coerce (funds ++ fundsM))
        return (header, im', ds)
    (extPragmas, extImports, extDs, extExp) <- liftIO $ doesFileExist fp >>= \case
      True -> do
        ext <- liftIO $ parseFile' fp
        case fmap void ext of
          ParseOk (Module _ mh p i d) -> return (p, i, d, e)
            where e = case mh of
                        Just (ModuleHead _ _ _ (Just (ExportSpecList _ ex))) -> ex
                        _ -> []
          ParseOk _           -> return ([] , [], [], [])
          ParseFailed loc err -> do
            liftIO $ fail $ unlines
              [ "External definitions file is corrupted."
              , "For the file \"" ++ fp ++ "\"."
              , "Parsing failed with:"
              , err
              , "At location:"
              , prettyPrint loc
              , "Aborting compilation ..." ]
      False -> return ([], [], [], [])
    let ps' = defaultPragmas ++ extPragmas
    let qualNameImports = map (\n -> ImportDecl () (ModuleName () n) True False False Nothing Nothing Nothing)
          $ Set.toList $ Set.delete (convertModName nm) -- do not import self
          $ Set.delete "M" $ Set.delete "B" $ Set.delete "P" -- do not import these module aliases
          $ everything Set.union (mkQ Set.empty (\(ModuleName () n) -> Set.singleton n)) curryDs
    let im' = defaultImports ++ extImports ++ curryImports ++ qualNameImports
    let ds' = extDs ++ curryDs
    (header', ds'') <- case mi of
          Just ty -> patchMainPost ty opts header ds'
          Nothing -> return (header, ds')
    return (Module () (Just (combineHeaderExport header' extExp)) ps' im' ds'')
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

      combineHeaderExport (ModuleHead a b c (Just (ExportSpecList d ex))) ex' =
        ModuleHead a b c (Just (ExportSpecList d (ex ++ ex')))
      combineHeaderExport h _ = h


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
                | otherwise         = throwError [Message
                  NoSpanInfo
                  (text $
                    "Variable specified on the command line with name and index " ++
                    show (n, i) ++
                    " is not free in the expression.")
                  []]
          specifiedVs <- check (optVarNames opts)
          let ty' = foldr (FuncType . snd) fty specifiedVs
          return (TFunc qname (length specifiedVs) vis ty' (TRule specifiedVs (TFree (vs \\ specifiedVs) e')))
      patchMainDecl f = return f

-- Now we need to
-- generate the replacement for the function that was extended with arguments (gets the mainND name)
-- update the name of the function that was extended with arguments to mainND##
-- call the wrapper on the mainND## function, or if main is actually deterministic or IO, call the respective wrapper
patchMainPost :: TypeExpr -> KMCCOpts -> ModuleHead () -> [Decl ()] -> CM (ModuleHead (), [Decl ()])
patchMainPost ty opts (ModuleHead _ nm w (Just (ExportSpecList _ es))) ds = do
  let hasDetMain = EVar () (Qual () nm (Ident () "main_Det")) `elem` es
  let mainExport = EVar () (Qual () nm (Ident () "main##"))

  (mainExpr, ds') <- case ty of
      TCons ("Prelude","IO") [aty]
        | hasDetMain -> do
          resTy <- convertToMonadicHs aty
          return (App () (App () (Hs.Var () mainWrapperDetQualName) (TypeApp () (TyParen () resTy)))
                    (Hs.Var () (Qual () nm (Ident () "main_Det"))), ds)
        | otherwise  ->  do
          resTy <- convertToMonadicHs aty
          return (App () (App () (Hs.Var () mainWrapperNDetQualName) (TypeApp () (TyParen () resTy)))
                    (Hs.Var () (Qual () nm (Ident () "main_ND"))), ds)
      _
        | hasDetMain -> do
          resTy <- convertToMonadicHs ty
          return (App () (App () (App () (Hs.Var () exprWrapperDetQualName) (TypeApp () (TyParen () resTy)))
                    (Hs.Var () (searchStratQualName (optSearchStrategy opts))) )
                    (Hs.Var () (Qual () nm (Ident () "main_Det"))), ds)
        | otherwise  -> do
          let findMainDecl [] = throwError [Message
                NoSpanInfo
                (text "Main function not found")
                []]
              findMainDecl ((FunBind _ [Match _ (Ident () "main_ND") [] (UnGuardedRhs () e) Nothing]):bs) = return (e, bs)
              findMainDecl (b:bs) = second (b:) <$> findMainDecl bs
              findMainSig [] = throwError [Message NoSpanInfo (text "Main type signature not found") []]
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
          let mainNDExpr = foldr (mkLetBind . (,mkFree)) e' mainVs
          let mainNDDecl = FunBind () [Match () (Ident () "main_ND") [] (UnGuardedRhs () mainNDExpr) Nothing]

          let mainE = mkVarReturn mainVs (Hs.Var () (Qual () nm (Ident () "mainND##")))
          let varInfos = List () $ map (\(s,i) -> Tuple () Boxed [Hs.Lit () (String () s s), Hs.Lit () (Int () (toInteger i) (show i))]) (optVarNames opts)
          let bindingOpt = Hs.Var () $ if optShowBindings opts then trueQualName else falseQualName
          let searchOpt = Hs.Var () (searchStratQualName (optSearchStrategy opts))
          let intOpt = Hs.Var () $ if optInteractive opts then trueQualName else falseQualName
          return (App () (App () (App () ( App () ( App () (Hs.Var () exprWrapperNDetQualName) searchOpt ) intOpt ) varInfos) bindingOpt) mainE, mainNDDecl:mainNDHashDecl:mainNDHashType:rest)

  let mainDecl = PatBind () (PVar () (Ident () "main##")) (UnGuardedRhs () mainExpr) Nothing
  return (ModuleHead () nm w (Just (ExportSpecList () (mainExport:es))), mainDecl:ds')
patchMainPost _ _ h ds = return (h, ds)

getLiftedPats :: Exp () -> [Pat ()]
getLiftedPats (Hs.App _ _ (Lambda _ [p] e)) = p : getLiftedPats e
getLiftedPats (Hs.InfixApp _ _ bind (Lambda _ _ e))
  | isBind bind = getLiftedPats e
  where isBind (Hs.QConOp _ v) = v == bindQualName
        isBind (Hs.QVarOp _ v) = v == bindQualName
getLiftedPats (Hs.Let _ _ e) = getLiftedPats e
getLiftedPats _ = []

splitFreeVars :: TExpr -> ([(VarIndex, TypeExpr)], TExpr)
splitFreeVars (TFree vs e) = first (vs++) (splitFreeVars e)
splitFreeVars e = ([], e)

mkVarReturn :: [Name ()] -> Exp () -> Exp ()
mkVarReturn fvs = go
  where
    go e =
      let mainE = foldl (\e' -> mkMonadicApp e' . Hs.Var () . UnQual ()) e fvs
          eWithInfo = mkAddVarIds mainE (map (mkGetVarId . Hs.Var () . UnQual ()) fvs)
      in foldr (mkShareBind . (,mkFree)) eWithInfo fvs

newtype ModuleHeadStuff = MS (String, [(QName, [QName])], [QName])
type instance HsEquivalent ModuleHeadStuff = ModuleHead ()
instance ToHs ModuleHeadStuff where
  convertToHs (MS (s, qnamesT, qnamesF)) = do
    tyEnv <- asks newtypeNames
    let tyEx = concatMap (typeItem tyEnv) qnamesT
    fnEx <- concat <$> mapM funcItem qnamesF
    return $ ModuleHead () (ModuleName () (convertModName s)) Nothing $
      Just (ExportSpecList () (tyEx ++ fnEx))
    where
      typeItem _        (qname, [])      = map (EAbs () (NoNamespace ()))
        [convertTypeNameToHs qname, convertTypeNameToMonadicHs qname]
      typeItem newtypes (qname, csNames) = map (uncurry (EThingWith () (NoWildcard ())))
        [ (convertTypeNameToHs qname, concatMap conItem csNames)
        , (convertTypeNameToMonadicHs qname,
            (if all (`Set.member` newtypes) csNames
              then id
              else (ConName () (convertQualNameToFlatName qname) : ))
            (map conItemMonadic csNames)) ]

      conItem qname = [ConName () (convertTypeNameToHs (Unqual qname))]
      conItemMonadic qname = ConName () (convertTypeNameToMonadicHs (Unqual qname))

      funcItem qname = do
        analysis <- asks ndAnalysisResult
        return (EVar () (convertFuncNameToMonadicHs qname) : case Map.lookup qname analysis of
          Just Det -> [EVar () $ convertFuncNameToHs qname]
          _        -> [])

newtype ImportString = IS String
type instance HsEquivalent ImportString = ImportDecl ()
instance ToHs ImportString where
  convertToHs (IS s) = return $ ImportDecl () (ModuleName () (convertModName s)) False False False Nothing Nothing Nothing

newtype HsTypeDecl = HTD (Decl ())
type instance HsEquivalent TypeDecl = HsTypeDecl
instance ToHs TypeDecl where
  convertToHs (Type qname@(mdl, nm) _ vs cs)
    | [] <- cs = return $ HTD $ TypeDecl () (mkTypeHead qname []) $
      TyCon () (Qual () (ModuleName () (convertModName mdl)) (Ident () (escapeTypeName nm ++ "_Det#")))
    | otherwise = do
      cs' <- mapM convertToHs cs
      return $ HTD $
        DataDecl () (DataType ()) Nothing (mkTypeHead qname vs) cs' []
  convertToHs (TypeSyn qname _ vs texpr) = do
    ty <- convertToHs texpr
    return $ HTD $
      TypeDecl () (mkTypeHead qname vs) ty
  convertToHs (TypeNew qname _ vs (NewCons qname2 vis texpr)) = do
    c' <- convertToHs (Cons qname2 1 vis [texpr])
    return $ HTD $
      DataDecl () (NewType ()) Nothing (mkTypeHead qname vs) [c'] []

instance ToMonadicHs TypeDecl where
  convertToMonadicHs (Type qname@(mdl, nm) _ vs cs)
    | [] <- cs = return $ HTD $ TypeDecl () (mkMonadicTypeHead qname []) $
      TyCon () (Qual () (ModuleName () (convertModName mdl)) (Ident () (escapeTypeName nm ++ "_ND#")))
    | otherwise = do
      cs' <- (mkFlatConstr qname vs :) <$> mapM convertToMonadicHs cs
      return $ HTD $
        DataDecl () (DataType ()) Nothing (mkMonadicTypeHead qname vs) cs' []
  convertToMonadicHs (TypeSyn qname _ vs texpr) =  do
    ty <- convertToMonadicHs texpr
    return $ HTD $
      TypeDecl () (mkMonadicTypeHead qname vs) ty
  convertToMonadicHs (TypeNew qname _ vs c) = do
    NC c' <- convertToMonadicHs c
    return $ HTD $
      DataDecl () (NewType ()) Nothing (mkMonadicTypeHead qname vs) [c'] []

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
    let tys = map convertQualType texprs
    return $ QualConDecl () Nothing Nothing (ConDecl () (convertTypeNameToMonadicHs (Unqual cname)) tys)

newtype NC = NC (QualConDecl ())
type instance HsEquivalent NewConsDecl = NC
instance ToMonadicHs NewConsDecl where
  convertToMonadicHs (NewCons cname _ texpr) = do
    let tys = [convertTypeToMonadicHs texpr]
    return $ NC $ QualConDecl () Nothing Nothing (ConDecl () (convertTypeNameToMonadicHs (Unqual cname)) tys)

type instance HsEquivalent TypeExpr = Type ()
instance ToHs TypeExpr where
  convertToHs (TVar idx) = return $ TyVar () (indexToName idx)
  convertToHs (FuncType t1 t2) = TyFun () <$> convertToHs t1 <*> convertToHs t2
  convertToHs (TCons qn tys) = foldl (TyApp ()) (TyCon () (convertTypeNameToHs qn)) <$> mapM convertToHs tys
  convertToHs (ForallType vs t) = TyForall () (Just $ map mkOther vs ++ map toTyVarBndr vs)
                                    (Just $ CxTuple () $ concatMap mkCtxt vs)
                                    <$> convertToHs t
    where mkOther (i, k) = KindedVar () (appendName "c" (indexToName i)) (kindToHsType k)
          mkCtxt (i, k) = [ TypeA () $ TyEquals () (TyApp () (TyCon () hsEquivQualName) n') (TyVar () n)
                          , mkQuantifiedForWithNaming mkCurryClassType (appendName "c" . indexToName) (i, k)]
            where n = indexToName i
                  n' = TyVar () $ appendName "c" n

instance ToMonadicHs TypeExpr where
  convertToMonadicHs = return . convertTypeToMonadicHs

convertTypeToMonadicHs :: TypeExpr -> Type ()
convertTypeToMonadicHs (TVar idx) = TyVar () (indexToName idx)
convertTypeToMonadicHs (FuncType t1 t2) = mkLiftedFunc (convertTypeToMonadicHs t1) (convertTypeToMonadicHs t2)
convertTypeToMonadicHs (TCons qn tys) = foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qn)) $ map convertTypeToMonadicHs tys
convertTypeToMonadicHs (ForallType vs t) = TyForall () (Just (map toTyVarBndr vs)) (mkCurryCtxt vs) (convertQualType t)

convertQualType :: TypeExpr -> Type ()
convertQualType ty
  | ForallType _ _ <- ty = ty'
  | otherwise            = mkCurry ty'
  where
    ty' = convertTypeToMonadicHs ty

mkCurryCtxt :: [TVarWithKind] -> Maybe (Context ())
mkCurryCtxt = mkQuantifiedCtxt mkCurryClassType

mkQuantifiedCtxt :: (Type () -> Type ()) -> [TVarWithKind] -> Maybe (Context ())
mkQuantifiedCtxt _      [] = Nothing
mkQuantifiedCtxt mkType vs = Just $ CxTuple () $ map (mkQuantifiedFor mkType) vs

mkQuantifiedFor :: (Type () -> Type ()) -> TVarWithKind -> Asst ()
mkQuantifiedFor mtype = mkQuantifiedForWithNaming mtype indexToName

mkQuantifiedForWithNaming :: (Type () -> Type ()) -> (Int -> Name ()) -> TVarWithKind -> Asst ()
mkQuantifiedForWithNaming mkType naming (i, KStar) = TypeA () $ mkType (TyVar () (naming i))
mkQuantifiedForWithNaming mkType naming (i, k) = construct $ map (mkQuantifiedForWithNaming mkType naming) argsWithTVar
  where
    (args, _) = splitKindArrow k
    argTVars = take (length args) [i+1..]
    argsWithTVar = zip argTVars args
    construct args' = TypeA () $ TyForall ()
      (Just (map (\(i', k') -> KindedVar () (naming i') (kindToHsType k')) argsWithTVar))
      (Just (CxTuple () args'))
      (mkType (foldl (TyApp ()) (TyVar () (naming i)) (map (TyVar () . naming) argTVars)))

newtype HsFuncDecl = HFD (Maybe (Decl (), Decl ()))
type instance HsEquivalent TFuncDecl = HsFuncDecl
instance ToHs TFuncDecl where
  convertToHs (TFunc qname _ _ texpr rule) = do
    analysis <- asks ndAnalysisResult
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
    let ty = convertQualType (normalizeCurryType texpr)
    let tsig = TypeSig () [convertFuncNameToMonadicHs (Unqual qname)] ty
    match <- resetCaseDepth $ convertToMonadicHs (RI (qname, rule))
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
    analysis <- asks ndAnalysisResult
    dataEnv <- asks dataWithFunNames
    e <- convertToHs (annotateND analysis dataEnv expr)
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
    analysis <- asks ndAnalysisResult
    dataEnv <- asks dataWithFunNames
    e' <- convertToMonadicHs (annotateND analysis dataEnv expr)
    let argInfo = map fst args
    let e'' = foldr (\v -> mkReturnFunc .  Lambda () [PVar () (indexToName v)]) e' argInfo
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
  convertToHs (ALit _ lit) = return $ convertLit (Paren ()) (Hs.Lit ()) lit
  convertToHs (AComb _ FuncCall (("Prelude", "apply"), _) [arg1, arg2]) =
    App () <$> convertToHs arg1 <*> convertToHs arg2
  convertToHs (AComb _ ct (qname, _) args) = do
    tyEnv <- asks newtypeNames
    case (Set.member qname tyEnv, args) of
      (True, [arg]) -> Hs.App () (Hs.Var () (convertTypeNameToHs qname)) . Paren ()
                        <$> convertToHs arg
      (True, []) -> return $ Hs.Var () (convertTypeNameToHs qname)
      _ -> do
        args' <- mapM convertToHs args
        let convertNameToHs = case ct of
              ConsCall -> convertTypeNameToHs
              ConsPartCall _ -> convertTypeNameToHs
              FuncCall -> convertFuncNameToHs
              FuncPartCall _ -> convertFuncNameToHs
        return $ foldl (App ()) (Hs.Var () (convertNameToHs qname)) args'
  convertToHs (ALet _ bs e) = do
    let mkP v e' ty' = PatBind () (PVar () (indexToName v))
                        (UnGuardedRhs () (ExpTypeSig () (Paren () e') (TyParen () ty'))) Nothing
    e' <- convertToHs e
    bs' <- mapM (\((v, (ty, _)), lclE) ->
                    mkP v
                    <$> convertToHs lclE
                    <*> convertToHs ty) bs
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
    (Hs.Var () (Qual () (ModuleName () "B") (Ident () "failed")))) Nothing

instance ToMonadicHs (AExpr (TypeExpr, NDInfo)) where
  convertToMonadicHs = convertExprToMonadicHs Set.empty

applyToArgs :: (Exp () -> Exp () -> Exp ())
            -> (Exp () -> Exp ()) -> Exp ()
            -> [(Maybe (AExpr (TypeExpr, NDInfo)), Exp ())]
            -> CM (Exp ())
applyToArgs apply ret funE args = do
  vs <- replicateM (length args) freshVarName
  let combineForSharing [] = ([], [])
      combineForSharing ((_, (_, e)):xs) | isKnownShareless ||
                                           isTransparent e
                                          = (e:es, infos)
        where (es, infos) = combineForSharing xs
      combineForSharing ((v, (_, e)):xs) = (Hs.Var () (UnQual () v):es, (v, e):infos)
        where (es, infos) = combineForSharing xs
  let (exprs, shares) = combineForSharing $ zip vs args
  let applic = ret $ foldl apply funE exprs
  return $ foldr mkShareBind applic shares
  where
    isKnownShareless = case funE of
      Hs.Var () (Qual () (ModuleName () "Curry_Prelude") (Ident () "apply_ND")) -> True
      Hs.Var () (Qual () (ModuleName () "Curry_Prelude") (Ident () "qmark_ND")) -> True
      _ -> False
    isTransparent (Hs.Var _ (UnQual _ _)) = True
    isTransparent (Hs.Lit _ _) = True
    isTransparent (Hs.App () (Hs.Var () (Qual () (ModuleName () "B") (Ident () "fromHaskell"))) _) = True
    isTransparent (Hs.App () (Hs.Var () (Qual () (ModuleName () "M") (Ident () "return"))) _) = True
    isTransparent _ = False

convertExprToMonadicHs :: Set.Set Int -> AExpr (TypeExpr, NDInfo) -> CM (Exp ())
convertExprToMonadicHs vset (AVar _ idx) = if idx `elem` vset
  then return $ Hs.Var () (UnQual () (appendName "_nd" (indexToName idx)))
  else return $ Hs.Var () (UnQual () (indexToName idx))
convertExprToMonadicHs _ (ALit _ lit) = return $ mkReturn $ convertLit (Paren ()) (Hs.Lit ()) lit
convertExprToMonadicHs _ ex@(AComb (ty, Det) FuncCall _ _)  =
  mkFromHaskellTyped <$> convertToHs ex <*> convertToMonadicHs ty
convertExprToMonadicHs _ ex@(AComb (ty, Det) ConsCall  _ _) =
  mkFromHaskellTyped <$> convertToHs ex <*> convertToMonadicHs ty
convertExprToMonadicHs vset (AComb (_, _) _ (("Prelude", "apply"), _) [f, a]) = do
  args' <- mapM (convertExprToMonadicHs vset) [f, a]
  case args' of
    [f', a'] -> applyToArgs mkMonadicApp id f' [(Just a, a')]
    _ -> throwError [message "Internal error: Prelude.apply called with wrong number of arguments"]
convertExprToMonadicHs vset (AComb _ ConsCall (qname, _) args) = do
  tyEnv <- asks newtypeNames
  case (Set.member qname tyEnv, args) of
    (True, [arg]) -> Hs.App () (mkFmap (Hs.Var () (convertTypeNameToMonadicHs qname))) . Paren ()
                      <$> convertExprToMonadicHs vset arg
    _ -> do
      args' <- mapM (\a -> (Just a,) <$> convertExprToMonadicHs vset a) args
      applyToArgs (App ()) mkReturn (Hs.Var () (convertTypeNameToMonadicHs qname)) args'
convertExprToMonadicHs vset (AComb _ (ConsPartCall missing) (qname, _) args) = do
  tyEnv <- asks newtypeNames
  missingVs <- replicateM missing freshVarName
  case (Set.member qname tyEnv, args, missingVs) of
    (True, [], [v]) -> return $ Paren () $ mkFmapPartial $
      Lambda () [PVar () v] $
      Hs.App () (Hs.Var () (convertTypeNameToMonadicHs qname)) $ Paren () $
      Hs.Var () (UnQual () v)
    _ -> do
      args' <- mapM (\a -> (Just a,) <$> convertExprToMonadicHs vset a) args
      let mkLam e = foldr (\v -> mkReturnFunc .  Lambda () [PVar () v]) e missingVs
      mkLam <$> applyToArgs (App ()) mkReturn (Hs.Var () (convertTypeNameToMonadicHs qname)) (args' ++ map ((Nothing,) . Hs.Var () . UnQual ()) missingVs)
convertExprToMonadicHs vset (AComb _ _ (qname, _) args) = do -- (Partial) FuncCall
  args' <- mapM (\a -> (Just a,) <$> convertExprToMonadicHs vset a) args
  applyToArgs mkMonadicApp id (Hs.Var () (convertFuncNameToMonadicHs qname)) args'
convertExprToMonadicHs _ ex@(ALet (ty, Det) _ _) =
  mkFromHaskellTyped <$> convertToHs ex <*> convertToMonadicHs ty
convertExprToMonadicHs vset ex@(ALet _ bs e) = do
  let detIdx ((i, (_, Det)), _) = Just i
      detIdx _ = Nothing
      isDet ((_, (_, Det)), _) = True
      isDet _ = False
      vset' = Set.union vset (Set.fromList (mapMaybe detIdx bs))
      (d, nd) = partition isDet bs
  ndres <- mapM (convertBindingToMonadic vset' ex) nd
  dres <- mapM (convertBindingToMonadic vset' ex) d
  -- bool = was deterministic
  let collect (a, b, c, Just (x, y)) = [(a, b, c, True), (x, y, c, False)]
      collect (a, b, c, Nothing) = [(a, b, c, False)]
      bsd = concatMap collect dres
      bsnd = concatMap collect ndres
  e' <- convertExprToMonadicHs vset' e
  return $ mkShareLet (mkShareLet e' bsnd) bsd
convertExprToMonadicHs vset (AFree _ vs e) = do
  e' <- convertExprToMonadicHs vset e
  return $ foldr (mkShareBind . (,mkFree) . indexToName . fst) e' vs
convertExprToMonadicHs vset (AOr _ e1 e2) =
  mkMplus <$> convertExprToMonadicHs vset e1 <*> convertExprToMonadicHs vset e2
convertExprToMonadicHs vset (ACase _ Flex e bs)
  | all isLitPat bs = do
    e' <- convertExprToMonadicHs vset e
    i <- freshVarName
    bs1 <- foldr mkMplus mkFailed <$> mapM (mkFlexVarBranch vset i) bs
    bs2 <- mkFlexValBranch vset bs
    return $ mkLiteralCase e' (Hs.Lambda () [Hs.PVar () i] bs1) bs2
  where isLitPat (ABranch (ALPattern _ _) _) = True
        isLitPat _ = False
convertExprToMonadicHs _ ex@(ACase (ty, Det) _ _ _) =
  mkFromHaskellTyped <$> convertToHs ex <*> convertToMonadicHs ty
convertExprToMonadicHs vset (ACase _ _ e bs)
  | (_, Det) <- exprAnn e = do
      e' <- convertToHs e
      bs' <- mapM (convertDetBranchToMonadic vset) bs
      return $ Hs.Case () e' (bs' ++ [failedMonadicBranch])
  | otherwise = increaseCaseDepth $ do
      tyEnv <- asks newtypeNames
      case bs of
        [ABranch p@(APattern _ (qname, _) [(v,_)]) be] | Set.member qname tyEnv -> do
          (\p' e' bs' -> mkLetBind (indexToName v, Hs.App ()
            (mkFmap (Hs.Lambda () [p'] (Hs.Var () (UnQual () (indexToName v))))) e') bs')
            <$> convertToMonadicHs p
            <*> convertExprToMonadicHs vset e
            <*> convertExprToMonadicHs vset be
        _ -> do
          CMState { caseDepth } <- get
          e' <- convertExprToMonadicHs vset e
          bs' <- concat <$> mapM (convertBranchToMonadicHs caseDepth vset) bs
          let f | caseDepth < maxCaseDepth = id
                | otherwise =  Hs.App () (Hs.Var () (Qual () (ModuleName () "B") (Ident () "elimFlatM")))
          return $ mkBind (f e') $ Hs.LCase () (bs' ++ [failedMonadicBranch])

convertExprToMonadicHs _ ex@(ATyped (ty, Det) _ _) =
  mkFromHaskellTyped <$> convertToHs ex <*> convertToMonadicHs ty
convertExprToMonadicHs vset (ATyped _ e ty) =
  ExpTypeSig () <$> convertExprToMonadicHs vset e <*> pure (convertQualType ty)

convertBindingToMonadic :: Set.Set Int -> AExpr a
                        -> ((Int, (TypeExpr, NDInfo)), AExpr (TypeExpr, NDInfo))
                        -> CM (Name (), Exp (), VarUse, Maybe (Name (), Exp ()))
convertBindingToMonadic _ _ ((a, (ty, Det)), b) = do
  ty' <- convertToMonadicHs ty
  let nd = Just (appendName "_nd" $ indexToName a
                , mkFromHaskellTyped (Hs.Var () (UnQual () (indexToName a))) ty')
  (indexToName a, , One, nd)
          <$> convertToHs b
convertBindingToMonadic vset ex ((a, (_, NonDet)), b) =
  (indexToName a, , countVarUse ex a, Nothing)
          <$> convertExprToMonadicHs vset b

mkFlexVarBranch :: Set.Set Int -> Name ()
                -> ABranchExpr (TypeExpr, NDInfo)
                -> CM (Exp ())
mkFlexVarBranch vset i (ABranch (ALPattern a l) be) = do
  litE <- convertExprToMonadicHs vset (ALit a l)
  be' <- convertExprToMonadicHs vset be
  return (mkCondSeq (mkBindVar (Hs.Var () (UnQual () i)) (Hs.ExpTypeSig () litE eType)) be')
  where
    eType = TyApp () (TyCon () curryQualName) $
            TyCon () $ Qual () (ModuleName () "P") $ Ident () $ case l of
              Intc _ -> "Integer"
              Floatc _ -> "Double"
              Charc _ -> "Char"
mkFlexVarBranch _ _ b = error $ "mkFlexBranch: " ++ show b

mkFlexValBranch :: Set.Set Int
                -> [ABranchExpr (TypeExpr, NDInfo)]
                -> CM (Exp ())
mkFlexValBranch vset bs = do
  bs' <- mapM (convertDetBranchToMonadic vset) bs
  return (Hs.LCase () (bs' ++ [failedMonadicBranch]))

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
  | (ty, Det) <- exprAnn e = do
      mty <- convertToMonadicHs ty
      e' <- convertToHs e
      pat' <- convertToHs pat
      return (Alt () pat' (UnGuardedRhs () (mkFromHaskellTyped e' mty)) Nothing)
  | otherwise = do
      pat' <- convertToHs pat
      vsNames <- case pat of
                  APattern _ _ args -> mapM (\(i, (ty, _)) -> (i,) <$> convertToMonadicHs ty) args
                  _                 -> return []
      let vSet' = Set.union vSet (Set.fromList (map fst vsNames))
      analysis <- asks ndAnalysisResult
      dataEnv <- asks dataWithFunNames
      let annE = annotateND' analysis (Map.fromSet (const Det) vSet')
                   dataEnv (genTypedExpr (fmap fst e))
      e' <- convertExprToMonadicHs vSet' annE
      return (Alt () pat' (UnGuardedRhs () (foldr (uncurry mkFromHaskellBind) e' vsNames)) Nothing)

convertBranchToMonadicHs :: Integer -> Set.Set Int -> ABranchExpr (TypeExpr, NDInfo) -> CM [Alt ()]
convertBranchToMonadicHs n vSet (ABranch pat e)
  | (ty, Det) <- exprAnn e = do
      e' <- convertToHs e
      mty <- convertToMonadicHs ty
      let transE = mkFromHaskellTyped e' mty
      pat1 <- convertToMonadicHs pat
      let alt2 = case pat of
                   APattern _ (qname, (ty', _)) args ->
                    [Alt () (mkFlatPattern qname ty' (map fst args)) (UnGuardedRhs () transE) Nothing]
                   ALPattern _ _ -> []
      let alt1 = Alt () pat1 (UnGuardedRhs () transE) Nothing
      return (alt1 : if n <= maxCaseDepth then alt2 else [])
  | otherwise = do
      alt1 <- do
        e' <- convertExprToMonadicHs vSet e
        pat' <- convertToMonadicHs pat
        return $ Alt () pat' (UnGuardedRhs () e') Nothing
      case pat of
        APattern _ (qname@(_, baseName), (ty', _)) vs
          | not ("_Dict#" `isPrefixOf` baseName) -> do
          vsTyped <- mapM (\(i, (ty, _)) -> (i,) <$> convertToMonadicHs ty) vs
          let vsNames = map fst vsTyped
          let vSet' = Set.union vSet (Set.fromList vsNames)
          analysis <- asks ndAnalysisResult
          dataEnv <- asks dataWithFunNames
          let annE = annotateND' analysis (Map.fromSet (const Det) vSet')
                       dataEnv (genTypedExpr (fmap fst e))
          e' <- convertExprToMonadicHs vSet' annE
          let pat' = mkFlatPattern qname ty' vsNames
          let alt2 = [Alt () pat' (UnGuardedRhs () (foldr (uncurry mkFromHaskellBind) e' vsTyped)) Nothing]
          return (alt1 : if n <= maxCaseDepth then alt2 else [])
        _ -> return [alt1]

mkFlatPattern :: QName -> TypeExpr -> [Int] -> Pat ()
mkFlatPattern qname ty args =
  PApp () (convertQualNameToFlatQualName (typeExprQualName ty))
  [PApp () (convertTypeNameToHs qname) $
  map (PVar () . indexToName) args]
  where
   typeExprQualName x = case x of
     TCons qname' _   -> qname'
     ForallType _ ty' -> typeExprQualName ty'
     _                -> error "mkFlatPattern: typeExprQualName"

type instance HsEquivalent (APattern (TypeExpr, NDInfo)) = (Pat ())
instance ToHs (APattern (TypeExpr, NDInfo)) where
  convertToHs (APattern _ (qname, _) args) =
    return (PApp () (convertTypeNameToHs qname) (map (PVar () . indexToName . fst) args))
  convertToHs (ALPattern _ lit) =
    return $ convertLit (PParen ()) (PLit () (Signless ())) lit

instance ToMonadicHs (APattern (TypeExpr, NDInfo)) where
  convertToMonadicHs p@(ALPattern _ _) = convertToHs p
  convertToMonadicHs (APattern _ (qname, _) args) = do
    return ( PApp () (convertTypeNameToMonadicHs qname) (map (PVar () . indexToName . fst) args))

litSign :: Literal -> Sign ()
litSign (Intc i)
  | i < 0 = Negative ()
litSign (Floatc f)
  | f < 0 = Negative ()
litSign _ = Signless ()

convertLit :: (a -> a) -> (Hs.Literal () -> a) -> Literal -> a
convertLit f g l = case l of
  Intc i   -> f' $ g $ Hs.Int () i (show i)
  Floatc d -> f' $ g $ Hs.Frac () (toRational d) (show d)
  Charc c  ->  f' $ g $ Hs.Char () c (show c)
  where f' = case litSign l of
              Negative _ -> f
              Signless _ -> id

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
  convertTypeNameToHs n@(m, _)
    | n == anyQName = anyHsQualName
    | otherwise = Hs.Qual () (ModuleName () (convertModName m)) (convertTypeNameToHs (Unqual n))
  convertFuncNameToHs n@(m, _) = Hs.Qual () (ModuleName () (convertModName m)) (convertFuncNameToHs (Unqual n))

instance ToMonadicHsName QName where
  convertTypeNameToMonadicHs n@(m, _)
    | n == anyQName = anyHsQualName
    | otherwise = Hs.Qual () (ModuleName () (convertModName m)) (convertTypeNameToMonadicHs (Unqual n))
  convertFuncNameToMonadicHs n@(m, _) = Hs.Qual () (ModuleName () (convertModName m)) (convertFuncNameToMonadicHs (Unqual n))

convertQualNameToFlatName :: QName -> Name ()
convertQualNameToFlatName (_, s) =
  Ident () (escapeTypeName s ++ "Flat#")

convertQualNameToFlatQualName :: QName -> Hs.QName ()
convertQualNameToFlatQualName qname@(m, _) =
  Qual () (ModuleName () (convertModName m)) $ convertQualNameToFlatName qname

convertModName :: String -> String
convertModName m = convertModName' $ break (== '.') m
  where
    convertModName' (x, [])     = "Curry_" ++ x
    convertModName' (x, y:ys) = x ++ (y : convertModName' (break (== '.') ys))

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
  , ImportDecl () (ModuleName () "BasicDefinitions") True False False Nothing (Just (ModuleName () "B")) Nothing
  , ImportDecl () (ModuleName () "Control.Monad") True False False Nothing (Just (ModuleName () "M")) Nothing
  , ImportDecl () (ModuleName () "Control.Monad.Fix") True False False Nothing (Just (ModuleName () "M")) Nothing
  , ImportDecl () (ModuleName () "Prelude") True False False Nothing (Just (ModuleName () "P")) Nothing
  , ImportDecl () (ModuleName () "Text.Read") True False False Nothing (Just (ModuleName () "P")) Nothing
  , ImportDecl () (ModuleName () "Text.Read.Lex") True False False Nothing (Just (ModuleName () "P")) Nothing
  , ImportDecl () (ModuleName () "Text.ParserCombinators.ReadPrec") True False False Nothing (Just (ModuleName () "P")) Nothing
  , ImportDecl () (ModuleName () "Control.DeepSeq") True False False Nothing (Just (ModuleName () "P"))  Nothing
  ]

-- |Compute the filename of the Haskell file for a source file
haskellName :: FilePath -> FilePath
haskellName p = flip updateModuleName ("Curry_"++) $
  if takeExtension p == ".curry"
    then replaceExtension p haskellExt
    else p <.> haskellExt

updateModuleName :: FilePath -> (String -> String) -> FilePath
updateModuleName p f = replaceFileName p $ intercalate "." $ reverse $ mapSecond $ splitExts $ takeFileName p
  where
    splitExts = unfoldr (\q -> let ~(r, e) = splitExtension q in
      if null q
        then Nothing
        else if null e
          then Just (r, "") -- keep the rest
          else Just (drop 1 e, r)) -- drop the dot
    mapSecond (x:y:xs) = x : f y : xs
    mapSecond xs = xs

-- |Filename extension for Haskell files
haskellExt :: String
haskellExt = ".hs"

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

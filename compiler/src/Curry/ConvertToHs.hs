{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Curry.ConvertToHs (compileToHs) where

import Control.Monad (void, replicateM)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader (..), runReaderT)
import Control.Monad.State (StateT, MonadState (..), evalStateT, modify)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Coerce (coerce)
import Data.Char (toLower, toUpper)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts hiding (Literal, Cons, Kind, QName)
import qualified Language.Haskell.Exts as Hs
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)
import System.IO (openFile, IOMode (..), utf8, hSetEncoding, hPutStr, hClose, hGetContents')

import Base.Messages (status, abortWithMessages, Message, message)
import Curry.Base.Ident (ModuleIdent)
import Curry.FlatCurry hiding (Let)
import Curry.FlatCurry.Annotated.Type
import Curry.FlatCurry.Typed.Type
import Curry.Files.Filenames (addOutDirModule)
import qualified CompilerOpts as Frontend
import CompilerOpts (Options(..))
import CurryBuilder (smake, compMessage)

import Curry.Analysis (NDAnalysisResult, NDInfo (..))
import Curry.Annotate (annotateND, isFunFree)
import Haskell.ExtsInstances ()

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

compileToHs :: [(TProg, ModuleIdent, FilePath)] -> NDAnalysisResult -> Frontend.Options
            -> IO [Module ()]
compileToHs mdls ndInfo opts = evalCM (mapM process' (zip [1 ..] mdls)) ndInfo
  where
    total = length mdls
    deps = map (\(_, _, dep) -> dep) mdls
    process' (n, (prog, m, fp)) = process opts (n, total) prog m fp deps

process :: Frontend.Options -> (Int, Int) -> TProg
        -> ModuleIdent -> FilePath -> [FilePath] -> CM (Module ())
process opts idx tprog m fn deps
  | optForce opts = compile
  | otherwise     = smake [destFile] deps compile skip
  where
    destFile = tgtDir (haskellName fn)
    skip = do
      status opts $ compMessage idx (11, 16) "Skipping" m (fn, destFile)
      eithRes <- liftIO (parseFile' destFile)
      case eithRes of
        ParseFailed loc err -> do
          liftIO $ putStr $ unlines
            [ "Haskell file is corrupted."
            , "For the file \"" ++ fn ++ "\"."
            , "Parsing failed with:"
            , err
            , "At location:"
            , prettyPrint loc
            , "Retrying compilation from analyzed flat curry ..." ]
          compile
        ParseOk res -> return (void res)
    compile = do
      status opts $ compMessage idx (11, 16) "Translating" m (fn, destFile)
      res <- convertToHs (TWFP (tprog, externalName fn))
      liftIO $ writeUTF8File' (tgtDir (haskellName fn)) (prettyPrint res)
      return res

    tgtDir = addOutDirModule (optUseOutDir opts) (optOutDir opts) m

type family HsEquivalent a = hs | hs -> a

class ToHs a where
  convertToHs :: a -> CM (HsEquivalent a)

class ToMonadicHs a where
  convertToMonadicHs :: a -> CM (HsEquivalent a)

newtype TProgWithFilePath = TWFP (TProg, FilePath)
type instance HsEquivalent TProgWithFilePath = Module ()
instance ToHs TProgWithFilePath where
  convertToHs (TWFP (TProg nm im tys fs _, fp)) = do
    (header, curryImports, curryDs) <- do
        im' <- mapM (convertToHs . IS) im
        tyds <- mapM convertToHs tys
        funds <- mapM convertToHs fs
        tydsM <- mapM convertToMonadicHs tys
        fundsM <- mapM convertToMonadicHs fs
        let visT = mapMaybe getVisT tys
        let visF = mapMaybe getVisF fs
        header <- convertToHs (MS (nm, visT, visF))
        let extract (Just (x, y)) = [x,y]
            extract Nothing = []
        let insts = concatMap genInstances tys
        let ds = insts ++ coerce (tyds ++ tydsM) ++ concatMap @[] extract (coerce (funds ++ fundsM))
        -- TODO: deriving stuff?
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
    return (Module () (Just header) ps' im' ds')
    where
      getVisT (Type qname Public _ cs) = Just (qname, mapMaybe getVisC cs)
      getVisT (TypeSyn qname Public _ _) = Just (qname, [])
      getVisT (TypeNew qname Public _ c) = Just (qname, maybe [] return (getVisN c))
      getVisT _ = Nothing

      getVisC (Cons qname _ Public _) = Just qname
      getVisC _ = Nothing

      getVisN (NewCons qname Public _) = Just qname
      getVisN _ = Nothing

      getVisF (TFunc qname _ Public _ _) = Just qname
      getVisF _ = Nothing

genInstances :: TypeDecl -> [Decl ()]
genInstances (Type _ _ _ []) = []
genInstances (Type qname _ vs cs) = map hsEquivDecl [0..length vs] ++ [shareableDecl, hsToDecl, hsFromDecl]
  where
    hsEquivDecl arity = TypeInsDecl () (TyApp () (TyCon () hsEquivQualName)
      (foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
        (map (TyVar () . indexToName . fst) (take arity vs))))
      (foldl (TyApp ()) (TyCon () (convertTypeNameToHs qname))
        (map (TyApp () (TyCon () hsEquivQualName) . TyVar () . indexToName . fst) (take arity vs)))
    shareableDecl = InstDecl () Nothing
      (IRule () Nothing (mkShareableCtxt vs)
        (IHApp () (IHCon () shareableQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (map mkShareMatch cs))])
    hsToDecl = InstDecl () Nothing
      (IRule () Nothing (mkShareableCtxt vs)
        (IHApp () (IHCon () hsToQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkToMatch cs))])
    hsFromDecl = InstDecl () Nothing
      (IRule () Nothing (mkShareableCtxt vs)
        (IHApp () (IHCon () hsFromQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkFromMatch cs))])
    mkShareMatch (Cons qname2 ar _ _) = Match () (Ident () "shareArgs")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () (mkShareImpl qname2 ar)) Nothing
    mkToMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "to")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (mkToImpl qname2 ar)
    mkFromMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "from")
      [PApp () (convertTypeNameToHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (mkFromImpl qname2 ar)
    mkShareImpl qname2 ar
      | not ("_Dict#" `isPrefixOf` snd qname2) =
        mkApplicativeChain (Hs.Var () (convertTypeNameToMonadicHs qname2))
                           (map (mkShare . Hs.Var () . UnQual () . indexToName) [1..ar])
      | otherwise =
        mkReturn (foldl (Hs.App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
                   (map (Hs.Var () . UnQual () . indexToName) [1..ar]))
    mkToImpl qname2 ar
      | not ("_Dict#" `isPrefixOf` snd qname2) = Just $
        mkApplicativeChain (Hs.Var () (convertTypeNameToHs qname2))
                           (map (mkToHaskell . Hs.Var () . UnQual () . indexToName) [1..ar])
      | otherwise = Nothing
    mkFromImpl qname2 ar
      | not ("_Dict#" `isPrefixOf` snd qname) = Just $
        foldl (Hs.App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
          (map (mkFromHaskell . Hs.Var () . UnQual () . indexToName) [1..ar])
      | otherwise = Nothing
genInstances TypeSyn {} = []
genInstances (TypeNew qname1 vis1 vs (NewCons qname2 vis2 ty)) =
  genInstances (Type qname1 vis1 vs [Cons qname2 1 vis2 [ty]])

mkApplicativeChain :: Hs.Exp () -> [Hs.Exp ()] -> Hs.Exp ()
mkApplicativeChain e [] = mkReturn e
mkApplicativeChain e xs = InfixApp () e (QVarOp () dollarApplicativeQualName)
  (foldl1 (flip (InfixApp ()) (QVarOp () starApplicativeQualName)) xs)

dollarApplicativeQualName :: Hs.QName ()
dollarApplicativeQualName = Qual () (ModuleName () "P") (Symbol () "<$>")

starApplicativeQualName :: Hs.QName ()
starApplicativeQualName = Qual () (ModuleName () "P") (Symbol () "<*>")

mkFromHaskell :: Exp () -> Exp ()
mkFromHaskell = Hs.App () (Hs.Var () fromHaskellQualName)

fromHaskellQualName :: Hs.QName ()
fromHaskellQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "fromHaskell")

mkToHaskell :: Exp () -> Exp ()
mkToHaskell = Hs.App () (Hs.Var () toHaskellQualName)

toHaskellQualName :: Hs.QName ()
toHaskellQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "toHaskell")

hsEquivQualName :: Hs.QName ()
hsEquivQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "HsEquivalent")

hsToQualName :: Hs.QName ()
hsToQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "ToHs")

hsFromQualName :: Hs.QName ()
hsFromQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "FromHs")

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
        [ (convertTypeNameToHs qname, map conItem csNames)
        , (convertTypeNameToMonadicHs qname, map conItemMonadic csNames) ]

      conItem qname = ConName () (convertTypeNameToHs (Unqual qname))
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
      TyCon () (Qual () (ModuleName () ("Curry_" ++ mdl)) (Ident () (escapeTypeName nm ++ "#")))
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
      TyCon () (Qual () (ModuleName () ("Curry_" ++ mdl)) (Ident () (escapeTypeName nm ++ "ND#")))
    | otherwise = do
      cs' <- mapM convertToMonadicHs cs
      return $ HTD $
        DataDecl () (DataType ()) Nothing (mkMonadicTypeHead qname vs) cs' []
  convertToMonadicHs (TypeSyn qname _ vs texpr) =  do
    ty <- convertToMonadicHs texpr
    return $ HTD $
      TypeDecl () (mkMonadicTypeHead qname vs) ty
  convertToMonadicHs (TypeNew qname _ vs (NewCons cname cvis texpr)) = do
    c' <- convertToMonadicHs (Cons cname 1 cvis [texpr])
    return $ HTD $
      DataDecl () (DataType ()) Nothing (mkMonadicTypeHead qname vs) [c'] []

type instance HsEquivalent ConsDecl = QualConDecl ()
instance ToHs ConsDecl where
  convertToHs (Cons cname _ _ texprs) = do
    tys <- mapM convertToHs texprs
    return $ QualConDecl () Nothing Nothing (ConDecl () (convertTypeNameToHs (Unqual cname)) tys)

instance ToMonadicHs ConsDecl where
  convertToMonadicHs (Cons cname _ _ texprs) = do
    tys <- mapM convertQualType texprs
    return $ QualConDecl () Nothing Nothing (ConDecl () (convertTypeNameToMonadicHs (Unqual cname)) tys)

convertQualType :: TypeExpr -> CM (Type ())
convertQualType ty
  | ForallType _ _ <- ty = ty'
  | otherwise            = mkCurry <$> ty'
  where
    ty' = convertToMonadicHs ty

mkTypeHead :: QName -> [TVarWithKind] -> DeclHead ()
mkTypeHead qname vs = foldl (DHApp ()) (DHead () (convertTypeNameToHs (Unqual qname))) $ map toTyVarBndr vs

mkMonadicTypeHead :: QName -> [TVarWithKind] -> DeclHead ()
mkMonadicTypeHead qname vs = foldl (DHApp ()) (DHead () (convertTypeNameToMonadicHs (Unqual qname))) $ map toTyVarBndr vs

toTyVarBndr :: TVarWithKind -> TyVarBind ()
toTyVarBndr (i, k) = KindedVar () (indexToName i) (kindToHsType k)

kindToHsType :: Kind -> Type ()
kindToHsType KStar = TyCon () (Qual () (ModuleName () "Data.Kind") (Ident () "Type"))
kindToHsType (KArrow k1 k2) = TyFun () (kindToHsType k1) (kindToHsType k2)

indexToName :: Int -> Name ()
indexToName i = [Ident () (c : rest) | n <- [0 :: Int ..], let rest = if n == 0 then "" else show n, c <- ['a'..'z']] !! i

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
  convertToMonadicHs (ForallType vs t) = TyForall () (Just (map toTyVarBndr vs)) (mkShareableCtxt vs) <$> convertQualType t

mkShareableCtxt :: [TVarWithKind] -> Maybe (Context ())
mkShareableCtxt [] = Nothing
mkShareableCtxt vs = Just $ CxTuple () $ map mkShareableFor vs

mkShareableFor :: TVarWithKind -> Asst ()
mkShareableFor (i, KStar) = TypeA () $ mkShareableType (TyVar () (indexToName i))
mkShareableFor (i, k) = construct $ map mkShareableFor argsWithTVar
  where
    (args, _) = splitKindArrow k
    argTVars = take (length args) [i+1..]
    argsWithTVar = zip argTVars args
    construct shareableArgs = TypeA () $ TyForall () (Just (map toTyVarBndr argsWithTVar))
      (Just (CxTuple () shareableArgs))
      (mkShareableType (foldl (TyApp ()) (TyVar () (indexToName i)) (map (TyVar () . indexToName) argTVars)))

splitKindArrow :: Kind -> ([Kind], Kind)
splitKindArrow KStar = ([], KStar)
splitKindArrow (KArrow k1 k2) = (k1 : args, res)
  where
    (args, res) = splitKindArrow k2

mkShareableType :: Type () -> Type ()
mkShareableType = TyApp () (TyCon () shareableQualName)

mkLiftedFunc :: Type () -> Type () -> Type ()
mkLiftedFunc t1 = TyApp () (TyApp () (TyCon () liftedFuncQualName) t1)

mkCurry :: Type () -> Type ()
mkCurry = TyApp () (TyCon () curryQualName)

shareableQualName :: Hs.QName ()
shareableQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "Shareable")

liftedFuncQualName :: Hs.QName ()
liftedFuncQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "LiftedFunc")

curryQualName :: Hs.QName ()
curryQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "Curry")

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
      (UnGuardedRhs () (Hs.Var () (UnQual () (Ident () (escapeFuncName unqualStr ++ "#"))))) Nothing
    where unqualStr = case dropWhile (/= '.') str of
            ""  -> str
            "." -> str
            _:s -> s

instance ToMonadicHs RuleInfo where
  convertToMonadicHs (RI (qname, TRule args expr)) = do
    analysis <- ask
    e <- convertToMonadicHs (annotateND analysis expr)
    let e' = foldr (\v -> mkReturnFunc .  Lambda () [PVar () $ indexToName $ fst v]) e args
    return $ Match () (convertFuncNameToMonadicHs (Unqual qname)) []
      (UnGuardedRhs () e') Nothing
  convertToMonadicHs (RI (qname, TExternal _ str)) = return $
    Match () (convertFuncNameToMonadicHs (Unqual qname)) []
      (UnGuardedRhs () (Hs.Var () (UnQual () (Ident () (escapeFuncName unqualStr ++ "ND#"))))) Nothing
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
    return $ foldl (Hs.App ()) (Hs.Var () (convertNameToHs qname)) args'
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
  convertToHs (ATyped _ e ty) = ExpTypeSig () <$> convertToHs e <*> convertQualType ty

failedBranch :: Alt ()
failedBranch = Alt () (PWildCard ())
  (UnGuardedRhs ()
    (Hs.Var () (Qual () (ModuleName () "Curry_Prelude") (Ident () "failed")))) Nothing

instance ToMonadicHs (AExpr (TypeExpr, NDInfo)) where
  convertToMonadicHs (AVar _ idx) = return $ Hs.Var () (UnQual () (indexToName idx))
  convertToMonadicHs (ALit _ lit) = return $ mkReturn $ Hs.Lit () (convertLit lit)
  convertToMonadicHs ex@(AComb (_, Det) FuncCall _ _) =
    mkFromHaskell <$> convertToHs ex
  convertToMonadicHs ex@(AComb (_, Det) ConsCall  _ _) =
    mkFromHaskell <$> convertToHs ex
  convertToMonadicHs (AComb (_, NonDet) ConsCall (qname, _) args) = do
    args' <- mapM convertToMonadicHs args
    return $ mkReturn $ foldl (Hs.App ()) (Hs.Var () (convertTypeNameToMonadicHs qname)) args'
  convertToMonadicHs (AComb _ (ConsPartCall missing) (qname, _) args) = do
    args' <- mapM convertToMonadicHs args
    missingVs <- replicateM missing freshVarName
    let mkLam e = foldr (\v -> mkReturnFunc .  Lambda () [PVar () v]) e missingVs
    return $ mkLam $ mkReturn $ foldl (Hs.App ()) (Hs.Var () (convertTypeNameToMonadicHs qname)) (args' ++ map (Hs.Var () . UnQual ()) missingVs)
  convertToMonadicHs (AComb _ _ (qname, _) args) = do -- (Partial) FuncCall
    args' <- mapM convertToMonadicHs args
    return $ foldl mkMonadicApp (Hs.Var () (convertFuncNameToMonadicHs qname)) args'
  convertToMonadicHs ex@(ALet (ty, Det) _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToMonadicHs ex@(ALet _ bs e) = do
    e' <- convertToMonadicHs e
    bs' <- mapM (\((a, _), b) -> (indexToName a, , countVarUse ex a) <$> convertToMonadicHs b) bs
    return $ mkShareLet e' bs'
  convertToMonadicHs (AFree _ vs e) = do
    e' <- convertToMonadicHs e
    return $ foldr mkShareBind e' (zip3 (map (indexToName . fst) vs) (repeat mkFree) (map (countVarUse e . fst) vs))
  convertToMonadicHs (AOr _ e1 e2) = mkMplus <$> convertToMonadicHs e1 <*> convertToMonadicHs e2
  convertToMonadicHs ex@(ACase (ty, Det) _ e bs)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
    | otherwise = do
      e' <- mkFromHaskell <$> convertToMonadicHs e
      bs' <- mapM convertToMonadicHs bs
      return $ mkBind e' $ Hs.LCase () (bs' ++ [failedMonadicBranch])
  convertToMonadicHs (ACase (_, NonDet) _  e bs) = do
    e' <- convertToMonadicHs e
    bs' <- mapM convertToMonadicHs bs
    return $ mkBind e' $ Hs.LCase () (bs' ++ [failedMonadicBranch])
  convertToMonadicHs ex@(ATyped (ty, Det) _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToMonadicHs (ATyped _ e ty) = ExpTypeSig () <$> convertToMonadicHs e <*> convertToMonadicHs ty

failedMonadicBranch :: Alt ()
failedMonadicBranch = Alt () (PWildCard ())
  (UnGuardedRhs ()
    (Hs.Var () (Qual () (ModuleName () "Curry_Prelude") (Ident () "failedND")))) Nothing

mkMonadicApp :: Exp () -> Exp () -> Exp ()
mkMonadicApp e1 = Hs.App () (Hs.App () (Hs.Var () appQualName) e1)

appQualName :: Hs.QName ()
appQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "app")

mkMplus :: Exp () -> Exp () -> Exp ()
mkMplus e1 = Hs.App () (Hs.App () (Hs.Var () mplusQualName) e1)

mplusQualName :: Hs.QName ()
mplusQualName = Qual () (ModuleName () "M") (Ident () "mplus")

mkShareLet :: Hs.Exp () -> [(Hs.Name (), Hs.Exp (), VarUse)] -> Hs.Exp ()
mkShareLet e [] = e
mkShareLet e bs
  | all (\(_, _, m) -> notMany m) bs = foldr mkShareBind e bs
  | otherwise = mkBind (mkMFix fixLam) bindLam
  where
    notMany Many = False
    notMany _    = True
    lamHead = Lambda () [mkLazyTuplePat $ map (\(v, _, _) -> PVar () v) bs]
    fixLam = lamHead $ mkApplicativeChain (mkTupleCon (length bs))
                     $ map (\(_, e',_) -> mkShare e') bs
    bindLam = lamHead e

mkTupleCon :: Int -> Exp ()
mkTupleCon 0 = Hs.Var () (Special () (UnitCon ()))
mkTupleCon 1 = Hs.Var () (Qual () (ModuleName () "P") (Ident () "id"))
mkTupleCon n = Hs.Var () (Special () (TupleCon () Boxed n))

mkLazyTuplePat :: [Hs.Pat ()] -> Hs.Pat ()
mkLazyTuplePat [p] = PParen () p
mkLazyTuplePat xs  = PParen () $ PIrrPat () $ PTuple () Boxed xs

mkMFix :: Hs.Exp () -> Hs.Exp ()
mkMFix = Hs.App () (Hs.Var () (Qual () (ModuleName () "M") (Ident () "mfix")))

data VarUse = None | One | Many
  deriving (Eq, Ord, Enum, Show)

instance Semigroup VarUse where
  None <> x = x
  x <> None = x
  _ <> _ = Many

instance Monoid VarUse where
  mempty = None

countVarUse :: AExpr a -> VarIndex -> VarUse
countVarUse (AVar _ idx) vidx = if idx == vidx then One else None
countVarUse (AComb _ _ _ args) vidx = mconcat (map (`countVarUse` vidx) args)
countVarUse (ALet _ bs e) vidx = case mconcat (map (\((_, _), b) -> countVarUse b vidx) bs) of
  None -> countVarUse e vidx
  _ -> Many
countVarUse (AFree _ _ e) vidx = countVarUse e vidx
countVarUse (AOr _ e1 e2) vidx = max (countVarUse e1 vidx) (countVarUse e2 vidx)
countVarUse (ACase _ _ e bs) vidx = max (countVarUse e vidx) (maximum (map (\(ABranch _ e') -> countVarUse e' vidx) bs))
countVarUse (ALit _ _) _ = None
countVarUse (ATyped _ e _) vidx = countVarUse e vidx

mkShareBind :: (Hs.Name (), Exp (), VarUse) -> Exp () -> Exp ()
mkShareBind (_, _, None) e2 = e2
mkShareBind (v, e1, One ) e2 = Hs.Let () (BDecls () [PatBind () (PVar () v) (UnGuardedRhs () e1) Nothing]) e2
mkShareBind (v, e1, Many) e2 = mkBind (mkShare e1) (Lambda () [PVar () v] e2)

mkShare :: Exp () -> Exp ()
mkShare = Hs.App () (Hs.Var () shareQualName)

mkFree :: Exp ()
mkFree = Hs.Var () freeQualName

freeQualName :: Hs.QName ()
freeQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "free")

mkBind :: Exp () -> Exp () -> Exp ()
mkBind e1 = Hs.InfixApp () e1 (QConOp () bindQualName)

bindQualName :: Hs.QName ()
bindQualName = Hs.Qual () (ModuleName () "M") (Symbol () ">>=")

shareQualName :: Hs.QName ()
shareQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "share")

mkReturn :: Exp () -> Exp ()
mkReturn = Hs.App () (Hs.Var () returnQualName)

returnQualName :: Hs.QName ()
returnQualName = Hs.Qual () (ModuleName () "M") (Ident () "return")

mkReturnFunc :: Exp () -> Exp ()
mkReturnFunc = Hs.App () (Hs.Var () returnFuncQualName)

returnFuncQualName :: Hs.QName ()
returnFuncQualName = Hs.Qual () (ModuleName () "BasicDefinitions") (Ident () "returnFunc")

type instance HsEquivalent (ABranchExpr (TypeExpr, NDInfo)) = Alt ()
instance ToHs (ABranchExpr (TypeExpr, NDInfo)) where
  convertToHs (ABranch pat e) = do
    e' <- convertToHs e
    pat' <- convertToHs pat
    return $ Alt () pat' (UnGuardedRhs () e') Nothing

instance ToMonadicHs (ABranchExpr (TypeExpr, NDInfo)) where
  convertToMonadicHs (ABranch pat e) = do
    e' <- convertToMonadicHs e
    (pat', vs) <- convertPatToMonadic pat e
    return $ Alt () pat' (UnGuardedRhs () (foldr mkShareBind e' vs)) Nothing

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

newtype UnqualName = Unqual QName
type instance HsEquivalent UnqualName = Name ()
instance ToHsName UnqualName where
  convertTypeNameToHs (Unqual (_, s)) = Ident () $ escapeTypeName s
  convertFuncNameToHs (Unqual (_, s)) = Ident () $ escapeFuncName s

instance ToMonadicHsName UnqualName where
  convertTypeNameToMonadicHs (Unqual (_, s)) = Ident () $ escapeTypeName s ++ "ND"
  convertFuncNameToMonadicHs (Unqual (_, s)) = Ident () $ escapeFuncName s ++ "ND"

type instance HsEquivalent QName = Hs.QName ()
instance ToHsName QName where
  convertTypeNameToHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertTypeNameToHs (Unqual n))
  convertFuncNameToHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertFuncNameToHs (Unqual n))

instance ToMonadicHsName QName where
  convertTypeNameToMonadicHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertTypeNameToMonadicHs (Unqual n))
  convertFuncNameToMonadicHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertFuncNameToMonadicHs (Unqual n))

defaultPragmas :: [ModulePragma ()]
defaultPragmas =
  [ LanguagePragma () [Ident () "NoImplicitPrelude"]
  , LanguagePragma () [Ident () "KindSignatures"]
  , LanguagePragma () [Ident () "TypeOperators"]
  , LanguagePragma () [Ident () "ExplicitForAll"]
  , LanguagePragma () [Ident () "ImpredicativeTypes"]
  , LanguagePragma () [Ident () "QuantifiedConstraints"]
  , LanguagePragma () [Ident () "LambdaCase"]
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
haskellName = ("Curry_" ++) . flip replaceExtension haskellExt

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

tupleStringArity :: String -> Maybe Int
tupleStringArity s = case s of
  '(':rest | last s == ')' -> Just $ length rest
  _                        -> Nothing

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

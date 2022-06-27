{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Curry.ConvertToHs where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Language.Haskell.Exts hiding (Literal, Cons, Kind, QName)
import qualified Language.Haskell.Exts as Hs
import System.FilePath (replaceExtension)

import Base.Messages (status, abortWithMessages, Message, message)
import Curry.Base.Ident (ModuleIdent)
import Curry.Base.Monad (CYIO)
import Curry.FlatCurry hiding (Let)
import Curry.FlatCurry.Typed.Type
import Curry.Files.Filenames (addOutDirModule)
import qualified CompilerOpts as Frontend
import CompilerOpts (Options(..))
import CurryBuilder (smake, compMessage)

import Curry.Analysis (NDAnalysisResult, NDInfo (..))
import Curry.FrontendUtils (runCurryFrontendAction)

newtype CM a = CM {
    runCM :: ReaderT NDAnalysisResult (ExceptT [Message] IO) a
  } deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader NDAnalysisResult, MonadError [Message] )

evalCM :: CM a -> NDAnalysisResult -> IO a
evalCM (CM a) st = do
  eith <- runExceptT (runReaderT a st)
  case eith of
    Left msgs -> abortWithMessages msgs
    Right res -> return res

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
      status opts $ compMessage idx "Skipping" m (fn, destFile)
      eithRes <- parseModule <$> liftIO (readFile (tgtDir (haskellName fn)))
      case eithRes of
        ParseFailed _ err -> do
          liftIO $ putStrLn $ unwords
            [ "Haskell file is corrupted."
            , "For the file\"" ++ fn ++ "\"."
            , "Parsing failed with:"
            , err
            , "Retrying analysis from flat curry..." ]
          compile
        ParseOk res -> return (void res)
    compile = convertToHs tprog

    tgtDir = addOutDirModule (optUseOutDir opts) (optOutDir opts) m

type family HsEquivalent a = hs | hs -> a

class ToHs a where
  convertToHs :: a -> CM (HsEquivalent a)

class ToMonadicHs a where
  convertToMonadicHs :: a -> CM (HsEquivalent a)

type instance HsEquivalent TProg = Module ()
instance ToHs TProg where
  convertToHs (TProg nm im tys fs _) = do
    head <- convertToHs (MS nm)
    im' <- mapM (convertToHs . IS) im
    tyds <- mapM convertToHs tys
    funds <- mapM convertToHs fs
    tydsM <- mapM convertToMonadicHs tys
    fundsM <- mapM convertToMonadicHs fs
    let extract (Just (x, y)) = [x,y]
        extract Nothing = []
    let ds = coerce (tyds ++ tydsM) ++ concatMap @[] extract (coerce (funds ++ fundsM))
    return (Module () (Just head) defaultPragmas (defaultImports ++ im') ds)

-- TODO export only what is visible
newtype ModuleString = MS String
type instance HsEquivalent ModuleString = ModuleHead ()
instance ToHs ModuleString where
  convertToHs (MS s) = return $ ModuleHead () (ModuleName () s) Nothing Nothing

newtype ImportString = IS String
type instance HsEquivalent ImportString = ImportDecl ()
instance ToHs ImportString where
  convertToHs (IS s) = return $ ImportDecl () (ModuleName () s) False False False Nothing Nothing Nothing

-- TODO: deriving stuff?
newtype HsTypeDecl = HTD (Decl ())
type instance HsEquivalent TypeDecl = HsTypeDecl
instance ToHs TypeDecl where
  convertToHs (Type qname vis vs cs) = do
    cs' <- mapM convertToHs cs
    return $ HTD $
      DataDecl () (DataType ()) Nothing (mkTypeHead qname vs) cs' []
  convertToHs (TypeSyn qname _ vs texpr) = do
    ty <- convertToHs texpr
    return $ HTD $
      TypeDecl () (mkTypeHead qname vs) ty
  convertToHs (TypeNew qname vis vs (NewCons cname cvis texpr)) = do
    c' <- convertToHs (Cons cname 1 cvis [texpr])
    return $ HTD $
      DataDecl () (DataType ()) Nothing (mkTypeHead qname vs) [c'] []

instance ToMonadicHs TypeDecl where
  convertToMonadicHs (Type qname vis vs cs) = undefined
  convertToMonadicHs (TypeSyn qname _ vs texpr) = undefined
  convertToMonadicHs (TypeNew qname vis vs (NewCons cname cvis texpr)) = undefined

type instance HsEquivalent ConsDecl = QualConDecl ()
instance ToHs ConsDecl where
  convertToHs (Cons cname _ cvis texprs) = do
    tys <- mapM convertToHs texprs
    return $ QualConDecl () Nothing Nothing (ConDecl () (convertNameToHs (Unqual cname)) tys)

mkTypeHead :: QName -> [TVarWithKind] -> DeclHead ()
mkTypeHead qname vs = foldl (DHApp ()) (DHead () (convertNameToHs (Unqual qname))) $ map toTyVarBndr vs

toTyVarBndr :: TVarWithKind -> TyVarBind ()
toTyVarBndr (i, k) = KindedVar () (indexToName i) (kindToHsType k)

kindToHsType :: Kind -> Type ()
kindToHsType KStar = TyCon () (Qual () (ModuleName () "Data.Kind") (Ident () "Type"))
kindToHsType (KArrow k1 k2) = TyFun () (kindToHsType k1) (kindToHsType k2)

indexToName :: Int -> Name ()
indexToName i = [Ident () (c : rest) | n <- [0..], let rest = if n == 0 then "" else show n, c <- ['a'..'z']] !! i

type instance HsEquivalent TypeExpr = Type ()
instance ToHs TypeExpr where
  convertToHs (TVar idx) = return $ TyVar () (indexToName idx)
  convertToHs (FuncType t1 t2) = TyFun () <$> convertToHs t1 <*> convertToHs t2
  convertToHs (TCons qn tys) = foldr (TyApp ()) (TyCon () (convertNameToHs qn)) <$> mapM convertToHs tys
  convertToHs (ForallType vs t) = TyForall () (Just $ map toTyVarBndr vs) Nothing <$> convertToHs t

newtype HsFuncDecl = HFD (Maybe (Decl (), Decl ()))
type instance HsEquivalent TFuncDecl = HsFuncDecl
instance ToHs TFuncDecl where
  convertToHs (TFunc qname ar vis texpr rule) = do
    analysis <- ask
    case Map.lookup qname analysis of
      Just Det -> do
        ty <- convertToHs texpr
        let tsig = TypeSig () [convertNameToHs (Unqual qname)] ty
        match <- convertToHs (RI (qname, rule))
        let f = FunBind () [match]
        return $ HFD $ Just (tsig, f)
      _ -> return $ HFD Nothing

instance ToMonadicHs TFuncDecl where
  convertToMonadicHs (TFunc qname ar vis texpr rule) = undefined

newtype RuleInfo = RI (QName, TRule)
type instance HsEquivalent RuleInfo = Match ()
instance ToHs RuleInfo where
  convertToHs (RI (qname, TRule args expr)) = do
    e <- convertToHs expr
    return $ Match () (convertNameToHs (Unqual qname)) (map (PVar () . indexToName . fst) args)
      (UnGuardedRhs () e) Nothing
  convertToHs (RI (qname, TExternal _ str)) = return $
    Match () (convertNameToHs (Unqual qname)) []
      (UnGuardedRhs () (Hs.Var () (UnQual () (Ident () str)))) Nothing

type instance HsEquivalent TExpr = Exp ()
instance ToHs TExpr where
  convertToHs (TVarE _ idx) = return $ Hs.Var () (UnQual () (indexToName idx))
  convertToHs (TLit _ lit) = return $ Hs.Lit () (convertLit lit)
  convertToHs (TComb _ _ qname args) = do
    args' <- mapM convertToHs args
    return $ foldl (Hs.App ()) (Hs.Var () (convertNameToHs qname)) args'
  convertToHs (TLet bs e) = do
    e' <- convertToHs e
    bs' <- mapM (\((v, _), lclE) -> PatBind () (PVar () (indexToName v))
                     <$> (UnGuardedRhs () <$> convertToHs e)
                     <*> pure Nothing) bs
    return $ Let () (BDecls () bs') e'
  convertToHs (TFree _ _) = throwError [message "Encountered a free variable in an expression inferred to be deterministic"]
  convertToHs (TOr _ _) = throwError [message "Encountered an 'or' in an expression inferred to be deterministic"]
  convertToHs (TCase _ e bs) = do
    e' <- convertToHs e
    bs' <- mapM convertToHs bs
    return $ Hs.Case () e' bs'
  convertToHs (TTyped e ty) = ExpTypeSig () <$> convertToHs e <*> convertToHs ty

type instance HsEquivalent TBranchExpr = Alt ()
instance ToHs TBranchExpr where
  convertToHs (TBranch pat e) = do
    e' <- convertToHs e
    pat' <- convertToHs pat
    return $ Alt () pat' (UnGuardedRhs () e') Nothing

type instance HsEquivalent TPattern = Pat ()
instance ToHs TPattern where
  convertToHs (TPattern _ qname args) = return $ PApp () (convertNameToHs qname) (map (PVar () . indexToName . fst) args)
  convertToHs (TLPattern _ lit) = return $ PLit () (litSign lit) (convertLit lit)
  -- TODO: remove sign?

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
  convertNameToHs :: a -> HsEquivalent a

newtype UnqualName = Unqual QName
type instance HsEquivalent UnqualName = Name ()
instance ToHsName UnqualName where
  convertNameToHs (Unqual (_, s)) = Ident () $ escapeName s

type instance HsEquivalent QName = Hs.QName ()
instance ToHsName QName where
  convertNameToHs n@(m, _) = Hs.Qual () (ModuleName () m) (convertNameToHs (Unqual n))

defaultPragmas :: [ModulePragma ()]
defaultPragmas = []

defaultImports :: [ImportDecl ()]
defaultImports = []

-- |Compute the filename of the Haskell file for a source file
haskellName :: FilePath -> FilePath
haskellName = flip replaceExtension haskellExt

-- |Filename extension for Haskell files
haskellExt :: String
haskellExt = ".hs"

escapeName :: String -> String
escapeName = id

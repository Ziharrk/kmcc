{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Curry.ConvertToHs where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Coerce (coerce)
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
import Curry.FlatCurry.Typed.Type
import Curry.Files.Filenames (addOutDirModule)
import qualified CompilerOpts as Frontend
import CompilerOpts (Options(..))
import CurryBuilder (smake, compMessage)

import Curry.Analysis (NDAnalysisResult, NDInfo (..))
import Haskell.ExtsInstances ()
import Data.Char (toLower, toUpper)

newtype CM a = CM {
    runCM :: ReaderT NDAnalysisResult (ExceptT [Message] IO) a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO
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
        --tydsM <- mapM convertToMonadicHs tys
        --fundsM <- mapM convertToMonadicHs fs
        let visT = mapMaybe getVisT tys
        let visF = mapMaybe getVisF fs
        header <- convertToHs (MS (nm, visT, visF))
        let extract (Just (x, y)) = [x,y]
            extract Nothing = []
        --let ds = coerce (tyds ++ tydsM) ++ concatMap @[] extract (coerce (funds ++ fundsM))
        let ds = coerce tyds ++ concatMap @[] extract (coerce funds)
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


-- TODO export only what is visible
newtype ModuleHeadStuff = MS (String, [(QName, [QName])], [QName])
type instance HsEquivalent ModuleHeadStuff = ModuleHead ()
instance ToHs ModuleHeadStuff where
  convertToHs (MS (s, qnamesT, qnamesF)) = return $ ModuleHead () (ModuleName () ("Curry_" ++ s)) Nothing $
    Just (ExportSpecList () (map typeItem qnamesT ++ map funcItem qnamesF))
    where
      typeItem (qname, [])      = EAbs () (NoNamespace ()) (convertTypeNameToHs qname)
      typeItem (qname, csNames) = EThingWith () (NoWildcard ()) (convertTypeNameToHs qname) (map conItem csNames)

      conItem qname = ConName () (convertTypeNameToHs (Unqual qname))

      funcItem = EVar () . convertFuncNameToHs

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
  convertToMonadicHs (Type qname vis vs cs) = undefined
  convertToMonadicHs (TypeSyn qname _ vs texpr) = undefined
  convertToMonadicHs (TypeNew qname vis vs (NewCons cname cvis texpr)) = undefined

type instance HsEquivalent ConsDecl = QualConDecl ()
instance ToHs ConsDecl where
  convertToHs (Cons cname _ _ texprs) = do
    tys <- mapM convertToHs texprs
    return $ QualConDecl () Nothing Nothing (ConDecl () (convertTypeNameToHs (Unqual cname)) tys)

mkTypeHead :: QName -> [TVarWithKind] -> DeclHead ()
mkTypeHead qname vs = foldl (DHApp ()) (DHead () (convertTypeNameToHs (Unqual qname))) $ map toTyVarBndr vs

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
  convertToMonadicHs (TFunc qname ar vis texpr rule) = undefined

newtype RuleInfo = RI (QName, TRule)
type instance HsEquivalent RuleInfo = Match ()
instance ToHs RuleInfo where
  convertToHs (RI (qname, TRule args expr)) = do
    e <- convertToHs expr
    return $ Match () (convertFuncNameToHs (Unqual qname)) (map (PVar () . indexToName . fst) args)
      (UnGuardedRhs () e) Nothing
  convertToHs (RI (qname, TExternal _ str)) = return $
    Match () (convertFuncNameToHs (Unqual qname)) []
      (UnGuardedRhs () (Hs.Var () (UnQual () (Ident () (escapeFuncName unqualStr ++ "#"))))) Nothing
    where unqualStr = case dropWhile (/= '.') str of
            ""  -> str
            "." -> str
            _:s -> s

type instance HsEquivalent TExpr = Exp ()
instance ToHs TExpr where
  convertToHs (TVarE _ idx) = return $ Hs.Var () (UnQual () (indexToName idx))
  convertToHs (TLit _ lit) = return $ Hs.Lit () (convertLit lit)
  convertToHs (TComb _ ct qname args) = do
    args' <- mapM convertToHs args
    let convertNameToHs = case ct of
          ConsCall -> convertTypeNameToHs
          ConsPartCall _ -> convertTypeNameToHs
          FuncCall -> convertFuncNameToHs
          FuncPartCall _ -> convertFuncNameToHs
    return $ foldl (Hs.App ()) (Hs.Var () (convertNameToHs qname)) args'
  convertToHs (TLet bs e) = do
    e' <- convertToHs e
    bs' <- mapM (\((v, _), lclE) -> PatBind () (PVar () (indexToName v))
                     <$> (UnGuardedRhs () <$> convertToHs lclE)
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
  convertToHs (TPattern _ qname args) = return $ PApp () (convertTypeNameToHs qname) (map (PVar () . indexToName . fst) args)
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
  convertTypeNameToHs :: a -> HsEquivalent a
  convertFuncNameToHs :: a -> HsEquivalent a

newtype UnqualName = Unqual QName
type instance HsEquivalent UnqualName = Name ()
instance ToHsName UnqualName where
  convertTypeNameToHs (Unqual (_, s)) = Ident () $ escapeTypeName s
  convertFuncNameToHs (Unqual (_, s)) = Ident () $ escapeFuncName s

type instance HsEquivalent QName = Hs.QName ()
instance ToHsName QName where
  convertTypeNameToHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertTypeNameToHs (Unqual n))
  convertFuncNameToHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertFuncNameToHs (Unqual n))

defaultPragmas :: [ModulePragma ()]
defaultPragmas =
  [ LanguagePragma () [Ident () "NoImplicitPrelude"]
  , LanguagePragma () [Ident () "KindSignatures"]
  , LanguagePragma () [Ident () "TypeOperators"]
  , LanguagePragma () [Ident () "ExplicitForAll"]
  , OptionsPragma () (Just GHC) "-w " -- Space is important here
  ]

defaultImports :: [ImportDecl ()]
defaultImports =
  [ ImportDecl () (ModuleName () "Data.Kind") True False False Nothing Nothing Nothing
  , ImportDecl () (ModuleName () "BasicDefinitions") False False False Nothing Nothing Nothing
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

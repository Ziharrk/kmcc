{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Curry.ConvertToHs (compileToHs, haskellName) where

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
import CompilerOpts (Options(..))
import CurryBuilder (smake, compMessage)

import Options (KMCCOpts(..), dumpMessage)
import Curry.Analysis (NDAnalysisResult, NDInfo (..))
import Curry.Annotate (annotateND, isFunFree)
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
process kopts idx tprog m fn mi deps
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
            liftIO $ dumpMessage kopts $ "Read cached Haskell file:\n" ++ prettyPrint res
    compile = do
      status opts $ compMessage idx (11, 16) "Translating" m (fn, destFile)
      res <- convertToHs (TWFP (tprog, externalName fn, mi, kopts))
      let printed = prettyPrint res
      liftIO $ dumpMessage kopts $ "Generated Haskell file:\n" ++ printed
      liftIO $ writeUTF8File' (tgtDir (haskellName fn)) printed
      return ()

    opts = frontendOpts kopts

    tgtDir = addOutDirModule (optUseOutDir opts) (optOutDir opts) m

hsPrettyPrintMode :: PPHsMode
hsPrettyPrintMode = PPHsMode 2 2 2 2 2 2 2 False PPOffsideRule False

hsPrettyPrintStyle :: Style
hsPrettyPrintStyle = Style LeftMode 500 2

type family HsEquivalent a = hs | hs -> a

class ToHs a where
  convertToHs :: a -> CM (HsEquivalent a)

class ToFlatHs a where
  convertToFlatHs :: a -> CM (HsEquivalent a)

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
        fundsF <- mapM convertToFlatHs fs
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
        let ds = insts ++ coerce (tyds ++ tydsM) ++ concatMap @[] extract (coerce (funds ++ fundsF ++ fundsM))
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

      getVisC (Cons qname _ Public _) = Just qname
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
  let hasDetMain = EVar () (Qual () nm (Ident () "main")) `elem` es
  let mainExport = EVar () (Qual () nm (Ident () "main##"))

  (mainExpr, ds') <- case ty of
      TCons ("Prelude","IO") _
        | hasDetMain -> return (App () (Hs.Var () mainWrapperDetQualName) (Hs.Var () (Qual () nm (Ident () "main"))), ds)
        | otherwise  -> return (App () (Hs.Var () mainWrapperNDetQualName) (Hs.Var () (Qual () nm (Ident () "mainND"))), ds)
      _
        | hasDetMain -> return (App () (Hs.Var () exprWrapperDetQualName) (Hs.Var () (Qual () nm (Ident () "main"))), ds)
        | otherwise  -> do
          let findMainDecl [] = throwError $ return @[] $ Message NoSpanInfo $ text "Main function not found"
              findMainDecl ((FunBind _ [Match _ (Ident () "mainND") [] (UnGuardedRhs () e) Nothing]):bs) = return (e, bs)
              findMainDecl (b:bs) = second (b:) <$> findMainDecl bs
              findMainSig [] = throwError $ return @[] $ Message NoSpanInfo $ text "Main type signature not found"
              findMainSig ((TypeSig _ [Ident () "mainND"] mainTy):bs) = return (mainTy, bs)
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
          let mainNDDecl = FunBind () [Match () (Ident () "mainND") [] (UnGuardedRhs () mainNDExpr) Nothing]

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

genInstances :: TypeDecl -> [Decl ()]
genInstances (Type _ _ _ []) = []
genInstances (Type qname _ vs cs) =
  [hsShowFreeDecl | not (isListOrTuple qname)]
  ++
  map hsEquivDecl [0..length vs] ++
  [ shareableDecl, hsToDecl, hsFromDecl, hsNarrowableDecl
  , hsUnifiableDecl, hsPrimitiveDecl, hsNormalFormDecl, hsCurryDecl ]
  where
    hsEquivDecl arity = TypeInsDecl () (TyApp () (TyCon () hsEquivQualName)
      (foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
        (map (TyVar () . indexToName . fst) (take arity vs))))
      (foldl (TyApp ()) (TyCon () (convertTypeNameToHs qname))
        (map (TyApp () (TyCon () hsEquivQualName) . TyVar () . indexToName . fst) (take arity vs)))
    shareableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHApp () (IHCon () shareableQualName) (TyCon () curryQualName)) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (map mkShareMatch cs))])
    hsToDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () hsToQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkToMatch cs))])
    hsFromDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () hsFromQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkFromMatch cs))])
    hsNarrowableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () narrowableQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () [mkNarrowMatch]), InsDecl () (FunBind () (mapMaybe mkSameConstrMatch cs))])
    hsUnifiableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () unifiableQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkUnifyWithMatch cs ++ [unifyWithFailMatch])), InsDecl () (FunBind () (mapMaybe mkLazyUnifyMatch cs))])
    hsPrimitiveDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () primitiveQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      Nothing
    hsNormalFormDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () normalFormQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkNfWithMatch cs))])
    hsShowFreeDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () showFreeClassQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkShowsFreePrecMatch cs))])
    hsCurryDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () curryClassQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      Nothing
    mkShareMatch (Cons qname2 ar _ _) = Match () (Ident () "shareArgs")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () (mkShareImpl qname2 ar)) Nothing
    mkToMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "to")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (preventDict mkToImpl qname2 ar)
    mkFromMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "from")
      [PApp () (convertTypeNameToHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (preventDict mkFromImpl qname2 ar)
    mkNarrowMatch = Match () (Ident () "narrow") [] (UnGuardedRhs () (List () (mapMaybe mkNarrowExp cs))) Nothing
    mkNarrowExp (Cons qname2 ar _ _) = preventDict mkNarrowImpl qname2 ar
    mkSameConstrMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "narrowConstr")
      [PApp () (convertTypeNameToMonadicHs qname2) (replicate ar (PWildCard ()))]
      (UnGuardedRhs () e) Nothing) (preventDict mkSameConstrImpl qname2 ar)
    mkUnifyWithMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "unifyWith")
      [ PVar () (Ident () "_f")
      , PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () .  appendName "_a" . indexToName) [1..ar])
      , PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () .  appendName "_b" . indexToName) [1..ar])
      ]
      (UnGuardedRhs () e) Nothing) (preventDict mkUnifyWithImpl qname2 ar)
    unifyWithFailMatch = Match () (Ident () "unifyWith")
      [PWildCard (), PWildCard (), PWildCard ()]
      (UnGuardedRhs () mkFailed) Nothing
    mkLazyUnifyMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "lazyUnifyVar")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar]), PVar () (Ident () "_i")]
      (UnGuardedRhs () e) Nothing) (preventDict mkLazyUnifyImpl qname2 ar)
    mkNfWithMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "nfWith")
      [PVar () (Ident () "_f"), PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (preventDict mkNfWithImpl qname2 ar)
    mkShowsFreePrecMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "showsFreePrec")
      [PVar () (Ident () "_p"), PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (preventDict mkShowsFreePrecImpl qname2 ar)
    preventDict f qname2 ar
      | not ("_Dict#" `isPrefixOf` snd qname2) = Just (f qname2 ar)
      | otherwise = Nothing
    mkShareImpl qname2 ar
      | not ("_Dict#" `isPrefixOf` snd qname2) =
        mkApplicativeChain (Hs.Var () (convertTypeNameToMonadicHs qname2))
                           (map (mkShare . Hs.Var () . UnQual () . indexToName) [1..ar])
      | otherwise =
        mkReturn (foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
                   (map (Hs.Var () . UnQual () . indexToName) [1..ar]))
    mkToImpl qname2 ar =
      mkApplicativeChain (Hs.Var () (convertTypeNameToHs qname2))
                          (map (mkToHaskell . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkFromImpl qname2 ar =
      foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
        (map (mkFromHaskell . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkNarrowImpl qname2 ar =
      foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
        (map (const mkFree) [1..ar])
    mkSameConstrImpl qname2 ar = foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
      (replicate ar mkFree)
    mkUnifyWithImpl _ ar = Do () $ maybeAddReturnTrue $
      map (\i -> Qualifier () $ App () (App () (Hs.Var () (UnQual () (Ident () "_f")))
                    (Hs.Var () (UnQual () (appendName "_a" (indexToName i)))))
                    (Hs.Var () (UnQual () (appendName "_b" (indexToName i))))) [1..ar]
    mkLazyUnifyImpl qname2 ar = Do () $
      map (\i -> Generator () (PVar () (appendName "_s" (indexToName i))) (mkShare mkFree)) [1..ar] ++
      [Qualifier () $ mkAddToVarHeap (Hs.Var () (UnQual () (Ident () "_i"))) $ mkReturn
                        (foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
                        (map (Hs.Var () . UnQual () . indexToName) [1..ar]))] ++
      maybeAddReturnTrue (
      map (\i -> Qualifier () $ mkLazyUnify
                    (Hs.Var () (UnQual () (indexToName i)))
                    (Hs.Var () (UnQual () (appendName "_s" (indexToName i))))) [1..ar])
    mkNfWithImpl qname2 ar = mkApplicativeChain
      (mkLambda (map (PVar () . appendName "_l" . indexToName) [1..ar])
                (foldl (\c -> App () c . mkReturn . Hs.Var (). UnQual () . appendName "_l" . indexToName)
                       (Hs.Var () (convertTypeNameToMonadicHs qname2))
                       [1..ar]))
      (map (App () (Hs.Var () (UnQual () (Ident () "_f"))) . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkShowsFreePrecImpl qname2 0 = mkShowStringCurry (snd qname2)
    mkShowsFreePrecImpl qname2 ar
      | isOpQName qname2 =
        mkShowsBrackets (Hs.Var () (UnQual () (Ident () "_p")))
        (mkShowSpace (mkShowSpace (mkShowsCurryHighPrec (indexToName 1)) (mkShowStringCurry (snd qname2)))
          (mkShowsCurryHighPrec (indexToName 2)))
      | otherwise        =
        mkShowsBrackets (Hs.Var () (UnQual () (Ident () "_p")))
        (foldl1 mkShowSpace (mkShowStringCurry (snd qname2) : map (mkShowsCurryHighPrec . indexToName) [1..ar]))
    maybeAddReturnTrue [] = [Qualifier () $ mkReturn (Hs.Var () trueQualName)]
    maybeAddReturnTrue xs = xs
    mkLambda [] e = e
    mkLambda ps e = Paren () (Hs.Lambda () ps e)
genInstances TypeSyn {} = []
genInstances (TypeNew qname1 vis1 vs (NewCons qname2 vis2 ty)) =
  genInstances (Type qname1 vis1 vs [Cons qname2 1 vis2 [ty]])

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
          Just Det        -> [EVar () $ convertFuncNameToHs qname]
          Just FlatNonDet -> [EVar () $ convertFuncNameToHs qname, EVar () $ convertFuncNameToFlatHs qname]
          _               -> [])

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

type instance HsEquivalent TypeExpr = Type ()
instance ToHs TypeExpr where
  convertToHs (TVar idx) = return $ TyVar () (indexToName idx)
  convertToHs (FuncType t1 t2) = TyFun () <$> convertToHs t1 <*> convertToHs t2
  convertToHs (TCons qn tys) = foldl (TyApp ()) (TyCon () (convertTypeNameToHs qn)) <$> mapM convertToHs tys
  convertToHs (ForallType vs t) = TyForall () (Just $ map toTyVarBndr vs) Nothing <$> convertToHs t

instance ToFlatHs TypeExpr where
  convertToFlatHs (TVar idx) = return $ TyVar () (indexToName idx)
  convertToFlatHs (FuncType t1 t2) = TyFun () . mkCurry . mkCurry <$> convertToFlatHs t1 <*> convertToFlatHs t2
  convertToFlatHs (TCons qn tys) = foldl (TyApp ()) (TyCon () (convertTypeNameToFlatHs qn)) <$> mapM convertToFlatHs tys
  convertToFlatHs (ForallType vs t) = TyForall () (Just (map toTyVarBndr vs)) (mkCurryCtxt vs) <$> convertQualTypeWith convertToFlatHs t

instance ToMonadicHs TypeExpr where
  convertToMonadicHs (TVar idx) = return $ TyVar () (indexToName idx)
  convertToMonadicHs (FuncType t1 t2) = mkLiftedFunc <$> convertToMonadicHs t1 <*> convertToMonadicHs t2
  convertToMonadicHs (TCons qn tys) = foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qn)) <$> mapM convertToMonadicHs tys
  convertToMonadicHs (ForallType vs t) = TyForall () (Just (map toTyVarBndr vs)) (mkCurryCtxt vs) <$> convertQualTypeWith convertToFlatHs t

convertQualType :: TypeExpr -> CM (Type ())
convertQualType = convertQualTypeWith convertToMonadicHs

convertQualTypeWith :: (TypeExpr -> CM (Type ())) -> TypeExpr -> CM (Type ())
convertQualTypeWith f ty
  | ForallType _ _ <- ty = ty'
  | otherwise            = mkCurry <$> ty'
  where
    ty' = f ty

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

instance ToFlatHs TFuncDecl where
  convertToFlatHs (TFunc qname _ _ texpr rule) = do
    analysis <- ask
    case Map.lookup qname analysis of
      Just FlatNonDet -> do
        ty <- convertQualTypeWith convertToFlatHs (normalizeCurryType texpr)
        let tsig = TypeSig () [convertFuncNameToFlatHs (Unqual qname)] ty
        match <- convertToFlatHs (RI (qname, rule))
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
      (UnGuardedRhs () (Hs.Var () (UnQual () (Ident () (escapeFuncName unqualStr ++ "#"))))) Nothing
    where unqualStr = case dropWhile (/= '.') str of
            ""  -> str
            "." -> str
            _:s -> s

instance ToFlatHs RuleInfo where
  convertToFlatHs (RI (qname, TRule args expr)) = do
    analysis <- ask
    e' <- convertToFlatHs (annotateND analysis expr)
    return $ Match () (convertFuncNameToMonadicHs (Unqual qname)) (map (PVar () . indexToName . fst) args)
      (UnGuardedRhs () e') Nothing
  convertToFlatHs (RI (qname, TExternal _ str)) = return $
    Match () (convertFuncNameToMonadicHs (Unqual qname)) []
      (UnGuardedRhs () (Hs.Var () (UnQual () (Ident () (escapeFuncName unqualStr ++ "Flat#"))))) Nothing
    where unqualStr = case dropWhile (/= '.') str of
            ""  -> str
            "." -> str
            _:s -> s

instance ToMonadicHs RuleInfo where
  convertToMonadicHs (RI (qname, TRule args expr)) = do
    analysis <- ask
    e' <- convertToMonadicHs (annotateND analysis expr)
    let e'' = foldr (\v -> mkReturnFunc .  Lambda () [PVar () $ indexToName $ fst v]) e' args
    return $ Match () (convertFuncNameToMonadicHs (Unqual qname)) []
      (UnGuardedRhs () e'') Nothing
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
    (Hs.Var () (Qual () (ModuleName () "Curry_Prelude") (Ident () "failed")))) Nothing

instance ToFlatHs (AExpr (TypeExpr, NDInfo)) where
  convertToFlatHs (AVar _ idx) = return $ Hs.Var () (UnQual () (indexToName idx))
  convertToFlatHs (ALit _ lit) = return $ mkReturn $ Hs.Lit () (convertLit lit)
  convertToFlatHs ex@(AComb (ty, Det) FuncCall _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToFlatHs ex@(AComb (ty, Det) ConsCall  _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToFlatHs (AComb (ty, _) FuncCall  (_, (_, Det)) _)
    | isFunFree ty = undefined -- TODO
  convertToFlatHs (AComb _ ConsCall (qname, _) args) = do
    undefined
    -- args' <- mapM convertToFlatHs args -- TODO Constructor is not FlatNonDet
    -- return $ mkReturn $ foldl (App ()) (Hs.Var () (convertTypeNameToFlatHs qname)) args'
  convertToFlatHs (AComb _ (ConsPartCall missing) (qname, _) args) = do
    undefined
    -- args' <- mapM convertToFlatHs args
    -- missingVs <- replicateM missing freshVarName
    -- let mkLam e = foldr (\v -> mkReturnFunc .  Lambda () [PVar () v]) e missingVs
    -- return $ mkLam $ mkReturn $ foldl (App ()) (Hs.Var () (convertTypeNameToFlatHs qname)) (args' ++ map (Hs.Var () . UnQual ()) missingVs)
  convertToFlatHs (AComb _ _ (qname, _) args) = do -- (Partial) FuncCall
    undefined -- TODO
    -- args' <- mapM convertToFlatHs args
    -- return $ foldl mkMonadicApp (Hs.Var () (convertFuncNameToFlatHs qname)) args'
  convertToFlatHs ex@(ALet (ty, Det) _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToFlatHs ex@(ALet _ bs e) = do
    undefined -- TODO
    -- e' <- convertToFlatHs e
    -- bs' <- mapM (\((a, _), b) -> (indexToName a, , countVarUse ex a) <$> convertToFlatHs b) bs
    -- return $ mkShareLet e' bs'
  convertToFlatHs AFree {} = throwError [message "Encountered a free variable in an expression inferred to be flat non-deterministic"]
  convertToFlatHs AOr {} = throwError [message "Encountered an 'or' in an expression inferred to be flat non-deterministic"]
  convertToFlatHs ex@(ACase (ty, Det) _ _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToFlatHs (ACase (_, _) _  e bs) = do
    undefined -- TODO
    -- e' <- convertToFlatHs e
    -- bs' <- mapM convertToFlatHs bs
    -- return $ mkBind e' $ Hs.LCase () (bs' ++ [failedFlatBranch])
  convertToFlatHs ex@(ATyped (ty, Det) _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToFlatHs (ATyped _ e ty) = ExpTypeSig () <$> convertToFlatHs e <*> convertQualTypeWith convertToFlatHs ty

failedFlatBranch :: Alt ()
failedFlatBranch = failedMonadicBranch

instance ToMonadicHs (AExpr (TypeExpr, NDInfo)) where
  convertToMonadicHs (AVar _ idx) = return $ Hs.Var () (UnQual () (indexToName idx))
  convertToMonadicHs (ALit _ lit) = return $ mkReturn $ Hs.Lit () (convertLit lit)
  convertToMonadicHs ex@(AComb (ty, Det) FuncCall _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToMonadicHs ex@(AComb (ty, Det) ConsCall  _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToMonadicHs (AComb _ ConsCall (qname, _) args) = do
    args' <- mapM convertToMonadicHs args
    return $ mkReturn $ foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname)) args'
  convertToMonadicHs (AComb _ (ConsPartCall missing) (qname, _) args) = do
    args' <- mapM convertToMonadicHs args
    missingVs <- replicateM missing freshVarName
    let mkLam e = foldr (\v -> mkReturnFunc .  Lambda () [PVar () v]) e missingVs
    return $ mkLam $ mkReturn $ foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname)) (args' ++ map (Hs.Var () . UnQual ()) missingVs)
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
  convertToMonadicHs ex@(ACase (ty, Det) _ _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToMonadicHs (ACase (_, _) _  e bs) = do
    e' <- convertToMonadicHs e
    bs' <- mapM convertToMonadicHs bs
    return $ mkBind e' $ Hs.LCase () (bs' ++ [failedMonadicBranch])
  convertToMonadicHs ex@(ATyped (ty, Det) _ _)
    | isFunFree ty = mkFromHaskell <$> convertToHs ex
  convertToMonadicHs (ATyped _ e ty) = ExpTypeSig () <$> convertToMonadicHs e <*> convertQualType ty

failedMonadicBranch :: Alt ()
failedMonadicBranch = Alt () (PWildCard ())
  (UnGuardedRhs ()
    (Hs.Var () (Qual () (ModuleName () "Curry_Prelude") (Ident () "failedND")))) Nothing

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

class ToHsName a => ToFlatHsName a where
  convertTypeNameToFlatHs :: a -> HsEquivalent a
  convertTypeNameToFlatHs = convertTypeNameToHs
  convertFuncNameToFlatHs :: a -> HsEquivalent a

class ToFlatHsName a => ToMonadicHsName a where
  convertTypeNameToMonadicHs :: a -> HsEquivalent a
  convertFuncNameToMonadicHs :: a -> HsEquivalent a

newtype UnqualName = Unqual QName
type instance HsEquivalent UnqualName = Name ()
instance ToHsName UnqualName where
  convertTypeNameToHs (Unqual (_, s)) = Ident () $ escapeTypeName s ++ "D"
  convertFuncNameToHs (Unqual (_, s)) = Ident () $ escapeFuncName s ++ "D"

instance ToFlatHsName UnqualName where
  convertFuncNameToFlatHs (Unqual (_, s)) = Ident () $ escapeFuncName s ++ "Flat"

instance ToMonadicHsName UnqualName where
  convertTypeNameToMonadicHs (Unqual (_, s)) = Ident () $ escapeTypeName s ++ "ND"
  convertFuncNameToMonadicHs (Unqual (_, s)) = Ident () $ escapeFuncName s ++ "ND"

type instance HsEquivalent QName = Hs.QName ()
instance ToHsName QName where
  convertTypeNameToHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertTypeNameToHs (Unqual n))
  convertFuncNameToHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertFuncNameToHs (Unqual n))

instance ToFlatHsName QName where
  convertFuncNameToFlatHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertFuncNameToFlatHs (Unqual n))

instance ToMonadicHsName QName where
  convertTypeNameToMonadicHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertTypeNameToMonadicHs (Unqual n))
  convertFuncNameToMonadicHs n@(m, _) = Hs.Qual () (ModuleName () ("Curry_" ++ m)) (convertFuncNameToMonadicHs (Unqual n))

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

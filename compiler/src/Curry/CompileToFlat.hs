{-# LANGUAGE LambdaCase #-}
module Curry.CompileToFlat (getDependencies, compileFileToFcy, checkForMain, externalName, externalExt) where

import Control.Monad.IO.Class ( liftIO )
import Data.Bifunctor ( second )
import Data.Binary ( decodeFileOrFail, encodeFile)
import Data.List ( intercalate )
import Data.Maybe ( mapMaybe, fromMaybe, catMaybes )
import Data.Set ( Set, insert )
import System.FilePath ( (</>), (-<.>), replaceExtension )
import System.Directory ( doesFileExist )

import Curry.Frontend.Base.SCC (scc)
import Curry.Frontend.Base.Messages ( status )
import Control.Monad ( msum, void )
import Curry.Base.Monad ( CYIO )
import Curry.Base.Pretty ( Pretty(..) )
import Curry.Base.Ident ( ModuleIdent (..) )
import Curry.FlatCurry.Files ( readTypedFlatCurry )
import Curry.FlatCurry.Typed.Type ( TProg (..), TFuncDecl (..), TypeExpr (..)
                                  , QName, NewConsDecl(..), TypeDecl(..), ConsDecl (..) )
import Curry.Files.Filenames
import qualified Curry.Syntax.ShowModule as CS
import Curry.Frontend.CurryBuilder ( findCurry, processPragmas
                                   , adjustOptions, smake, compMessage)
import Curry.Frontend.CurryDeps ( flatDeps, Source(..) )
import Curry.Frontend.CompilerOpts ( Options(..), TargetType(..), DumpLevel (..)
                                   , DebugOpts (..), OptimizationOpts (..) )
import Curry.Frontend.Modules hiding ( compileModule )
import Curry.Frontend.Transformations ( qual )
import Curry.Frontend.Checks ( expandExports )
import Curry.Frontend.Generators ( genTypedFlatCurry, genAnnotatedFlatCurry
                                 , genFlatCurry )

import Curry.FrontendUtils ( runCurryFrontendAction, checkNewer )
import Options ( KMCCOpts (..), dumpMessage )

getDependencies :: KMCCOpts -> IO [(ModuleIdent, Source)]
getDependencies opts = do
  deps <- runCurryFrontendAction (frontendOpts opts) (findCurry (frontendOpts opts) (optTarget opts) >>= flatDeps (frontendOpts opts))
  dumpMessage opts $ "Dependencies:\n" ++ showDeps deps
  return deps

showDeps :: [(ModuleIdent, Source)] -> String
showDeps = concatMap showDeps'
  where
    showMid (ModuleIdent _ m) = intercalate "." m
    showDeps' (m, Source _ _ []) = showMid m ++ "\n"
    showDeps' (m, Source _ _ is) = let ms = showMid m
                                   in case map showMid is of
                                        []     -> ms
                                        (x:xs) -> ms ++ " -> " ++ x ++ "\n" ++ unlines (map (\x' -> replicate (length ms + 4) ' ' ++ x') xs)
    showDeps' (m, _) = showMid m

compileFileToFcy :: KMCCOpts -> [(ModuleIdent, Source)]
                 -> IO ([((TProg, Bool), ModuleIdent, FilePath)], Set QName, Set QName)
compileFileToFcy opts srcs = runCurryFrontendAction (frontendOpts opts) $ do
  res <- catMaybes <$> mapM process' (zip [1 ..] srcs)
  return (res, foldr insertNewtypes mempty res, foldr insertDataTypes mempty res)
  where
    total  = length srcs
    tgtDir = addOutDirModule (optUseOutDir (frontendOpts opts)) (optOutDir (frontendOpts opts))

    process' :: (Int, (ModuleIdent, Source)) -> CYIO (Maybe ((TProg, Bool), ModuleIdent, FilePath))
    process' (n, (m, Source fn ps is)) = do
      opts' <- processPragmas (frontendOpts opts) ps
      deps <- ((fn : mapMaybe curryInterface is)++) <$> liftIO getExternalDepFile
      res <- process (opts { frontendOpts = adjustOptions (n == total) opts' })
               (n, total) m fn deps
      return  $ Just (res, m, fn)
      where
        curryInterface i = case lookup i srcs of
          Just (Source    fn' _ _) -> Just $ tgtDir i $ interfName fn'
          Just (Interface fn'    ) -> Just $ tgtDir i $ interfName fn'
          _                        -> Nothing

        getExternalDepFile = let efp = externalName fn
          in doesFileExist efp >>= \case
                True  -> return [efp]
                False -> return []

    process' _ = return Nothing

    insertNewtypes ((TProg _ _ ds _ _, _), _, _) tcE = foldr insertNewtype tcE ds
      where
        insertNewtype (TypeNew _ _ _ (NewCons qname _ _)) = insert qname
        insertNewtype _ = id

    insertDataTypes ((TProg _ _ ds _ _, _), _, _) dataE =
      foldr add (insert funQName dataE) (scc typeName typeArgs ds)
      where
        add :: [TypeDecl] -> Set QName -> Set QName
        add xs s =
          if any (any (`elem` s) . typeArgs) xs
            then foldr (\t s' -> foldr insert s' (typeName t)) s xs
            else s
        typeName (Type qname _ _ _)    = [qname]
        typeName (TypeNew qname _ _ _) = [qname]
        typeName (TypeSyn qname _ _ _) = [qname]
        typeArgs (Type _ _ _ tes)                = concatMap typeArgs' tes
        typeArgs (TypeNew _ _ _ (NewCons _ _ t)) = typeArgs'' t
        typeArgs (TypeSyn _ _ _ t)               = typeArgs'' t
        typeArgs' (Cons _ _ _ tes) = concatMap typeArgs'' tes
        typeArgs'' (FuncType ty1 ty2) =
          funQName : typeArgs'' ty1 ++ typeArgs'' ty2
        typeArgs'' (TVar _) = []
        typeArgs'' (TCons qname tys) = qname : concatMap typeArgs'' tys
        typeArgs'' (ForallType _ ty) = typeArgs'' ty
        funQName = ("Prelude", "->")

-- |Compile a single source module to typed flat curry.
process :: KMCCOpts -> (Int, Int)
        -> ModuleIdent -> FilePath -> [FilePath] -> CYIO (TProg, Bool)
process kmccopts idx@(thisIdx,maxIdx) m fn deps
  | optForce opts = compile
  | otherwise     = smake (tgtDir (interfName fn) : destFiles) deps compile optCheck
  where
    skip = do
      status opts $ compMessage idx (11, 16) "Skipping" m (fn, destFile0)
      binaryOrNot <- liftIO $ checkNewer (tgtDir (typedBinaryFlatName fn)) (tgtDir (typedFlatName fn))
      if binaryOrNot
        then do
          eithRes <- liftIO $ decodeFileOrFail (tgtDir (typedBinaryFlatName fn))
          case eithRes of
            Left (_, err) -> do
              liftIO $ putStr $ unlines
                [ "Binary interface file is corrupted."
                , "For the file \"" ++ fn ++ "\"."
                , "Decoding failed with:"
                , err
                , "Retrying compilation from source..." ]
              compile
            Right res -> do
              if thisIdx == maxIdx
                then liftIO $ dumpMessage kmccopts $ "Read cached flat curry file:\n" ++ show res
                else liftIO $ dumpMessage kmccopts "Read cached flat curry file."
              return (res, False)
        else do
          mbRes <- liftIO $ readTypedFlatCurry (tgtDir (typedFlatName fn))
          case mbRes of
            Nothing -> do
              liftIO $ putStr $ unlines
                [ "Flat curry file is corrupted."
                , "For the file \"" ++ fn ++ "\"."
                , "Retrying compilation from source..." ]
              compile
            Just res -> do
              if thisIdx == maxIdx
                then liftIO $ dumpMessage kmccopts $ "Read cached flat curry file:\n" ++ show res
                else liftIO $ dumpMessage kmccopts "Read cached flat curry file."
              return (res, False)
    optCheck = do
      fileExists <- liftIO $ doesFileExist optInterface
      if not fileExists then compile else do
        ok <- liftIO compareOptions
        if ok then skip else compile
    compile = do
      status opts $ compMessage idx (11, 16) "Compiling" m (fn, destFile0)
      res <- compileModule opts m fn
      if thisIdx == maxIdx
        then liftIO $ dumpMessage kmccopts $ "Generated flat curry file:\n" ++ show res
        else liftIO $ dumpMessage kmccopts "Generated flat curry file."
      liftIO saveOptions

      return (res, True)

    optInterface = tgtDir (interfName fn) -<.> ".optint"

    compileOpts = let
      obl = optOptimizationBaseLevel kmccopts
      od  = optOptimizationDeterminism kmccopts
      in (obl, od)

    saveOptions = encodeFile optInterface compileOpts

    compareOptions = do
      eitherRes <- decodeFileOrFail optInterface
      case eitherRes of
        Left (_, err) -> do
          putStr $ unlines
            [ "Binary interface file is corrupted."
            , "For the file \"" ++ optInterface ++ "\"."
            , "Decoding failed with:"
            , err
            , "Retrying compilation from source..." ]
          return False
        Right oldOpts -> return $ oldOpts == compileOpts

    opts = frontendOpts kmccopts

    tgtDir = addOutDirModule (optUseOutDir opts) (optOutDir opts) m

    (destFile0, destFiles) =
      case [ gen fn | (t, gen) <- nameGens, t `elem` optTargetTypes opts] of
        [] -> error "No output destination found"
        (x:xs) -> (x, x:xs)
    nameGens  =
      [ (Tokens              , tgtDir . tokensName         )
      , (Comments            , tgtDir . commentsName       )
      , (Parsed              , tgtDir . sourceRepName      )
      , (FlatCurry           , tgtDir . flatName           )
      , (TypedFlatCurry      , tgtDir . typedFlatName      )
      , (TypedBinaryFlatCurry, tgtDir . typedBinaryFlatName)
      , (AnnotatedFlatCurry  , tgtDir . annotatedFlatName  )
      , (AbstractCurry       , tgtDir . acyName            )
      , (UntypedAbstractCurry, tgtDir . uacyName           )
      , (AST                 , tgtDir . astName            )
      , (ShortAST            , tgtDir . shortASTName       )
      , (Html                , const (fromMaybe "." (optHtmlDir opts) </> htmlName m))
      ]

compileModule :: Options -> ModuleIdent -> FilePath -> CYIO TProg
compileModule opts m fn = do
  mdl <- loadAndCheckModule opts m fn
  mdl' <- expandExports opts mdl
  writeTokens   opts (fst mdl)
  writeComments opts (fst mdl)
  writeParsed   opts mdl
  writeAST      opts (second void mdl)
  let qmdl = qual mdl
  writeHtml     opts qmdl
  writeShortAST opts (second void qmdl)
  qmdl' <- dumpWith opts CS.showModule pPrint DumpQualified $ qual mdl'
  -- generate interface file
  intf <- uncurry (exportInterface opts) qmdl'
  writeInterface opts (fst mdl') intf
  ((env, il), mdl'') <- transModule opts qmdl'
  -- never dump anything when writing the flat curry files.
  -- We do this manually in the next step.
  writeFlat (opts { optDebugOpts = (optDebugOpts opts) { dbDumpLevels = [] } }) env (snd mdl'') il
  let remIm = optRemoveUnusedImports (optOptimizations opts)
      afcy = genAnnotatedFlatCurry remIm env (snd mdl'') il
  genTypedFlatCurry . snd <$> dumpWith opts show (pPrint . genFlatCurry) DumpFlatCurry (env, afcy)

checkForMain :: [TProg] -> Maybe TypeExpr
checkForMain [] = Nothing
checkForMain xs = case last xs of
  TProg _ _ _ fs _ -> msum (map isMainFunc fs)
  where
    isMainFunc (TFunc (_, nm) _ _ ty _)
      | nm == "main" = Just ty
      | otherwise    = Nothing

-- |Compute the filename of the external definition file for a source file
externalName :: FilePath -> FilePath
externalName = flip replaceExtension externalExt

-- |Filename extension for external definition files
externalExt :: String
externalExt = ".kmcc.hs"

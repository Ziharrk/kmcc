module Curry.CompileToFlat (getDependencies, compileFileToFcy, checkForMain) where

import Control.Monad.IO.Class ( liftIO )
import Data.Bifunctor ( second )
import Data.Binary ( decodeFileOrFail)
import Data.Maybe ( mapMaybe, fromMaybe, catMaybes )
import System.FilePath ( (</>) )

import Base.Messages ( status )
import Control.Monad ( msum, void )
import Curry.Base.Monad ( CYIO )
import Curry.Base.Pretty ( Pretty(..) )
import Curry.Base.Ident ( ModuleIdent )
import Curry.FlatCurry.Typed.Type ( TProg (..), TFuncDecl (..), TypeExpr (..) )
import Curry.Files.Filenames
import qualified Curry.Syntax.ShowModule as CS
import CurryBuilder ( findCurry, processPragmas, adjustOptions, smake, compMessage)
import CurryDeps ( flatDeps, Source(..) )
import CompilerOpts ( Options(..), TargetType(..), DumpLevel (..) )
import Modules hiding ( compileModule )
import Transformations ( qual )
import Checks ( expandExports )
import Generators ( genTypedFlatCurry, genAnnotatedFlatCurry )

import Curry.FrontendUtils ( runCurryFrontendAction )
import Options (KMCCOpts (..), dumpMessage)

getDependencies :: KMCCOpts -> IO [(ModuleIdent, Source)]
getDependencies opts = do
  deps <- runCurryFrontendAction (frontendOpts opts) (findCurry (frontendOpts opts) (optTarget opts) >>= flatDeps (frontendOpts opts))
  dumpMessage opts $ "Dependencies: " ++ show deps
  return deps

compileFileToFcy :: KMCCOpts -> [(ModuleIdent, Source)]
                 -> IO [(TProg, ModuleIdent, FilePath)]
compileFileToFcy opts srcs = runCurryFrontendAction (frontendOpts opts) $
  catMaybes <$> mapM process' (zip [1 ..] srcs)
  where
    total  = length srcs
    tgtDir = addOutDirModule (optUseOutDir (frontendOpts opts)) (optOutDir (frontendOpts opts))

    process' :: (Int, (ModuleIdent, Source)) -> CYIO (Maybe (TProg, ModuleIdent, FilePath))
    process' (n, (m, Source fn ps is)) = do
      opts' <- processPragmas (frontendOpts opts) ps
      Just . (, m, fn) <$> process (opts { frontendOpts = adjustOptions (n == total) opts' }) (n, total) m fn deps
      where
        deps = fn : mapMaybe curryInterface is

        curryInterface i = case lookup i srcs of
          Just (Source    fn' _ _) -> Just $ tgtDir i $ interfName fn'
          Just (Interface fn'    ) -> Just $ tgtDir i $ interfName fn'
          _                        -> Nothing
    process' _ = return Nothing

-- |Compile a single source module to typed flat curry.
process :: KMCCOpts -> (Int, Int)
        -> ModuleIdent -> FilePath -> [FilePath] -> CYIO TProg
process kmccopts idx m fn deps
  | optForce opts = compile
  | otherwise     = smake (tgtDir (interfName fn) : destFiles) deps compile skip
  where
    skip = do
      status opts $ compMessage idx (11, 16) "Skipping" m (fn, head destFiles)
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
          liftIO $ dumpMessage kmccopts $ "Read cached flat curry file:\n" ++ show res
          return res
    compile = do
      status opts $ compMessage idx (11, 16) "Compiling" m (fn, head destFiles)
      res <- compileModule opts m fn
      liftIO $ dumpMessage kmccopts $ "Generated flat curry file:\n" ++ show res
      return res

    opts = frontendOpts kmccopts

    tgtDir = addOutDirModule (optUseOutDir opts) (optOutDir opts) m

    destFiles = [ gen fn | (t, gen) <- nameGens, t `elem` optTargetTypes opts]
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
  let intf = uncurry exportInterface qmdl'
  writeInterface opts (fst mdl') intf
  ((env, il), mdl'') <- transModule opts qmdl'
  writeFlat opts env (snd mdl'') il
  return $ genTypedFlatCurry $ genAnnotatedFlatCurry env (snd mdl'') il

checkForMain :: [TProg] -> Maybe TypeExpr
checkForMain [] = Nothing
checkForMain xs = case last xs of
  TProg _ _ _ fs _ -> msum (map isMainFunc fs)
  where
    isMainFunc (TFunc (_, nm) _ _ ty _)
      | nm == "main" = Just ty
      | otherwise    = Nothing

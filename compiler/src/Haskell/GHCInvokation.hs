module Haskell.GHCInvokation (invokeGHC) where

import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import System.Directory (findExecutable, copyFile, exeExtension)
import System.Environment (getExecutablePath)
import System.FilePath
    ( (</>),
      replaceBaseName,
      replaceDirectory,
      replaceExtension,
      takeFileName,
      takeDirectory,
      splitPath,
      joinPath,
      dropExtension )
import System.Process (callProcess)

import Curry.Frontend.CompilerOpts (Options(..))
import Curry.Frontend.CurryDeps (Source(..))
import Curry.Files.Filenames (addOutDirModule)
import Curry.Base.Ident (ModuleIdent(..))

import Options (KMCCOpts (..), SearchStrat(FS), debugMessage, statusMessage)
import Curry.ConvertToHs (haskellName)

invokeGHC :: Bool -> [(ModuleIdent, Source)] -> KMCCOpts -> IO ()
invokeGHC hasMain deps opts = do
  mbEx <- findExecutable "stack"
  execDir <- takeDirectory <$> getExecutablePath
  let topDir = joinPath (init (splitPath execDir))
  let targetFile = tgtDir (frontendOpts opts) (fst (last deps)) (haskellName $ optTarget opts)
  case mbEx of
    Nothing -> fail "Executable 'stack' not found. Please install the Haskell tool \"Stack\""
    Just p  -> do
      let args = stackInvokeGHCArgs execDir opts ++ stackPkgArgs ++
                   "--" : invokeGHCDefaultArgs ++ getGHCOptsFor topDir hasMain deps targetFile opts
      debugMessage opts $ "Invoking GHC via: stack " ++ unwords args
      callProcess p args
  when hasMain $ copyExecutable targetFile opts

stackInvokeGHCArgs :: FilePath -> KMCCOpts -> [String]
stackInvokeGHCArgs execDir KMCCOpts { optCompilerVerbosity, optProfiling } =
  ["--silent" | optCompilerVerbosity < 1] ++
  ["--profile" | optProfiling] ++
  [ "ghc", "--stack-yaml", execDir </> "stackForCurry.yaml"]

stackPkgArgs :: [String]
stackPkgArgs = concatMap (("--package":) . return)
  [ "base"
  , "ghc-prim"
  , "template-haskell"
  , "containers"
  , "deque"
  , "transformers"
  , "mtl"
  , "syb"
  , "extra"
  , "tree-monad"
  , "kan-extensions"
  , "sbv"
  , "adjunctions"
  , "deepseq"
  , "network-bsd"
  ]

invokeGHCDefaultArgs :: [String]
invokeGHCDefaultArgs =
  ["--make", "-threaded",
   "-with-rtsopts=-T"]  -- enables CPU time measurements

getGHCOptsFor :: FilePath -> Bool -> [(ModuleIdent, Source)] -> FilePath -> KMCCOpts -> [String]
getGHCOptsFor topDir hasMain deps targetFile
  KMCCOpts { frontendOpts, optCompilerVerbosity, optOptimizationBaseLevel
           , optProfiling, ghcOpts, optSearchStrategy } =
  ["-fforce-recomp" | optForce frontendOpts] ++
  ["-v" | optCompilerVerbosity > 3] ++
  ["-v0" | optCompilerVerbosity == 0] ++
  (if hasMain then ["-main-is", mainId] else []) ++
  ["-i " ++ topDir </> "rts"] ++
  getOptimizationOpts ghcOpts optOptimizationBaseLevel ++
  concat [["-with-rtsopts=-pa", "-prof", "-osuf p_o", "-fprof-auto"] | optProfiling ] ++
  ["-with-rtsopts=-single-threaded" | optSearchStrategy /= FS] ++
  ["-with-rtsopts=-N" | optSearchStrategy == FS] ++
  getGHCSrcDirOpts deps frontendOpts ++
  ghcOpts ++
  [takeFileName (dropExtension targetFile)]
  where
    mainId = case last deps of
      (ModuleIdent _ ms, _) -> "Curry_" ++ intercalate "." ms ++ ".main##"

getOptimizationOpts :: [String] -> Int -> [String]
getOptimizationOpts _ghcOps optOptimizationBaseLevel =
  [ "-O " ++ show optOptimizationBaseLevel] ++
  if optOptimizationBaseLevel < 2
    then ["-fno-float-in"]
    else ["-flate-dmd-anal"]

getGHCSrcDirOpts :: [(ModuleIdent, Source)] -> Options -> [String]
getGHCSrcDirOpts deps opts = mapMaybe (\(mid, src) -> case src of
  Source fp _ _ -> Just $ "-i " ++ dropModuleSubfolder mid (takeDirectory (tgtDir opts mid fp))
  Interface fp  -> Just $ "-i " ++ dropModuleSubfolder mid (takeDirectory (tgtDir opts mid fp))
  Unknown -> Nothing) deps
  where
    dropModuleSubfolder (ModuleIdent _ ms) d =
      let ds = splitPath d in
      joinPath (take (length ds - length ms + 1) ds)

tgtDir :: Options -> ModuleIdent -> FilePath -> FilePath
tgtDir opts = addOutDirModule (optUseOutDir opts) (optOutDir opts)

copyExecutable :: FilePath -> KMCCOpts -> IO ()
copyExecutable fp opts = do
  copyFile exeFile destinationFile
  statusMessage opts ("Copied executable to \"" ++ destinationFile ++ "\"")
  where
    destinationFile = replaceDirectory newBaseName "./" -- copy to current directory
    exeFile = replaceExtension fp exeExtension -- add executable extension (if platform requires it)
    newBaseName = replaceBaseName exeFile (drop 6 $ takeFileName $ dropHsExtension exeFile) -- remove Curry_

dropHsExtension :: FilePath -> FilePath
dropHsExtension = replaceExtension ""

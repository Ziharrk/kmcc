module Haskell.GHCInvokation (invokeGHC) where

import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import System.Directory (findExecutable, copyFile, exeExtension)
import System.FilePath (takeDirectory, replaceDirectory, replaceExtension, replaceBaseName, takeBaseName)
import System.Process (callProcess)

import CompilerOpts (Options(..))
import CurryDeps (Source(..))
import Curry.Files.Filenames (addOutDirModule)
import Curry.Base.Ident (ModuleIdent(..))

import Options (KMCCOpts (..), debugMessage, statusMessage)
import Curry.ConvertToHs (haskellName)

invokeGHC :: Bool -> [(ModuleIdent, Source)] -> KMCCOpts -> IO ()
invokeGHC hasMain deps opts = do
  mbEx <- findExecutable "stack"
  let targetFile = tgtDir (frontendOpts opts) (fst (last deps)) (haskellName (optTarget opts))
  case mbEx of
    Nothing -> fail "Executable 'stack' not found. Please install the Haskell tool \"Stack\""
    Just p  -> do
      let args = stackInvokeGHCArgs ++
                   "--" : invokeGHCDefaultArgs ++ getGHCOptsFor hasMain deps targetFile opts
      debugMessage opts $ "Invoking GHC with: " ++ unwords args
      callProcess p args
  when hasMain $ copyExecutable targetFile opts

stackInvokeGHCArgs :: [String]
stackInvokeGHCArgs = ["--silent", "--stack-yaml", "bin/stackForCurry.yaml" , "ghc"]

invokeGHCDefaultArgs :: [String]
invokeGHCDefaultArgs = ["--make"]

getGHCOptsFor :: Bool -> [(ModuleIdent, Source)] -> FilePath -> KMCCOpts -> [String]
getGHCOptsFor hasMain deps targetFile
  KMCCOpts { frontendOpts, optCompilerVerbosity, optOptimizationBaseLevel } =
  ["-fforce-recomp" | optForce frontendOpts] ++
  ["-v" | optCompilerVerbosity > 3] ++
  ["-v0" | optCompilerVerbosity == 0] ++
  (if hasMain then ["-main-is", mainId] else []) ++
  ["-i rts"] ++
  ["-O " ++ show optOptimizationBaseLevel] ++
  getGHCSrcDirOpts deps frontendOpts ++
  [takeBaseName targetFile]
  where
    mainId = case last deps of
      (ModuleIdent _ ms, _) -> "Curry_" ++ intercalate "." ms ++ ".main##"

getGHCSrcDirOpts :: [(ModuleIdent, Source)] -> Options -> [String]
getGHCSrcDirOpts deps opts = mapMaybe (\(mid, src) -> case src of
  Source fp _ _ -> Just $ "-i " ++ takeDirectory (tgtDir opts mid fp)
  Interface fp  -> Just $ "-i " ++ takeDirectory (tgtDir opts mid fp)
  Unknown -> Nothing) deps

tgtDir :: Options -> ModuleIdent -> FilePath -> FilePath
tgtDir opts = addOutDirModule (optUseOutDir opts) (optOutDir opts)

copyExecutable :: FilePath -> KMCCOpts -> IO ()
copyExecutable fp opts = do
  copyFile exeFile destinationFile
  statusMessage opts ("Copied executable to \"" ++ destinationFile ++ "\"")
  where
    destinationFile = replaceDirectory newBaseName "./" -- copy to current directory
    exeFile = replaceExtension fp exeExtension -- add executable extension (if platform requires it)
    newBaseName = replaceBaseName exeFile (drop 6 $ takeBaseName exeFile) -- remove Curry_
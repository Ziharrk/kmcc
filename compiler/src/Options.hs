module Options where

import Control.Monad ( when )
import qualified Data.Map as Map
import Data.Time ( NominalDiffTime )
import Data.Version ( showVersion )
import System.FilePath ( (</>) )

import CompilerOpts
    ( Options(..),
      defaultCppOpts,
      defaultOptions,
      CppOpts(cppDefinitions),
      TargetType(TypedBinaryFlatCurry) )

import Paths_kmcc ( version )

data KMCCOpts = KMCCOpts {
    optTarget :: FilePath,
    optCompileOnly :: Bool,
    optInfo :: [InfoCommand],
    optVarNames :: [(String, Int)],
    optShowBindings :: Bool,
    optCompilerVerbosity :: Int,
    optShowTimings :: Bool,
    optOptimizationBaseLevel :: Int,
    optOptimizationDeterminism :: Bool,
    optSearchStrategy :: SearchStrat,
    optProfiling :: Bool,
    optInteractive :: Bool,
    frontendOpts :: Options,
    ghcOpts :: [String]
  }

data InfoCommand = CompilerName | NumericVersion | BaseVersion
data SearchStrat = DFS | BFS | FS

-- Verbosity between 0 and 4. Default: 1
-- 0: no output except for errors
-- 1: status messages
-- 2: adds some debug output
-- 3: additionally dumps all intermediate files and formats to stdout
-- 4: passes -v to GHC as well

data ActionMode = Version | Compile

defaultOpts :: KMCCOpts
defaultOpts = KMCCOpts
  { optTarget = ""
  , optCompileOnly = False
  , optInfo = []
  , optVarNames = []
  , optShowBindings = False
  , optCompilerVerbosity = 1
  , optShowTimings = False
  , optOptimizationBaseLevel = 1
  , optOptimizationDeterminism = True
  , optSearchStrategy = FS
  , optProfiling = False
  , optInteractive = False
  , frontendOpts = defaultFrontendOpts
  , ghcOpts = []
  }

defaultFrontendOpts :: Options
defaultFrontendOpts = defaultOptions
  { optTargetTypes = [TypedBinaryFlatCurry]
  , optCppOpts = defaultCppOpts {
      cppDefinitions = Map.fromList [("__KMCC__", 1)]
    }
  , optOutDir = ".curry" </> ("kmcc-" ++ showVersion version)
  }

deriveBaseVersion :: String -> String
deriveBaseVersion "0.1.0" = "3.0.0"
deriveBaseVersion _       = "?.?.?"

statusMessage :: KMCCOpts -> String -> IO ()
statusMessage ops s = when (optCompilerVerbosity ops > 0) $ putStrLn s

debugMessage :: KMCCOpts -> String -> IO ()
debugMessage ops s = when (optCompilerVerbosity ops > 1) $ putStrLn s

dumpMessage :: KMCCOpts -> String -> IO ()
dumpMessage ops s = when (optCompilerVerbosity ops > 2) $ putStrLn s

timeMessage :: KMCCOpts -> String -> NominalDiffTime -> IO ()
timeMessage ops s t = when (optShowTimings ops) $ putStrLn $ s ++ ": " ++ show t

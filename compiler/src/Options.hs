module Options where

import Control.Monad ( when )
import qualified Data.Map as Map
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
    optInfo :: (Bool, Bool, Bool),
    optCompilerVerbosity :: Int, -- between 0 and 4, but 4 is only for GHC. Default: 1
    frontendOpts :: Options
  }

data ActionMode = Version | Compile

defaultOpts :: KMCCOpts
defaultOpts = KMCCOpts
  { optTarget = ""
  , optCompileOnly = False
  , optInfo = (False, False, False)
  , optCompilerVerbosity = 1
  , frontendOpts = defaultFrontendOpts
  }
-- missing:
-- optimization opts (O1/2 and turning on/of det-analysis)
-- verbosity, logging, quiet
-- debug

defaultFrontendOpts :: Options
defaultFrontendOpts = defaultOptions
  { optTargetTypes = [TypedBinaryFlatCurry]
  , optCppOpts = defaultCppOpts {
      cppDefinitions = Map.fromList [("__KICS2__", 4)]
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

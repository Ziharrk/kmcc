module Options where

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
    frontendOpts :: Options,
    optTarget :: FilePath
  }

data ActionMode = Version | Compile

defaultOpts :: KMCCOpts
defaultOpts = KMCCOpts
  { frontendOpts = defaultFrontendOpts
  , optTarget = ""
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

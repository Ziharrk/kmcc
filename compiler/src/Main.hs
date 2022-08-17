module Main where

import Data.Maybe ( isJust )
import Data.Time ( getCurrentTime, diffUTCTime )
import Data.Version ( showVersion )
import System.Environment ( getExecutablePath )
import System.FilePath ( (</>), splitPath, joinPath, takeDirectory )

import CompilerOpts ( Options(..) )
import Base.Utils ( fst3 )

import CmdParser ( getCmdOpts )
import Options ( KMCCOpts(..), InfoCommand (..), deriveBaseVersion, statusMessage, timeMessage )
import Curry.CompileToFlat ( getDependencies, compileFileToFcy, checkForMain )
import Curry.Analysis ( analyzeNondet )
import Curry.ConvertToHs ( compileToHs )
import Haskell.GHCInvokation ( invokeGHC )
import Paths_kmcc ( version )

main :: IO ()
main = do
  execDir <- takeDirectory <$> getExecutablePath
  let libDir = joinPath (init (splitPath execDir)) </> "lib/"
  let includeLibDir opt = opt
        { frontendOpts = (frontendOpts opt)
          { optLibraryPaths = libDir : optLibraryPaths (frontendOpts opt)
          , optImportPaths  = libDir : optImportPaths (frontendOpts opt)
          }
        }
  kmccopts <- includeLibDir <$> getCmdOpts
  case optInfo kmccopts of
    [] -> do
      firstTime <- getCurrentTime
      deps <- getDependencies kmccopts
      depsTime <- getCurrentTime
      timeMessage kmccopts "Time for dependency analysis" (diffUTCTime depsTime firstTime)
      statusMessage kmccopts "Compiling..."
      progs <- compileFileToFcy kmccopts deps
      compileTime <- getCurrentTime
      timeMessage kmccopts "Time for flat curry compilation" (diffUTCTime compileTime depsTime)
      statusMessage kmccopts "Analyzing..."
      let !mainType = if optCompileOnly kmccopts then Nothing else checkForMain (map fst3 progs)
      ndInfos <- analyzeNondet progs kmccopts
      analyzeTime <- getCurrentTime
      timeMessage kmccopts "Time for analysis" (diffUTCTime analyzeTime compileTime)
      statusMessage kmccopts "Converting to Haskell..."
      compileToHs mainType progs ndInfos kmccopts
      convertTime <- getCurrentTime
      timeMessage kmccopts "Time for conversion to Haskell" (diffUTCTime convertTime analyzeTime)
      statusMessage kmccopts "Invoking GHC..."
      invokeGHC (isJust mainType) deps kmccopts
      ghcTime <- getCurrentTime
      timeMessage kmccopts "Time for GHC invokation" (diffUTCTime ghcTime convertTime)
      timeMessage kmccopts "Timne total" (diffUTCTime ghcTime firstTime)
    xs -> mapM_ printVersionOpt xs
  where
    printVersionOpt CompilerName   = putStrLn "kmcc"
    printVersionOpt NumericVersion = putStrLn (showVersion version)
    printVersionOpt BaseVersion    = putStrLn (deriveBaseVersion (showVersion version))

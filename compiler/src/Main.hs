module Main where

import Data.Char ( isSpace )
import Data.Maybe ( isJust )
import qualified Data.Map as Map
import Data.Time ( getCurrentTime, diffUTCTime )
import Data.Version ( showVersion )
import System.Environment ( getExecutablePath )
import System.FilePath ( (</>), splitPath, joinPath, takeDirectory )

import Curry.Frontend.CompilerOpts ( Options(..) )
import Curry.Frontend.Base.Utils ( fst3 )

import CmdParser ( getCmdOpts )
import Options ( KMCCOpts(..), InfoCommand (..), statusMessage, timeMessage, debugMessage )
import Curry.CompileToFlat ( getDependencies, compileFileToFcy, checkForMain )
import Curry.Analysis ( analyzeNondet, NDInfo (..) )
import Curry.ConvertToHs ( compileToHs )
import Haskell.GHCInvokation ( invokeGHC )
import Paths_kmcc ( version )

main :: IO ()
main = do
  execDir <- takeDirectory <$> getExecutablePath
  let libDir = joinPath (init (splitPath execDir)) </> "libs/src"
  let includeLibDir opt = opt
        { frontendOpts = (frontendOpts opt)
          { optLibraryPaths = libDir : optLibraryPaths (frontendOpts opt)
          , optImportPaths  = libDir : optImportPaths (frontendOpts opt)
          }
        }
      printVersionOpt CompilerName   = putStrLn "kmcc"
      printVersionOpt NumericVersion = putStrLn (showVersion version)
      printVersionOpt BaseVersion    = do
        readFile (libDir </> ".." </> "VERSION") >>= putStrLn . reverse . dropWhile isSpace . reverse
  kmccopts <- includeLibDir <$> getCmdOpts
  case optInfo kmccopts of
    [] -> do
      firstTime <- getCurrentTime
      deps <- getDependencies kmccopts
      depsTime <- getCurrentTime
      timeMessage kmccopts "Time for dependency analysis" (diffUTCTime depsTime firstTime)
      statusMessage kmccopts "Compiling..."
      (progs, tcEnv, dataEnv) <- compileFileToFcy kmccopts deps
      compileTime <- getCurrentTime
      timeMessage kmccopts "Time for flat curry compilation" (diffUTCTime compileTime depsTime)
      statusMessage kmccopts "Analyzing..."
      let !mainType = if optCompileOnly kmccopts then Nothing else checkForMain (map (fst . fst3) progs)
      ndInfos <- analyzeNondet progs kmccopts
      analyzeTime <- getCurrentTime
      timeMessage kmccopts "Time for analysis" (diffUTCTime analyzeTime compileTime)
      debugMessage kmccopts "ND Functions:"
      debugMessage kmccopts "----------------"
      let relevantNDFuns =
            filter (elem '#' . snd) (Map.keys (Map.filter (== NonDet) ndInfos))
      debugMessage kmccopts (show relevantNDFuns)
      statusMessage kmccopts "Converting to Haskell..."
      compileToHs mainType progs ndInfos tcEnv dataEnv kmccopts
      convertTime <- getCurrentTime
      timeMessage kmccopts "Time for conversion to Haskell" (diffUTCTime convertTime analyzeTime)
      statusMessage kmccopts "Invoking GHC..."
      invokeGHC (isJust mainType) deps kmccopts
      ghcTime <- getCurrentTime
      timeMessage kmccopts "Time for GHC invokation" (diffUTCTime ghcTime convertTime)
      timeMessage kmccopts "Time total" (diffUTCTime ghcTime firstTime)
    xs -> mapM_ printVersionOpt xs

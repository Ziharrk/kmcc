module Main where

import Control.Monad ( when )
import Data.Maybe ( isJust )
import Data.Version ( showVersion )
import System.Environment ( getExecutablePath )
import System.FilePath ( (</>), splitPath, joinPath, takeDirectory )

import CompilerOpts ( Options(..) )
import Base.Utils ( fst3 )

import CmdParser ( getCmdOpts )
import Options ( KMCCOpts(..), deriveBaseVersion, statusMessage )
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
    (cn, cv, bv)
      | not (or [cn, cv, bv]) -> do
        deps <- getDependencies kmccopts
        statusMessage kmccopts "Compiling..."
        progs <- compileFileToFcy kmccopts deps
        statusMessage kmccopts "Analyzing..."
        let mainType = if optCompileOnly kmccopts then Nothing else checkForMain (map fst3 progs)
        ndInfos <- analyzeNondet progs kmccopts
        statusMessage kmccopts "Converting to Haskell..."
        compileToHs mainType progs ndInfos kmccopts
        statusMessage kmccopts "Invoking GHC..."
        invokeGHC (isJust mainType) deps kmccopts
      | otherwise -> do
        when cn $ putStrLn "kmcc"
        when cv $ putStrLn (showVersion version)
        when bv $ putStrLn (deriveBaseVersion (showVersion version))

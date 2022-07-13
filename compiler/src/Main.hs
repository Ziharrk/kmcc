module Main where

import System.Environment ( getExecutablePath )
import System.FilePath ( (</>), splitPath, joinPath, takeDirectory )

import CompilerOpts ( Options(..) )
import Base.Utils ( fst3 )

import CmdParser ( getCmdOpts )
import Options ( KMCCOpts(..) )
import Curry.CompileToFlat ( getDependencies, compileFileToFcy, checkForMain )
import Curry.Analysis ( analyzeNondet )
import Curry.ConvertToHs ( compileToHs )
import Haskell.GHCInvokation ( invokeGHC )

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
  let opts = frontendOpts kmccopts
  deps <- getDependencies (optTarget kmccopts) opts
  putStrLn "Compiling..."
  progs <- compileFileToFcy opts deps
  putStrLn "Analyzing..."
  let hasMain = checkForMain (map fst3 progs)
  ndInfos <- analyzeNondet progs opts
  putStrLn "Converting to Haskell..."
  _ <- compileToHs hasMain progs ndInfos opts
  putStrLn "Invoking GHC..."
  invokeGHC hasMain deps kmccopts

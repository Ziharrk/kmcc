module Main where

import qualified Data.Map as Map ( fromList )

import CompilerOpts
    ( Options(..),
      defaultCppOpts,
      defaultOptions,
      CppOpts(..),
      TargetType(..) )
import Curry.CompileToFlat ( getDependencies, compileFileToFcy )
import Curry.Analysis ( analyzeNondet )
import Curry.ConvertToHs ( compileToHs )

main :: IO ()
main = do
  deps <- getDependencies "Prelude" opts
  progs <- compileFileToFcy opts deps
  ndInfos <- analyzeNondet progs opts
  _ <- compileToHs progs ndInfos opts
  return ()
  where
    opts = defaultOptions { optTargetTypes = [TypedBinaryFlatCurry]
                          , optCppOpts = defaultCppOpts {
                               cppDefinitions = Map.fromList [("__KICS2__", 4)]
                            }
                          , optForce = True
                          }

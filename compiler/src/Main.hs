module Main where

import CompilerOpts (Options(..), TargetType (..), defaultOptions)

import Curry.CompileToFlat ( getDependencies, compileFileToFcy )
import Curry.Analysis ( analyzeNondet )
import Curry.ConvertToHs (compileToHs)

main :: IO ()
main = do
  deps <- getDependencies "Prelude" opts
  progs <- compileFileToFcy opts deps
  ndInfos <- undefined analyzeNondet progs opts
  _ <- compileToHs
  return ()
  where
    opts = defaultOptions { optTargetTypes = [TypedBinaryFlatCurry] }

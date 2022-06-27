module Main where

import CompilerOpts (Options(..), TargetType (..), defaultOptions)

import Curry.CompileToFlat ( getDependencies, compileFileToFcy )
import Curry.Analysis ( analyzeNondet )
import Curry.ConvertToHs (compileToHs)

main :: IO ()
main = do
  deps <- getDependencies "Prelude" opts
  progs <- compileFileToFcy opts deps
  ndInfos <- analyzeNondet progs opts
  _ <- compileToHs progs ndInfos opts
  return ()
  where
    opts = defaultOptions { optTargetTypes = [TypedBinaryFlatCurry] }

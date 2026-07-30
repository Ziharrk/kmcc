import Control.Monad ( forM_ )
import Data.Binary ( decodeFileOrFail )
import Data.Char ( isSpace, isUpper )
import Data.List ( partition )
import qualified Data.Map as Map
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.FilePath ( takeExtension, splitFileName, splitDirectories, (</>), joinPath )

import Curry.Files.Filenames ( curryExt, ensureOutDir )

import Curry.Analysis ( NDAnalysisResult, analysisExt, analysisName )
import Options ( defaultOutDir )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath]
      | takeExtension filePath == analysisExt -> analyze filePath
      | takeExtension filePath == curryExt -> analyze (withOutDir (analysisName filePath))
    _ -> do
      putStrLn "Usage: analysis-viewer <analysis-file (*.an) or curry source file (*.curry)>"
      exitFailure

withOutDir :: FilePath -> FilePath
withOutDir fp = joinPath prefix </> ensureOutDir defaultOutDir (joinPath curryDirs) </> base
  where
    (curryDirs, prefix) = partition (all isUpper . take 1) (splitDirectories dir)
    (dir, base) = splitFileName fp

analyze :: [Char] -> IO ()
analyze filePath = do
  putStrLn $ "Loading analysis from: " ++ filePath
  analysis <- readAnalysis filePath
  putStrLn "Analysis loaded: "
  putStrLn "==============================="
  forM_ (Map.toList analysis) $ \(qname, ndInfo) -> do
    putStrLn $ show qname ++ " : " ++ show ndInfo

readAnalysis :: FilePath -> IO NDAnalysisResult
readAnalysis filePath = do
  eithRes <- decodeFileOrFail (dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse filePath)
  case eithRes of
    Left (_, err) -> do
      putStrLn $ "Error reading analysis file: " ++ err
      exitFailure
    Right analysis -> return analysis

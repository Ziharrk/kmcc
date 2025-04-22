module Curry.FrontendUtils where

import System.Directory ( doesFileExist, getModificationTime )

import Curry.Base.Monad ( runCYIO, CYIO )
import Curry.Frontend.Base.Messages ( warnOrAbort, abortWithMessages )
import qualified Curry.Frontend.CompilerOpts as Frontend
import Curry.Frontend.CompilerOpts ( Options(..) )

runCurryFrontendAction :: Frontend.Options -> CYIO a -> IO a
runCurryFrontendAction opts act =
  runCYIO act >>= \(e, msgs) -> either abortWithMessages (continueWithMessages msgs) e
  where
    continueWithMessages msgs results =
      warnOrAbort (optWarnOpts opts) msgs >> return results

checkNewer :: FilePath -> FilePath -> IO Bool
checkNewer file1 file2 = do
  exists1 <- doesFileExist file1
  exists2 <- doesFileExist file2
  if exists1 && exists2
    then do
      t1 <- getModificationTime file1
      t2 <- getModificationTime file2
      return $ t1 >= t2
    else return True

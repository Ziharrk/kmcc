{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified Data.Time.Clock.POSIX as D
import qualified System.Directory as S
import Data.Curry_Time
import BasicDefinitions

directorydotprimuscoredoesFileExist_Det# = liftForeign1 S.doesFileExist
directorydotprimuscoredoesFileExist_ND# = liftConvertIO1 directorydotprimuscoredoesFileExist_Det#

directorydotprimuscoredoesDirectoryExist_Det# = liftForeign1 S.doesDirectoryExist
directorydotprimuscoredoesDirectoryExist_ND# = liftConvertIO1 directorydotprimuscoredoesDirectoryExist_Det#

directorydotprimuscorefileSize_Det# = liftForeign1 S.getFileSize
directorydotprimuscorefileSize_ND# = liftConvertIO1 directorydotprimuscorefileSize_Det#

directorydotprimuscoregetModificationTime_Det# x = do
  modTime <- S.getModificationTime (toForeign x)
  let posixTime = D.utcTimeToPOSIXSeconds modTime
  P.return (fromForeign posixTime)
directorydotprimuscoregetModificationTime_ND# = liftConvertIO1 directorydotprimuscoregetModificationTime_Det#

directorydotgetCurrentDirectory_Det# = fromForeign S.getCurrentDirectory
directorydotgetCurrentDirectory_ND# = P.return (P.fmap from directorydotgetCurrentDirectory_Det#)

directorydotprimuscoresetCurrentDirectory_Det# = liftForeign1 S.setCurrentDirectory
directorydotprimuscoresetCurrentDirectory_ND# = liftConvertIO1 directorydotprimuscoresetCurrentDirectory_Det#

directorydotprimuscoregetDirectoryContents_Det# = liftForeign1 S.getDirectoryContents
directorydotprimuscoregetDirectoryContents_ND# = liftConvertIO1 directorydotprimuscoregetDirectoryContents_Det#

directorydotprimuscorecreateDirectory_Det# = liftForeign1 S.createDirectory
directorydotprimuscorecreateDirectory_ND# = liftConvertIO1 directorydotprimuscorecreateDirectory_Det#

directorydotprimuscoreremoveDirectory_Det# = liftForeign1 S.removeDirectory
directorydotprimuscoreremoveDirectory_ND# = liftConvertIO1 directorydotprimuscoreremoveDirectory_Det#

directorydotprimuscorerenameDirectory_Det# = liftForeign2 S.renameDirectory
directorydotprimuscorerenameDirectory_ND# = liftConvertIO2 directorydotprimuscorerenameDirectory_Det#

directorydotprimuscoreremoveFile_Det# = liftForeign1 S.removeFile
directorydotprimuscoreremoveFile_ND# = liftConvertIO1 directorydotprimuscoreremoveFile_Det#

directorydotprimuscorerenameFile_Det# = liftForeign2 S.renameFile
directorydotprimuscorerenameFile_ND# = liftConvertIO2 directorydotprimuscorerenameFile_Det#
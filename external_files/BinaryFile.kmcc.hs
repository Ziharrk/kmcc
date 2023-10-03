{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified Data.ByteString as B
import qualified System.IO as S
import qualified Data.Char as D
import System.Curry_IO
import BasicDefinitions

iOdotBinaryFiledotprimuscoreopenBinaryFile_Det# path mode = do
  handle <- S.openBinaryFile (toForeign path) (toForeign mode)
  P.return (SingleHandle handle)
iOdotBinaryFiledotprimuscoreopenBinaryFile_ND# = liftConvertIO2 iOdotBinaryFiledotprimuscoreopenBinaryFile_Det#

iOdotBinaryFiledotprimuscorehGetByte_Det# h = do
  byte <- S.hGetChar (readHandle h)
  P.return (P.toInteger (D.ord byte))
iOdotBinaryFiledotprimuscorehGetByte_ND# = liftConvertIO1 iOdotBinaryFiledotprimuscorehGetByte_Det#

iOdotBinaryFiledotprimuscorehPutByte_Det# h byte = fromForeign P.$ do
  S.hPutChar (writeHandle h) (D.chr (P.fromInteger byte))
iOdotBinaryFiledotprimuscorehPutByte_ND# = liftConvertIO2 iOdotBinaryFiledotprimuscorehPutByte_Det#
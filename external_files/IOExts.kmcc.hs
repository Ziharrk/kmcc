{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified System.Process as S
import BasicDefinitions

iOExtsdotprimuscoreexecCmd_Det# s = do
  (P.Just stdin, P.Just stdout, P.Just stderr, _) <- S.createProcess (S.shell (toForeign s))
  P.return P.$ CTuple3_Det stdin stdout stderr
iOExtsdotprimuscoreexecCmd_ND# = liftConvertIO1 iOExtsdotprimuscoreexecCmd_Det#

iOExtsdotprimuscoreconnectToCmd_Det# s = P.error "No implementation of connectToCmd_Det"
iOExtsdotprimuscoreconnectToCmd_ND# = P.error "No implementation of connectToCmd_ND"
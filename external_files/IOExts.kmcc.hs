{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified System.Process as S
import Control.Concurrent (forkIO)
import qualified System.IO as S
import BasicDefinitions
import System.Curry_IO

iOExtsdotprimuscoreexecCmd_Det# s = do
  (P.Just stdin, P.Just stdout, P.Just stderr, _) <- S.createProcess (S.shell (toForeign s)){S.std_in = S.CreatePipe,
                                                                                             S.std_out = S.CreatePipe,
                                                                                             S.std_err = S.CreatePipe}
  P.return P.$ CTuple3_Det (SingleHandle stdin) (SingleHandle stdout) (SingleHandle stderr)
iOExtsdotprimuscoreexecCmd_ND# = liftConvertIO1 iOExtsdotprimuscoreexecCmd_Det#

iOExtsdotprimuscoreconnectToCmd_Det# s = do
  (P.Just stdin, P.Just stdout, P.Just stderr, _) <- S.createProcess (S.shell (toForeign s)){S.std_in = S.CreatePipe,
                                                                                             S.std_out = S.CreatePipe,
                                                                                             S.std_err = S.CreatePipe}
  forkIO (forwardError stderr)
  P.return P.$ DualHandle stdout stdin
iOExtsdotprimuscoreconnectToCmd_ND# = liftConvertIO1 iOExtsdotprimuscoreconnectToCmd_Det#

forwardError :: S.Handle -> P.IO ()
forwardError h = do
   eof <- S.hIsEOF h
   if eof then P.return ()
          else S.hGetLine h P.>>= S.hPutStrLn S.stderr P.>> forwardError h
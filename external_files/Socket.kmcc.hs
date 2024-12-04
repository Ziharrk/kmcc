{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

import qualified Prelude as P
import Prelude ((*))
import qualified Network.Socket as N
import qualified System.IO as S
import qualified System.Timeout as S
import BasicDefinitions
import System.Curry_IO

type instance HsEquivalent N.Socket = N.Socket

instance ToHs N.Socket where
  to = P.return

instance FromHs N.Socket where
  from = P.id
  elimFlat = P.id

instance ShowTerm N.Socket where
  showTerm _ _ = P.showString "<<Socket>>"

instance ReadTerm N.Socket where
  readTerm = P.error "reading a Socket is not possible"

instance ShowFree N.Socket where
  showsFreePrec _ _ = showsStringCurry "<<Socket>>"

instance NormalForm N.Socket where
  nfWith _ !x = P.return (P.Right x)

instance Narrowable N.Socket where
  narrow = P.error "narrowing a Socket is not possible"
  narrowConstr = P.error "narrowing a Socket is not possible"

instance HasPrimitiveInfo N.Socket where
  primitiveInfo = NoPrimitive

instance Unifiable N.Socket where
  unifyWith _ _ _ = P.error "unifying a Socket is not possible"

  lazyUnifyVar _ _ = P.error "unifying a Socket is not possible"

instance NFDataC N.Socket where
  rnfC !_ = ()

instance Curryable N.Socket

type Socket_Det# = N.Socket
type Socket_ND# = N.Socket


listenOn :: P.Integer -> P.IO N.Socket
listenOn port = do
  let hints = N.defaultHints{N.addrFlags = [N.AI_PASSIVE], N.addrFamily = N.AF_INET}
  (addr:_) <- N.getAddrInfo (P.Just hints) P.Nothing (P.Just (P.show (toForeign port)))
  sock <- N.openSocket addr
  N.setSocketOption sock N.ReuseAddr 1
  N.bind sock (N.addrAddress addr)
  N.listen sock N.maxListenQueue
  P.return sock

socketdotprimuscorelistenOn_Det# = listenOn
socketdotprimuscorelistenOn_ND# = liftConvertIO1 socketdotprimuscorelistenOn_Det#

socketdotlistenOnFresh_Det# = do
  sock <- listenOn 0
  port <- N.socketPort sock
  P.return P.$ CTuple2_Det (P.toInteger port) sock
socketdotlistenOnFresh_ND# = P.return (P.fmap from socketdotlistenOnFresh_Det#)

socketdotprimuscoresocketAccept_Det# sock = do
  (conn, addr) <- N.accept sock
  handle <- N.socketToHandle conn S.ReadWriteMode
  P.return P.$ CTuple2_Det (fromForeign (P.show addr)) (SingleHandle handle)
socketdotprimuscoresocketAccept_ND# = liftConvertIO1 socketdotprimuscoresocketAccept_Det#

socketdotprimuscorewaitForSocketAccept_Det# sock timeout = do
  let ioAction = socketdotprimuscoresocketAccept_Det# sock
  let timer = (P.fromInteger timeout) * 1000
  res <- S.timeout timer ioAction
  case res of
    P.Nothing -> P.return Nothing_Det
    P.Just x -> P.return (Just_Det x)
socketdotprimuscorewaitForSocketAccept_ND# = liftConvertIO2 socketdotprimuscorewaitForSocketAccept_Det#

socketdotprimuscoresClose_Det# sock = fromForeign (N.close sock)
socketdotprimuscoresClose_ND# = liftConvertIO1 socketdotprimuscoresClose_Det#

socketdotprimuscoreconnectToSocket_Det# host port = do
  let hints = N.defaultHints {N.addrFamily = N.AF_INET}
  addr:_ <- N.getAddrInfo P.Nothing (P.Just (toForeign host)) (P.Just (P.show port))
  sock <- N.openSocket addr
  N.connect sock (N.addrAddress addr)
  handle <- N.socketToHandle sock S.ReadWriteMode
  P.return (SingleHandle handle)
socketdotprimuscoreconnectToSocket_ND# = liftConvertIO2 socketdotprimuscoreconnectToSocket_Det#

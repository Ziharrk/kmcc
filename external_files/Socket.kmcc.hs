{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

import qualified Prelude as P
import qualified Network.Socket as N
import BasicDefinitions

type instance HsEquivalent N.Socket = N.Socket

instance ToHs N.Socket where
  to = P.return

instance FromHs N.Socket where
  from = P.id
  
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
  
instance Curryable N.Socket

type Socket_Det# = N.Socket
type Socket_ND# = N.Socket


createSocket :: P.Integer -> P.IO N.Socket
createSocket port = do
  let hints = N.defaultHints{N.addrFlags = [N.AI_PASSIVE], N.addrFamily = N.AF_INET}
  (sockInfo:_) <- N.getAddrInfo (P.Just hints) P.Nothing (P.Just (P.show (toForeign port)))
  sock <- N.openSocket sockInfo
  N.setSocketOption sock N.ReuseAddr 1
  N.bind sock (N.addrAddress sockInfo)
  N.listen sock N.maxListenQueue
  P.return sock
  
socketdotprimuscorelistenOn_Det# = createSocket
socketdotprimuscorelistenOn_ND# = P.error ""

socketdotlistenOnFresh_Det# = do
  sock <- createSocket 0
  port <- N.socketPort sock
  P.return P.$ CTuple2_Det (P.toInteger port) sock
socketdotlistenOnFresh_ND# = P.error ""

socketdotprimuscoresocketAccept_Det# = P.error "No implementation of socketAccept_Det"
socketdotprimuscoresocketAccept_ND# = P.error "No implementation of socketAccept_ND"

socketdotprimuscorewaitForSocketAccept_Det# = P.error "No implementation of waitForSocketAccept_Det"
socketdotprimuscorewaitForSocketAccept_ND# = P.error "No implementation of waitForSocketAccept_Det"

socketdotprimuscoresClose_Det# = P.error "No implementation of sClose_Det"
socketdotprimuscoresClose_ND# = P.error "No implementation of sClose_ND"

socketdotprimuscoreconnectToSocket_Det# = P.error "No implementation of connectToSocket_Det"
socketdotprimuscoreconnectToSocket_ND# = P.error "No implementation of connectToSocket_ND"
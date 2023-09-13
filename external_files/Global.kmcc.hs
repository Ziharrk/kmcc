{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified Data.IORef as D
import qualified Data.Map as D
import qualified System.IO.Unsafe as S
import BasicDefinitions

type GlobalStore = D.Map P.String Untyped

globals :: D.IORef GlobalStore
globals = S.unsafePerformIO P.$ D.newIORef (D.empty)

globaldotprimuscoreglobalT_Det# s v = S.unsafePerformIO P.$ do
  store <- D.readIORef globals
  D.writeIORef globals (D.insert (toForeign s) (untyped v) store)
  P.return (GlobalT_Det s)
globaldotprimuscoreglobalT_ND# = P.error "No implementation of globalT_ND"

globaldotprimuscorereadGlobalT_Det# (GlobalT_Det s) = do
  store <- D.readIORef globals
  case D.lookup (toForeign s) store of
    P.Nothing -> P.error ("Invalid global: " P.++ (toForeign s))
    P.Just v  -> P.return (typed v)
globaldotprimuscorereadGlobalT_ND# = P.error "No implementation of readGlobalT_ND"

globaldotprimuscorewriteGlobalT_Det# (GlobalT_Det s) v = fromForeign P.$ do
  store <- D.readIORef globals
  D.writeIORef globals (D.insert (toForeign s) (untyped v) store)
globaldotprimuscorewriteGlobalT_ND# = P.error "No implementation of writeGlobalT_ND"
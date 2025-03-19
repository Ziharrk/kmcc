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
globaldotprimuscoreglobalT_ND# = P.return P.$ Func P.$ \x -> P.return P.$ Func P.$ \y -> do
  x1 <- toHaskell x
  y1 <- y
  let GlobalT_Det s = globaldotprimuscoreglobalT_Det# x1 y1
  P.return P.$ GlobalT_ND (fromHaskell s)

globaldotprimuscorereadGlobalT_Det# (GlobalT_Det s) = do
  store <- D.readIORef globals
  case D.lookup (toForeign s) store of
    P.Nothing -> P.error ("Invalid global: " P.++ (toForeign s))
    P.Just v  -> P.return (typed v)
globaldotprimuscorereadGlobalT_ND# = P.return P.$ Func P.$ \x -> do
  x1 <- x
  case x1 of
    GlobalTFlat# (GlobalT_Det s1) -> P.return P.$ globaldotprimuscorereadGlobalT_Det# (GlobalT_Det s1)
    GlobalT_ND s -> do
      s1 <- toHaskell s
      P.return P.$ globaldotprimuscorereadGlobalT_Det# (GlobalT_Det s1)

globaldotprimuscorewriteGlobalT_Det# (GlobalT_Det s) v = fromForeign P.$ do
  store <- D.readIORef globals
  D.writeIORef globals (D.insert (toForeign s) (untyped v) store)
globaldotprimuscorewriteGlobalT_ND# = P.return P.$ Func P.$ \x -> P.return P.$ Func P.$ \y -> do
  x1 <- x
  case x1 of
    GlobalTFlat# (GlobalT_Det s1) -> do
      y1 <- y
      P.return P.$ P.fmap from (globaldotprimuscorewriteGlobalT_Det# (GlobalT_Det s1) y1)
    GlobalT_ND s -> do
      s1 <- toHaskell s
      y1 <- y
      P.return P.$ P.fmap from (globaldotprimuscorewriteGlobalT_Det# (GlobalT_Det s1) y1)

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
import qualified Prelude as P
import qualified Data.IORef as D
import BasicDefinitions

type instance HsEquivalent IORef_ND# = IORef_Det#

instance ToHs (IORef_ND# a) where
  to (RefND r) = P.return (RefDet r)

instance FromHs (IORef_ND# a) where
  from (RefDet r) = RefND r

instance ShowTerm (IORef_ND# a) where
  showTerm _ _ = P.showString "<<IORef>>"

instance ReadTerm (IORef_ND# a) where
  readTerm = P.error "reading an IORef is not possible"

instance ShowFree (IORef_ND# a) where
  showsFreePrec _ _ = showsStringCurry "<<IORef>>"

instance NormalForm (IORef_ND# a) where
  nfWith _ !x = P.return (P.Left (Val x))

instance Narrowable (IORef_ND# a) where
  narrow = P.error "narrowing an IORef is not possible"
  narrowConstr = P.error "narrowing an IORef is not possible"

instance HasPrimitiveInfo (IORef_ND# a) where
  primitiveInfo = NoPrimitive

instance Unifiable (IORef_ND# a) where
  unifyWith _ _ _ = P.error "unifying an IORef is not possible"

  lazyUnifyVar _ _ = P.error "unifying an IORef is not possible"

instance NFDataC (IORef_ND# a) where
  rnfC !_ = ()

instance Curryable a => Curryable (IORef_ND# a)

-- type declarations for IORef
newtype IORef_Det# a = RefDet (D.IORef a)
newtype IORef_ND# a = RefND (D.IORef (HsEquivalent a))-- actually contains a HsEquivalent that is dynamically converted
-- that is also why we need unsafeCoerce in the following functions

-- function definitions
iORefdotnewIORef_Det# :: a -> Curry_Prelude.IO_Det (IORef_Det# a)
iORefdotnewIORef_Det# x = P.fmap RefDet (D.newIORef x)

iORefdotnewIORef_ND# :: ToHs a => Curry (a :-> Curry_Prelude.IO_ND (IORef_ND# a))
iORefdotnewIORef_ND# = P.return P.$ Func P.$ \x -> do
  x' <- toHaskell x
  P.return (P.fmap RefND (D.newIORef x'))

iORefdotprimuscorereadIORef_Det# :: IORef_Det# a -> Curry_Prelude.IO_Det a
iORefdotprimuscorereadIORef_Det# (RefDet r) = D.readIORef r

iORefdotprimuscorereadIORef_ND# :: FromHs a => Curry (IORef_ND# a :-> Curry_Prelude.IO_ND a)
iORefdotprimuscorereadIORef_ND# = P.return P.$ Func P.$ \x -> do
  RefND x' <- x
  P.return (P.fmap from (D.readIORef x'))

iORefdotprimuscorewriteIORef_Det# :: IORef_Det# a -> a -> Curry_Prelude.IO_Det (Curry_Prelude.CUnit_Det)
iORefdotprimuscorewriteIORef_Det# (RefDet x) y = fromForeign P.$ D.writeIORef x y

iORefdotprimuscorewriteIORef_ND# :: (ToHs a, FromHs a) => Curry (IORef_ND# a :-> a :-> Curry_Prelude.IO_ND (Curry_Prelude.CUnit_ND))
iORefdotprimuscorewriteIORef_ND# = P.return P.$ Func P.$ \x -> P.return P.$ Func P.$ \y -> do
  RefND x' <- x
  y' <- toHaskell y
  P.return (P.fmap from (fromForeign (D.writeIORef x' y')))

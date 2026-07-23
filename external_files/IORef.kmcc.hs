{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
import qualified Prelude as P
import qualified Data.IORef as D
import BasicDefinitions
-- we need unsafeCoerse because GHC's type system does not know
-- that the type of the content of a (deterministic) IORef
-- will match the type given by all the function type signatures.
import Unsafe.Coerce (unsafeCoerce)

type instance HsEquivalent IORef_ND# = IORef_Det#

instance ToHs (IORef_ND# a) where
  to (RefND r) = P.return (RefDet r)

instance FromHs (IORef_ND# a) where
  from (RefDet r) = unsafeCoerce (RefND r)

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
data IORef_Det# a = forall a' . HsEquivalent a' ~ a => RefDet (D.IORef a')
newtype IORef_ND# a = RefND (D.IORef a)

-- function definitions
iORefdotnewIORef_Det# :: forall a a'. (HsEquivalent a' ~ a, ToHs a', FromHs a')
                      => a -> Curry_Prelude.IO_Det (IORef_Det# a)
iORefdotnewIORef_Det# x = P.fmap RefDet (D.newIORef (from x))

iORefdotnewIORef_ND# :: Curry (a :-> Curry_Prelude.IO_ND (IORef_ND# a))
iORefdotnewIORef_ND# = P.return P.$ Func P.$ \x -> do
  x' <- x
  P.return (P.fmap RefND (D.newIORef x'))

iORefdotprimuscorereadIORef_Det# :: forall a a'. (HsEquivalent a' ~ a, ToHs a', FromHs a')
                                 => IORef_Det# a -> Curry_Prelude.IO_Det a
iORefdotprimuscorereadIORef_Det# (RefDet r) = do
  v <- D.readIORef r
  P.return (fromTree (evalCurry (ensureOneResult (to (unsafeCoerce v :: a')))))
  where fromTree (Single x) = x
        fromTree _          = P.error "readIORef: not a single result"

iORefdotprimuscorereadIORef_ND# :: FromHs a => Curry (IORef_ND# a :-> Curry_Prelude.IO_ND a)
iORefdotprimuscorereadIORef_ND# = P.return P.$ Func P.$ \x -> do
  RefND x' <- x
  P.return (D.readIORef x')

iORefdotprimuscorewriteIORef_Det# :: forall a a'. (HsEquivalent a' ~ a, ToHs a', FromHs a')
                                  => IORef_Det# a -> a -> Curry_Prelude.IO_Det (Curry_Prelude.CUnit_Det)
iORefdotprimuscorewriteIORef_Det# (RefDet x) y = fromForeign P.$ D.writeIORef (unsafeCoerce x :: D.IORef a') (from y)

iORefdotprimuscorewriteIORef_ND# :: Curry (IORef_ND# a :-> a :-> Curry_Prelude.IO_ND (Curry_Prelude.CUnit_ND))
iORefdotprimuscorewriteIORef_ND# = P.return P.$ Func P.$ \x -> P.return P.$ Func P.$ \y -> do
  RefND x' <- x
  y' <- y
  P.return (P.fmap from (fromForeign (D.writeIORef x' y')))

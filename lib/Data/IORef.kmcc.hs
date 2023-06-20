{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Prelude as P
import qualified Data.IORef as D
import BasicDefinitions

-- Curryable instance for S.Handle
type instance HsEquivalent D.IORef = D.IORef
type instance HsEquivalent (D.IORef a) = (D.IORef a)

instance ToHs (D.IORef a) where
  to = P.return

instance FromHs (D.IORef a) where
  from = P.id
  
instance ShowFree (D.IORef a) where
  showsFreePrec _ _ = showsStringCurry "<<IORef>>"

instance NormalForm (D.IORef a) where
  nfWith _ !x = P.return (P.Right x)
  
instance Narrowable (D.IORef a) where
  narrow = P.error "narrowing an IORef is not possible"
  narrowConstr = P.error "narrowing an IORef is not possible"
  
instance HasPrimitiveInfo (D.IORef a) where
  primitiveInfo = NoPrimitive

instance Unifiable (D.IORef a) where
  unifyWith _ _ _ = P.error "unifying an IORef is not possible"

  lazyUnifyVar _ _ = P.error "unifying an IORef is not possible"
  
instance Curryable a => Curryable (D.IORef a)

-- type declarations for IORef
type IORef_Det# = D.IORef
type IORef_ND# = D.IORef

-- function definitions
iORefdotnewIORef_Det# = D.newIORef
iORefdotnewIORef_ND# = P.return P.$ Func P.$ \x -> do
  x' <- x
  P.return (P.fmap from (D.newIORef x'))

iORefdotprimuscorereadIORef_Det# = D.readIORef
iORefdotprimuscorereadIORef_ND# = P.return P.$ Func P.$ \x -> do
  x' <- x
  P.return (D.readIORef x')

iORefdotprimuscorewriteIORef_Det# x y = fromForeign P.$ D.writeIORef x y
iORefdotprimuscorewriteIORef_ND# = P.return P.$ Func P.$ \x -> P.return P.$ Func P.$ \y -> do
  x' <- x
  y' <- y
  P.return (P.fmap from (fromForeign (D.writeIORef x' y')))
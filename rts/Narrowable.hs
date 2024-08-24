{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Narrowable where

import GHC.Generics
    ( Generic(..),
      V1,
      U1(..),
      K1(K1),
      M1(M1),
      type (:+:)(..),
      type (:*:)(..) )

import Data.SBV ( SymVal )

import {-# SOURCE #-} MemoizedCurry ( Curry )
import Classes ( MonadShare(..), MonadFree(..) )

-- Narrowable class and methods.
-- We use Generics to give a default implementation for this class.
-- That way, we can automatically derive instances.

class Narrowable a where
  narrow :: [Curry a]
  narrowConstr :: a -> (# Curry a, [Curry a] #)

-- Differentiates between primitive types (e.g., Int)
-- and non-primitive types (e.g, lists).
-- Also carries a type class constraint as an existential.
-- Depending on the type, we either need to be able to narrow it
-- or to use constraint solving with it.
data PrimitiveInfo a = Narrowable a => NoPrimitive
                     | SymVal a => Primitive

-- Class to see if a type is a primitive type (e.g., Int)
class HasPrimitiveInfo a where
  primitiveInfo :: PrimitiveInfo a
  default primitiveInfo :: (Narrowable a) => PrimitiveInfo a
  primitiveInfo = NoPrimitive

instance HasPrimitiveInfo Integer where
  primitiveInfo = Primitive

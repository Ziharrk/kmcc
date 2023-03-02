{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeOperators             #-}
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
  narrowConstr :: a -> Curry a

defaultNarrow :: (Narrowable a, Generic a, NarrowableGen (Rep a)) => [Curry a]
defaultNarrow = map (fmap to) narrowGen

defaultNarrowConstr :: (Narrowable a, Generic a, NarrowableGen (Rep a)) => a -> Curry a
defaultNarrowConstr a = to <$> narrowConstrGen (from a)

-- Beyond here, the machinery for the default implementation is defined.
-- There are some tutorials on the internet about GHC's generic types and how to use them to
-- derive instances of different classes.

class NarrowableGen f where
  narrowGen :: [Curry (f x)]
  narrowConstrGen ::f x -> Curry (f x)

instance NarrowableGen V1 where
  narrowGen = []
  narrowConstrGen x = case x of

instance NarrowableGen U1 where
  narrowGen = [return U1]
  narrowConstrGen U1 = return U1

instance (NarrowableGen f, NarrowableGen g) => NarrowableGen (f :+: g) where
  narrowGen = map (fmap L1) narrowGen ++ map (fmap R1) narrowGen

  narrowConstrGen (L1 x) = L1 <$> narrowConstrGen x
  narrowConstrGen (R1 x) = R1 <$> narrowConstrGen x

instance (NarrowableGen f, NarrowableGen g) => NarrowableGen (f :*: g) where
  narrowGen = concatMap (\x -> map (\y -> (:*:) <$> x <*> y) narrowGen) narrowGen

  narrowConstrGen (x :*: y) = (:*:) <$> narrowConstrGen x <*> narrowConstrGen y

instance NarrowableGen f => NarrowableGen (M1 j h f) where
  narrowGen = map (fmap M1) narrowGen
  narrowConstrGen (M1 x) = M1 <$> narrowConstrGen x

instance (FreeConstraints Curry a) => NarrowableGen (K1 i (Curry a)) where
  narrowGen = [K1 <$> share free]

  narrowConstrGen (K1 _) = K1 <$> share free

-- implementation is generated automatically.
instance Narrowable Bool where
  narrow = defaultNarrow
  narrowConstr = defaultNarrowConstr


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

instance HasPrimitiveInfo Bool

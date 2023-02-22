{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
{-|
Module      : Plugin.Effect.Classes
Description : Type classes used for the effect implementation
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains type classes for the nondeterminism effect,
explicit sharing and conversion between lifted and unlifted types.
The type classes are adapted from the
library explicit-sharing by Sebastian Fischer,
modernized with a generic implementation by Kai-Oliver Prott.
-}
module Classes where

import GHC.Generics as Gen
    ( Generic(..),
      V1,
      U1(..),
      K1(K1),
      M1(M1),
      type (:+:)(..),
      type (:*:)(..) )
import Data.Kind ( Constraint )

-- | A class for Monads with support for explicit sharing of effects.
class Monad s => MonadShare s where
  type family ShareConstraints s a :: Constraint
  type instance ShareConstraints s a = ()
  share :: ShareConstraints s a => s a -> s (s a)

-- | A class for Monads with support for explicit sharing of top-level effects.
class Monad s => SharingTop s where
  type family ShareTopConstraints s a :: Constraint
  type instance ShareTopConstraints s a = ()
  shareTopLevel :: ShareTopConstraints s a => (Int, String) -> s a -> s a
  shareTopLevel = const id

-- | A class for Monads with support for free (logic) variables.
class Monad f => MonadFree f where
  type family FreeConstraints f a :: Constraint
  type instance FreeConstraints f a = ()
  free :: FreeConstraints f a => f a

-- | A class for deep sharing of nested effects.
-- For types with a generic instance, it can be derived automatically.
class MonadShare m => Shareable m a where
  shareArgs :: a -> m a
  default shareArgs :: (Gen.Generic a, ShareableGen m (Gen.Rep a))
                    => a -> m a
  shareArgs a = Gen.to <$> shareArgsGen (Gen.from a)

class MonadShare m => ShareableGen m f where
  shareArgsGen :: f x -> m (f x)

instance (MonadShare m) => ShareableGen m Gen.V1 where
  shareArgsGen _ = undefined

instance (MonadShare m) => ShareableGen m Gen.U1 where
  shareArgsGen = return

instance (MonadShare m, ShareableGen m f, ShareableGen m g) =>
  ShareableGen m (f Gen.:+: g) where
    shareArgsGen (Gen.L1 x) = Gen.L1 <$> shareArgsGen x
    shareArgsGen (Gen.R1 x) = Gen.R1 <$> shareArgsGen x

instance (MonadShare m, ShareableGen m f, ShareableGen m g) =>
  ShareableGen m (f Gen.:*: g) where
    shareArgsGen (x Gen.:*: y) =
      (Gen.:*:) <$> shareArgsGen x <*> shareArgsGen y

instance (MonadShare m, ShareConstraints m b) => ShareableGen m (Gen.K1 i (m b)) where
    shareArgsGen (Gen.K1 x) = Gen.K1 <$> share x

instance (MonadShare m, ShareableGen m f) => ShareableGen m (Gen.M1 i t f) where
  shareArgsGen (Gen.M1 x) = Gen.M1 <$> shareArgsGen x

-- * Instances for Shareable

instance (MonadShare m) => Shareable m () where
  shareArgs = return

instance (MonadShare m) => Shareable m Ordering where
  shareArgs = return

instance (MonadShare m) => Shareable m Bool where
  shareArgs = return

instance (MonadShare m) => Shareable m Int where
  shareArgs = return

instance (MonadShare m) => Shareable m Integer where
  shareArgs = return

instance (MonadShare m) => Shareable m Float where
  shareArgs = return

instance (MonadShare m) => Shareable m Double where
  shareArgs = return

instance (MonadShare m) => Shareable m Char where
  shareArgs = return

instance (MonadShare m) => Shareable m (a -> b) where
  shareArgs = return

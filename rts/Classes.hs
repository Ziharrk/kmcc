{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}

{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
{-|
Module      : Plugin.Effect.Classes
Description : Type classes used for the effect implementation
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains type classes for the nondeterminism effect and the
explicit sharing.
-}
module Classes where

import Data.Kind ( Constraint )

-- | A class for Monads with support for explicit sharing of effects.
class Monad s => MonadShare s where
  share :: s a -> s (s a)

-- | A class for Monads with support for free (logic) variables.
class Monad f => MonadFree f where
  type family FreeConstraints f a :: Constraint
  type instance FreeConstraints f a = ()
  free :: FreeConstraints f a => f a

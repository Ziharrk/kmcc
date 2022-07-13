{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE RoleAnnotations          #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module MemoizedCurry where

import Control.Monad
import Data.Kind

import HasPrimitiveInfo (HasPrimitiveInfo)
import Classes

type Curry :: Type -> Type
type role Curry nominal
data Curry a

instance Functor Curry

instance Applicative Curry

instance Monad Curry

instance MonadPlus Curry

free :: (HasPrimitiveInfo a, Shareable Curry a) => Curry a

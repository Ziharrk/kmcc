{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE RoleAnnotations          #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module MemoizedCurry where

import Control.Monad ( MonadPlus )
import Data.Kind ( Type )

import Classes ( MonadFree, MonadShare )

type Curry :: Type -> Type
type role Curry nominal
data Curry a

instance Functor Curry

instance Applicative Curry

instance Monad Curry

instance MonadShare Curry

instance MonadFree Curry

instance MonadPlus Curry

{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-orphans        #-}
module BasicDefinitions
 ( module BasicDefinitions
 , module MemoizedCurry
 , module Narrowable
 , module NormalForm
 , module HasPrimitiveInfo
 , module Classes
 ) where

import Control.Exception
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.State
import System.IO.Unsafe
import GHC.IO.Exception

import MemoizedCurry
import Narrowable
import HasPrimitiveInfo
import Classes
import NormalForm

type family HsEquivalent (a :: k) = (b :: k) | b -> a

class ToHs a where
  to :: a -> Curry (HsEquivalent a)

toHaskell :: ToHs a => Curry a -> Curry (HsEquivalent a)
toHaskell = (>>= to)

class FromHs a where
  from :: HsEquivalent a -> a

fromHaskell :: FromHs a => HsEquivalent a -> Curry a
fromHaskell x = unsafePerformIO $ catch (return <$> evaluate (from x))
                                $ \Failed -> return mzero

class (ToHs a, FromHs a, Shareable Curry a, Unifiable a, NormalForm a, HasPrimitiveInfo a) => Curryable a

type instance HsEquivalent Integer = Integer

instance ToHs Integer where
  to = return

instance FromHs Integer where
  from = id

instance Curryable Integer

type instance HsEquivalent Double = Double

instance ToHs Double where
  to = return

instance FromHs Double where
  from = id

instance HasPrimitiveInfo Double where
  primitiveInfo = Primitive

instance Unifiable Double where
  unifyWith _ x y = if x == y then return True else mzero

  lazyUnifyVar n i = modify (addToVarHeap i (return n)) >> return True

instance NormalForm Double where
  nfWith _ !x = return x

instance Curryable Double

type instance HsEquivalent Char = Char

instance ToHs Char where
  to = return

instance FromHs Char where
  from = id

instance HasPrimitiveInfo Char where
  primitiveInfo = Primitive

instance Unifiable Char where
  unifyWith _ x y = if x == y then return True else mzero

  lazyUnifyVar n i = modify (addToVarHeap i (return n)) >> return True

instance NormalForm Char where
  nfWith _ !x = return x

instance Curryable Char

type instance HsEquivalent IO = IO
type instance HsEquivalent (IO a) = IO (HsEquivalent a)

instance ToHs (IO a) where
  to = error "FFI Error: 'To' Conversion on IO"

instance FromHs a => FromHs (IO a) where
  from x = from <$> x

instance Shareable Curry a => Shareable Curry (IO a) where
  shareArgs = return

instance HasPrimitiveInfo (IO a) where
  primitiveInfo = NoPrimitive

instance Narrowable (IO a) where
  narrow = error "narrowing an IO action is not possible"
  narrowConstr _ = error "narrowing an IO action is not possible"

instance Shareable Curry a => Unifiable (IO a) where
  unifyWith _ _ _ = error "unifying an IO action is not possible"
  lazyUnifyVar _ _ = error "lazily unifying an IO action is not possible"

instance NormalForm (IO a) where
  nfWith _ !x = return x

instance Curryable a => Curryable (IO a)

data Failed = Failed
  deriving Show

instance Exception Failed

type LiftedFunc = (:->)

type instance HsEquivalent LiftedFunc = (->)
type instance HsEquivalent (LiftedFunc a) = (->) (HsEquivalent a)
type instance HsEquivalent (LiftedFunc a b) = (->) (HsEquivalent a) (HsEquivalent b)

instance ToHs (LiftedFunc a b) where
  to _ = error "FFI Error: 'To' Conversion on functions"

instance FromHs (LiftedFunc a b) where
  from _ = error "FFI Error: 'From' Conversion on functions"

instance Shareable Curry (LiftedFunc a b) where
  shareArgs = return

instance HasPrimitiveInfo (LiftedFunc a b) where
  primitiveInfo = NoPrimitive

instance Narrowable (LiftedFunc a b) where
  narrow = error "narrowing a function is not possible"
  narrowConstr _ = error "narrowing a function is not possible"

instance Unifiable (LiftedFunc a b) where
  unifyWith _ _ = error "unifying a function is not possible"
  lazyUnifyVar _ = error "lazily unifying a function is not possible"

instance NormalForm (LiftedFunc a b) where
  nfWith _ !x = return x

instance Curryable (LiftedFunc a b)

app :: Curry (LiftedFunc a b) -> Curry a -> Curry b
app mf arg = mf >>= \(Func f) -> f arg

returnFunc :: (Curry a -> Curry b) -> Curry (LiftedFunc a b)
returnFunc f = return (Func f)

addToVarHeapM :: ID -> Curry a -> Curry ()
addToVarHeapM i x = modify (addToVarHeap i x)

liftConvert1 :: (ToHs a, FromHs b)
             => (HsEquivalent a -> HsEquivalent b)
             -> Curry (LiftedFunc a b)
liftConvert1 f = return $ Func $ toHaskell >=> \x' ->
  return (from (f x'))

liftConvert2 :: (ToHs a, ToHs b, FromHs c)
             => (HsEquivalent a -> HsEquivalent b -> HsEquivalent c)
             -> Curry (LiftedFunc a (LiftedFunc b c))
liftConvert2 f = return $ Func $ toHaskell >=> \x' -> return $ Func $ toHaskell >=> \y' ->
  return (from (f x' y'))

liftConvertIO1 :: (ToHs a, FromHs b)
               => (HsEquivalent a -> IO (HsEquivalent b))
               -> Curry (LiftedFunc a (IO b))
liftConvertIO1 f = return $ Func $ \x -> do
  x' <- toHaskell x
  return (fmap from (f x'))

liftConvertIO2 :: (ToHs a, ToHs b, FromHs c)
               => (HsEquivalent a -> HsEquivalent b -> IO (HsEquivalent c))
               -> Curry (LiftedFunc a (LiftedFunc b (IO c)))
liftConvertIO2 f = return $ Func $ \x -> return $ Func $ \y -> do
  x' <- toHaskell x
  y' <- toHaskell y
  return (fmap from (f x' y'))

ensureOneResult :: Curry a -> Curry a
ensureOneResult (Curry (ND act)) = Curry $ ND $ do
  s <- get
  case lowerCodensity (runStateT act s) of
    Fail _
      -> throw (IOError Nothing OtherError "ensureOneResult" "FAILERR_ IO action failed non-determinsitically" Nothing Nothing)
    Single (x, s')
      -> put s' >> return x
    Choice {}
      -> throw (IOError Nothing OtherError "ensureOneResult" "NDERR_ IO action was non-determinsitically" Nothing Nothing)

bindIONDImpl :: Curry (LiftedFunc (IO a) (LiftedFunc (LiftedFunc a (IO b)) (IO b)))
bindIONDImpl = return $ Func $ \ioND -> return $ Func $ \fND -> do
  io <- ensureOneResult ioND
  Func f <- ensureOneResult fND
  f . return $! unsafePerformIO $ unsafeInterleaveIO io

returnIONDImpl :: Curry (LiftedFunc a (IO a))
returnIONDImpl = return $ Func $ \x -> fmap return x

mainWrapper :: Curry (IO a) -> IO ()
mainWrapper x = case evalCurry (ensureOneResult x) of
  Single x' -> void x'
  _         -> error "mainWrapper: not a single result"

class ForeignType a where
  type Foreign a = b | b -> a
  toForeign :: a -> Foreign a
  fromForeign :: Foreign a -> a

instance ForeignType Integer where
  type Foreign Integer = Integer
  toForeign = id
  fromForeign = id

instance ForeignType Double where
  type Foreign Double = Double
  toForeign = id
  fromForeign = id

instance ForeignType Char where
  type Foreign Char = Char
  toForeign = id
  fromForeign = id

instance ForeignType a => ForeignType (IO a) where
  type Foreign (IO a) = IO (Foreign a)
  toForeign = fmap toForeign
  fromForeign = fmap fromForeign

liftForeign1 :: (ForeignType a, ForeignType b)
             => (Foreign a -> Foreign b) -> a -> b
liftForeign1 f x = fromForeign (f (toForeign x))

liftForeign2 :: (ForeignType a, ForeignType b, ForeignType c)
             => (Foreign a -> Foreign b -> Foreign c) -> a -> b -> c
liftForeign2 f x y = fromForeign (f (toForeign x) (toForeign y))

dollarBangNDImpl :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarBangNDImpl =
  returnFunc (\f ->
  returnFunc (\(Curry a) -> Curry (
    a >>= unCurry. \case
      Val _ x   -> x `seq` (f `app` return x)
      Var lvl i -> f `app` Curry (return (Var lvl i)))))

dollarBangBangNDImpl :: NormalForm a => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarBangBangNDImpl = returnFunc (\f -> returnFunc (\a -> f `app` normalForm a))

dollarHashHashNDImpl :: NormalForm a => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarHashHashNDImpl = returnFunc (\f -> returnFunc (\a -> f `app` groundNormalForm a))

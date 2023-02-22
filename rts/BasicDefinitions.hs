{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-orphans        #-}
module BasicDefinitions
 ( module BasicDefinitions
 , module MemoizedCurry
 , module Narrowable
 , module Classes
 , fs ,bfs , dfs
 ) where

import Control.Exception
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.State
import Data.List
import qualified Data.Set as Set
import System.IO.Unsafe
import GHC.IO.Exception

import MemoizedCurry
import Narrowable
import Classes
import Tree
import Data.SBV (SBV, (.===))

type family HsEquivalent (a :: k) = (b :: k) | b -> a

class ToHs a where
  to :: a -> Curry (HsEquivalent a)

toHaskell :: ToHs a => Curry a -> Curry (HsEquivalent a)
toHaskell = (>>= to)

class FromHs a where
  from :: HsEquivalent a -> a

{-# NOINLINE fromHaskell #-}
fromHaskell :: FromHs a => HsEquivalent a -> Curry a
fromHaskell x = unsafePerformIO $ catch (evaluate (from x) >>= \x' -> return (return x'))
                                $ \Failed -> return mzero

class ShowFree a where
  showsFreePrec :: Int -> a -> ShowSFree
  showFree :: a -> [(Integer, String)] -> Curry String
  showFree x fm = snd $ showsFreePrec 0 x (fm, return "")

type ShowSFree = ([(Integer, String)], Curry String) -> ([(Integer, String)], Curry String)

showsFreePrecCurry :: forall a. ShowFree a => Int -> Curry a -> ([(Integer, String)], Curry String)
                   -> ([(Integer, String)], Curry String)
showsFreePrecCurry p x (fm, s) = (fm,) $ Curry $ do
  x' <- deref x
  unCurry $ case x' of
    Var lvl i -> get >>= \NDState { .. } ->
      if Set.member i constrainedVars
        then instantiate @a lvl i >>= \y -> snd $ showsFreePrec p y (fm, s)
        else showsVar i fm s
    Val _ y -> snd $ showsFreePrec p y (fm, s)

showFreeCurry :: ShowFree a => Curry a -> [(Integer, String)] -> Curry String
showFreeCurry x fm = snd $ showsFreePrecCurry 0 x (fm, return "")

showsBracketsCurry :: Int -> ShowSFree -> ShowSFree
showsBracketsCurry 0 s = s
showsBracketsCurry _ s = showsStringCurry ")" . s . showsStringCurry "("

showSpaceCurry :: ShowSFree -> ShowSFree -> ShowSFree
showSpaceCurry f g = g . showsStringCurry " " . f

showsStringCurry :: String -> ShowSFree
showsStringCurry s (fm, x) = (fm, fmap (++s) x)

showsVar :: Integer -> [(Integer, String)] -> Curry String -> Curry String
showsVar i fm = case lookup i fm of
  Nothing -> fmap (++ ("_" ++ show i))
  Just s  -> fmap (++ s)

-- Class to pull all non-determinisim to the top
class NormalForm a where
  nfWith :: (forall x. NormalForm x => Curry x -> ND (Either (CurryVal x) (HsEquivalent x)))
         -> a -> ND (Either (CurryVal a) (HsEquivalent a))

normalForm' :: NormalForm a => Curry a -> ND (Either (CurryVal a) (HsEquivalent a))
normalForm' a = do
  a' <- deref a
  case a' of
    Val _   x -> nfWith normalForm' x
    Var lvl i -> return (Left (Var lvl i))

normalForm :: (NormalForm a, FromHs a) => Curry a -> Curry a
normalForm a = ndEitherToCurry (normalForm' a)

eitherToCurry :: FromHs a => Either (CurryVal a) (HsEquivalent a) -> Curry a
eitherToCurry = Curry . either return (unCurry . fromHaskell)

ndEitherToCurry :: FromHs a => ND (Either (CurryVal a) (HsEquivalent a)) -> Curry a
ndEitherToCurry a = Curry $ a >>= unCurry . eitherToCurry

groundNormalForm' :: NormalForm a => Curry a -> ND (Either (CurryVal a) (HsEquivalent a))
groundNormalForm' a = deref a >>= \case
  Val _   x -> nfWith groundNormalForm' x
  Var lvl i -> groundNormalForm' (instantiate lvl i)

groundNormalForm :: (NormalForm a, FromHs a) => Curry a -> Curry a
groundNormalForm a = ndEitherToCurry (groundNormalForm' a)

dollarBangBangNDImpl :: (NormalForm a, FromHs a) => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarBangBangNDImpl = returnFunc (\f -> returnFunc (\a -> f `app` normalForm a))

dollarHashHashNDImpl :: (NormalForm a, FromHs a) => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarHashHashNDImpl = returnFunc (\f -> returnFunc (\a -> f `app` groundNormalForm a))

class (ToHs a, FromHs a, ShowFree a, Shareable Curry a, Unifiable a, NormalForm a, HasPrimitiveInfo a) => Curryable a

type instance HsEquivalent Integer = Integer

instance ToHs Integer where
  to = return

instance FromHs Integer where
  from = id

instance ShowFree Integer where
  showsFreePrec _ x = showsStringCurry (show x)

instance NormalForm Integer where
  nfWith _ !x = return (Right x)

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
  nfWith _ !x = return (Right x)

instance ShowFree Double where
  showsFreePrec _ x = showsStringCurry (show x)

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
  nfWith _ !x = return (Right x)

instance ShowFree Char where
  showsFreePrec _ x = showsStringCurry (show x)

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
  nfWith _ !x = return (Left (Val Shared x))

instance ShowFree (IO a) where
  showsFreePrec _ _ = showsStringCurry "<<IO>>"

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
  nfWith _ !x = return (Left (Val Shared x))

instance ShowFree (LiftedFunc a b) where
  showsFreePrec _ _ = showsStringCurry "<<Function>>"

instance Curryable (LiftedFunc a b)

{-# INLINE [1] app #-}
app :: Curry (LiftedFunc a b) -> Curry a -> Curry b
app mf arg = mf >>= \(Func f) -> f arg

{-# INLINE [1] returnFunc #-}
returnFunc :: (Curry a -> Curry b) -> Curry (LiftedFunc a b)
returnFunc f = return (Func f)

{-# RULES
"app/returnFunc0" forall f. app (returnFunc f) = f
"app/returnFunc1" forall f x. app (returnFunc f) x = f x
  #-}

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

{-# NOINLINE bindIONDImpl #-}
bindIONDImpl :: Curry (LiftedFunc (IO a) (LiftedFunc (LiftedFunc a (IO b)) (IO b)))
bindIONDImpl = return $ Func $ \ioND -> return $ Func $ \fND -> do
  io <- ensureOneResult ioND
  Func f <- ensureOneResult fND
  f . return $! unsafePerformIO $ unsafeInterleaveIO io

returnIONDImpl :: Curry (LiftedFunc a (IO a))
returnIONDImpl = return $ Func $ \x -> fmap return x

mainWrapperDet :: IO a -> IO ()
mainWrapperDet ma = () <$ ma

mainWrapperNDet :: (ForeignType b, Foreign b ~ (), ToHs a, HsEquivalent a ~ b) => Curry (IO a) -> IO ()
mainWrapperNDet x = case evalCurry (ensureOneResult x) of
  Single x' -> x' >>= return . toForeign . unSingle . evalCurry . ensureOneResult . to
  _         -> error "mainWrapper: not a single result"

unSingle :: MemoizedCurry.Tree n f l -> l
unSingle (Single x) = x
unSingle _ = error "mainWrapper: not a single result"

exprWrapperDet :: (ShowFree b, HsEquivalent b ~ a, FromHs b) => (Tree.Tree String -> [String]) -> a -> IO ()
exprWrapperDet search a = case search $ evalCurryTree (showFreeCurry (fromHaskell a) []) of
  []  -> fail "**No value found"
  [s] -> putStrLn s
  _   -> error "internalError: More than on result from deterministic expression"

exprWrapperNDet :: ShowFree a => (Tree.Tree String -> [String]) -> [(String, Integer)] -> Bool -> Curry (CurryVal a, [VarInfo]) -> IO ()
exprWrapperNDet search fvs b ca = printRes (search $ evalCurryTree extract)
  where
    sortedFvs = map fst $ sortOn snd fvs

    printRes [] = fail "**No value found"
    printRes xs = mapM_ putStrLn xs

    extract = do
      (va, ids) <- ca
      let swapped = map (\(x, y) -> (y, x)) fvs
      str <- showFreeCurry (Curry (return va)) swapped
      vs <- mapM (\(VarInfo @a i) -> showFreeCurry (Curry $ deref $ Curry $ return $ Var @a 0 i) swapped) ids
      let vs' = zipWith (\s n -> n ++ " = " ++ s) vs sortedFvs
      let str' = if null vs || not b then str else "{ " ++ intercalate "\n, " vs' ++ " } " ++ str
      return str'

data VarInfo = forall a. (ShowFree a, HasPrimitiveInfo a, Shareable Curry a) => VarInfo Integer

getVarId :: forall a. ShowFree a => Curry a -> ND VarInfo
getVarId ca = do
  a <- unCurry ca
  case a of
    Var _ i -> return $ VarInfo @a i
    _ -> error "getVarId: not a variable"

addVarIds :: Curry a -> [ND VarInfo] -> Curry (CurryVal a, [VarInfo])
addVarIds ca xs = Curry $ do
  ids <- sequence xs
  a <- unCurry ca
  return (Val Shared (a, ids))

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


-- TODO update constrained vars
primitive1 :: forall a b
            . ( HasPrimitiveInfo a, ForeignType a
              , Curryable b, ForeignType b )
           => (SBV a -> SBV b)
           -> (Foreign a -> Foreign b)
           -> Curry (a :-> b)
primitive1 sbvF hsF = case (# primitiveInfo @a, primitiveInfo @b #) of
  (# Primitive, Primitive #) -> return . Func $ \ca -> Curry $ do
    a <- deref ca
    case a of
      Val _ x -> return $ Val Shared $ fromForeign $ hsF $ toForeign x
      Var lvl i -> do
        j <- freshID
        s@NDState { .. } <- get
        let c = sbvF (toSBV (Var lvl i)) .=== toSBV (Var lvl j)
        let csv' = foldr Set.insert constrainedVars (j : allVars a)
        let cst' = insertConstraint c constraintStore
        put (s { constraintStore = cst', constrainedVars = csv' })
        -- Consistency not necessary, see comment in primitive2
        return (Var lvl j)
  _ -> error "internalError: primitive1: non-primitive type"

primitive2 :: forall a b c
            . ( HasPrimitiveInfo a, ForeignType a
              , HasPrimitiveInfo b, ForeignType b
              , Curryable c, ForeignType c )
           => (SBV a -> SBV b -> SBV c)
           -> (Foreign a -> Foreign b -> Foreign c)
           -> Curry (a :-> b :-> c)
primitive2 sbvF hsF = case (# primitiveInfo @a, primitiveInfo @b, primitiveInfo @c #) of
  (# Primitive, Primitive, Primitive #) -> return . Func $ \ca -> return . Func $ \cb -> Curry $ do
    a <- deref ca
    b <- deref cb
    case (# a, b #) of
      (# Val _ x, Val _ y #) -> return $ Val Shared $ fromForeign $ hsF (toForeign x) (toForeign y)
      _ -> do
          k <- freshID
          s@NDState { .. } <- get
          let c = sbvF (toSBV a) (toSBV b) .=== toSBV (Var 0 k)
          let csv' = foldr Set.insert constrainedVars (k : allVars a ++ allVars b)
          let cst' = insertConstraint c constraintStore
          put (s { constraintStore = cst', constrainedVars = csv' })
          -- Checking consistency is unnecessary, because "j" is fresh.
          -- However, it is important to enter x and y in the set of constrained vars, because
          -- they might get constrained indirectly via "j". Example:
          -- j <- x + y
          -- j <- 1
          -- matchFL 9 x
          -- matchFL 0 y
          return (Var 0 k)
  _ -> error "internalError: primitive2: non-primitive type"

allVars :: CurryVal a -> [Integer]
allVars (Var _ i) = [i]
allVars _         = []

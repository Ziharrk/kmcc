{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
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

import Control.Exception (throw, catch, evaluate, Exception)
import Control.Monad (MonadPlus(..), (>=>))
import Control.Monad.Codensity (lowerCodensity)
import Control.Monad.State (modify, MonadState(put, get), StateT(runStateT))
import Control.DeepSeq (deepseq, NFData)
import Data.List (intercalate, sortOn)
import Data.SBV (SBV, (.===), sNot)
import qualified Data.Set as Set
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Text.Read (pfail)

import MemoizedCurry
import Narrowable
import Classes
import Any
import Tree (Tree, dfs, bfs, fs)

failed :: a
failed = throw Failed

toHaskell :: ToHs a => Curry a -> Curry (HsEquivalent a)
toHaskell = (>>= to)

{-# INLINE elimFlatM #-}
elimFlatM :: (FromHs a) => Curry a -> Curry a
elimFlatM = fmap elimFlat

{-# NOINLINE fromHaskell #-}
fromHaskell :: FromHs a => HsEquivalent a -> Curry a
fromHaskell x = unsafePerformIO $ catch (evaluate (from x) >>= \x' -> return (return x'))
                                $ \Failed -> return mzero

showsFreePrecCurry :: forall a. ShowFree a => Int -> Curry a -> ([(Integer, String)], Curry String)
                   -> ([(Integer, String)], Curry String)
showsFreePrecCurry p x (fm, s) = (fm,) $ Curry $ do
  x' <- deref x
  unCurry $ case x' of
    Var lvl i -> get >>= \NDState { .. } ->
      if Set.member i constrainedVars
        then instantiate @a lvl i >>= \y -> snd $ showsFreePrec p y (fm, s)
        else showsVar i fm s
    Val   y -> snd $ showsFreePrec p y (fm, s)

showFreeCurry :: ShowFree a => Curry a -> [(Integer, String)] -> Curry String
showFreeCurry x fm = snd $ showsFreePrecCurry 0 x (fm, return "")

showsBracketsCurry :: Int -> ShowSFree -> ShowSFree
showsBracketsCurry 0 s = s
showsBracketsCurry _ s = showsStringCurry ")" . s . showsStringCurry "("

showSpaceCurry :: ShowSFree -> ShowSFree -> ShowSFree
showSpaceCurry f g = g . showsStringCurry " " . f

showsVar :: Integer -> [(Integer, String)] -> Curry String -> Curry String
showsVar i fm = case lookup i fm of
  Nothing -> fmap (++ ("_" ++ show i))
  Just s  -> fmap (++ s)

literalCase :: HasPrimitiveInfo a => Curry a -> (ID -> Curry b) -> (a -> Curry b) -> Curry b
literalCase x f g = Curry $ do
  x' <- deref x
  unCurry $ case x' of
    Val   y -> g y
    Var _ i -> f i

bindVar :: Curryable a => ID -> Curry a -> Curry Bool
bindVar i = unify (freeWith 0 i)

normalForm' :: NormalForm a => Curry a -> ND (Either (CurryVal a) (HsEquivalent a))
normalForm' a = do
  a' <- deref a
  case a' of
    Val     x -> nfWith normalForm' x
    Var lvl i -> return (Left (Var lvl i))

normalForm :: (NormalForm a, FromHs a) => Curry a -> Curry a
normalForm a = ndEitherToCurry (normalForm' a)

eitherToCurry :: FromHs a => Either (CurryVal a) (HsEquivalent a) -> Curry a
eitherToCurry = Curry . either return (unCurry . fromHaskell)

ndEitherToCurry :: FromHs a => ND (Either (CurryVal a) (HsEquivalent a)) -> Curry a
ndEitherToCurry a = Curry $ a >>= unCurry . eitherToCurry

groundNormalForm' :: NormalForm a => Curry a -> ND (Either (CurryVal a) (HsEquivalent a))
groundNormalForm' a = deref a >>= \case
  Val     x -> nfWith groundNormalForm' x
  Var lvl i -> groundNormalForm' (instantiate lvl i)

groundNormalForm :: (NormalForm a, FromHs a) => Curry a -> Curry a
groundNormalForm a = ndEitherToCurry (groundNormalForm' a)

dollarBangBangNDImpl :: (NormalForm a, FromHs a) => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarBangBangNDImpl = returnFunc (\f -> returnFunc (\a -> f `app` normalForm a))

dollarHashHashNDImpl :: (NormalForm a, FromHs a) => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarHashHashNDImpl = returnFunc (\f -> returnFunc (\a -> f `app` groundNormalForm a))

data Failed = Failed
  deriving Show

instance Exception Failed

type LiftedFunc = (:->)

type instance HsEquivalent LiftedFunc = (->)

instance ToHs (LiftedFunc a b) where
  to _ = error "FFI Error: 'To' Conversion on functions"

instance FromHs (LiftedFunc a b) where
  from _ = error "FFI Error: 'From' Conversion on functions"
  elimFlat _ = error "FFI Error: 'From' Conversion on functions"

instance HasPrimitiveInfo (LiftedFunc a b) where
  primitiveInfo = NoPrimitive

instance Narrowable (LiftedFunc a b) where
  narrow = error "narrowing a function is not possible"
  narrowConstr _ = error "narrowing a function is not possible"

instance Unifiable (LiftedFunc a b) where
  unifyWith _ _ = error "unifying a function is not possible"
  lazyUnifyVar _ = error "lazily unifying a function is not possible"

instance NormalForm (LiftedFunc a b) where
  nfWith _ !x = return (Left (Val x))

instance ShowFree (LiftedFunc a b) where
  showsFreePrec _ _ = showsStringCurry "<<Function>>"

instance ShowTerm (LiftedFunc a b) where
  showTerm _ _ = showString "<<Function>>"

instance ReadTerm (LiftedFunc a b) where
  readTerm = pfail

instance (Levelable a, Levelable b) => Levelable (LiftedFunc a b) where
  setLevel l (Func f) = Func $ \a -> setLevelC l (f (setLevelC l a))

instance NFDataC (LiftedFunc a b) where
  rnfC x = x `seq` ()

instance (Levelable a, Levelable b) => Curryable (LiftedFunc a b)

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

liftConvert1 :: (Curryable a, Curryable b)
             => (HsEquivalent a -> HsEquivalent b)
             -> Curry (LiftedFunc a b)
liftConvert1 f = return $ Func $ share >=> toHaskell >=> \x' ->
  return (from (f x'))

liftConvert2 :: (Curryable a, Curryable b, Curryable c)
             => (HsEquivalent a -> HsEquivalent b -> HsEquivalent c)
             -> Curry (LiftedFunc a (LiftedFunc b c))
liftConvert2 f = return $ Func $ share >=> toHaskell >=> \x' -> return $ Func $ toHaskell >=> \y' ->
  return (from (f x' y'))

liftConvertIO1 :: (Curryable a, Curryable b)
               => (HsEquivalent a -> IO (HsEquivalent b))
               -> Curry (LiftedFunc a (IO b))
liftConvertIO1 f = return $ Func $ \x -> do
  x' <- share x >>= toHaskell
  return (fmap from (f x'))

liftConvertIO2 :: (Curryable a, Curryable b, Curryable c)
               => (HsEquivalent a -> HsEquivalent b -> IO (HsEquivalent c))
               -> Curry (LiftedFunc a (LiftedFunc b (IO c)))
liftConvertIO2 f = return $ Func $ \x -> return $ Func $ \y -> do
  x' <- share x >>= toHaskell
  y' <- share y >>= toHaskell
  return (fmap from (f x' y'))

ensureOneResult :: Curry a -> Curry a
ensureOneResult (Curry (ND act)) = Curry $ ND $ do
  s <- get
  case lowerCodensity (runStateT act s) of
    Fail _
      -> throw (IOError Nothing OtherError "ensureOneResult" "FAILERR_ IO action failed non-deterministic" Nothing Nothing)
    Single (x, s')
      -> put s' >> return x
    Choice {}
      -> throw (IOError Nothing OtherError "ensureOneResult" "NDERR_ IO action was non-deterministic" Nothing Nothing)

{-# NOINLINE bindIONDImpl #-}
bindIONDImpl :: Curry (LiftedFunc (IO a) (LiftedFunc (LiftedFunc a (IO b)) (IO b)))
bindIONDImpl = returnFunc $ \ioND -> returnFunc $ \fND -> bindIO ioND fND

bindIO :: Curry (IO a) -> Curry (LiftedFunc a (IO b)) -> Curry (IO b)
bindIO ioND fND = do
  io <- ensureOneResult ioND
  Func f <- ensureOneResult fND
  f . return $! unsafePerformIO $ unsafeInterleaveIO io

returnIONDImpl :: Curry (LiftedFunc a (IO a))
returnIONDImpl = return $ Func $ \x -> fmap return x

data UnitDispatch a where
  IsUnit :: UnitDispatch a
  NotUnit :: UnitDispatch a

class UnitDispatchable a where
  unitDispatch :: UnitDispatch a

instance UnitDispatchable () where
  unitDispatch = IsUnit

instance {-# INCOHERENT #-} UnitDispatchable a where
  unitDispatch = NotUnit

mainWrapperDet :: forall a. (ShowFree a, FromHs a, UnitDispatchable (HsEquivalent a)) => IO (HsEquivalent a) -> IO ()
mainWrapperDet mx = do
  x <- mx
  case evalCurry (showFreeCurry (return (from x)) []) of
    Single x' -> case unitDispatch @(HsEquivalent a) of
                  IsUnit  -> return ()
                  NotUnit -> putStrLn x'
    _         -> error "mainWrapper: not a single result"

mainWrapperNDet :: forall a. (ShowFree a, ToHs a, UnitDispatchable a) => Curry (IO a) -> IO ()
mainWrapperNDet mx = do
  case evalCurry (ensureOneResult (bindIO mx (returnFunc $ \x -> return <$> showFreeCurry x []))) of
    Single x' -> x' >>= \a -> case unitDispatch @a of
                  IsUnit  -> return ()
                  NotUnit -> putStrLn a
    _         -> error "mainWrapper: not a single result"

unSingle :: MemoizedCurry.Tree n f l -> l
unSingle (Single x) = x
unSingle _ = error "mainWrapper: not a single result"

exprWrapperDet :: forall a. (ShowFree a, FromHs a)
               => (Tree.Tree String -> [String])
               -> HsEquivalent a -> IO ()
exprWrapperDet search a = case search $ evalCurryTree (showFreeCurry (fromHaskell a) []) of
  []  -> fail "**No value found"
  [s] -> putStrLn s
  _   -> error "internalError: More than on result from deterministic expression"

exprWrapperNDet :: forall a. ShowFree a
                => (Tree.Tree String -> [String])
                -> Bool -> [(String, Integer)] -> Bool
                -> Curry (CurryVal a, [VarInfo]) -> IO ()
exprWrapperNDet search optInt fvs b ca = printRes (search $ evalCurryTree extract) optInt
  where
    sortedFvs = map fst $ sortOn snd fvs

    printRes [] _     = fail "**No value found"
    printRes xs False = mapM_ putStrLn xs
    printRes xs True  = printInteractive xs

    printInteractive [] = putStrLn "No more values"
    printInteractive (x:xs) = do
      putStrLn x
      parseCommand xs

    parseCommand xs = do
      putStrLn "More Values? [yes/no/all]"
      input <- getLine
      case input of
        ""    -> printInteractive xs
        "y"   -> printInteractive xs
        "yes" -> printInteractive xs
        "n"   -> return ()
        "no"  -> return ()
        "a"   -> mapM_ putStrLn xs
        "all" -> mapM_ putStrLn xs
        _     -> parseCommand xs

    extract = do
      (va, ids) <- ca
      let swapped = map (\(x, y) -> (y, x)) fvs
      str <- showFreeCurry (Curry (return va)) swapped
      vs <- mapM (\(VarInfo @v i) -> showFreeCurry (Curry $ deref $ Curry $ return $ Var @v 0 i) swapped) ids
      let vs' = zipWith (\s n -> n ++ " = " ++ s) vs sortedFvs
      let str' = if null vs || not b then str else "{ " ++ intercalate "\n, " vs' ++ " } " ++ str
      return str'

data VarInfo = forall a. (ShowFree a, HasPrimitiveInfo a) => VarInfo Integer

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
  return (Val (a, ids))

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

{-# INLINE liftForeign1 #-}
liftForeign1 :: (ForeignType a, ForeignType b)
             => (Foreign a -> Foreign b) -> a -> b
liftForeign1 f x = fromForeign (f (toForeign x))

{-# INLINE liftForeign2 #-}
liftForeign2 :: (ForeignType a, ForeignType b, ForeignType c)
             => (Foreign a -> Foreign b -> Foreign c) -> a -> b -> c
liftForeign2 f x y = fromForeign (f (toForeign x) (toForeign y))

dollarBangNDImpl :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarBangNDImpl =
  returnFunc (\f ->
  returnFunc (\(Curry a) -> Curry (
    a >>= unCurry. \case
      Val x   -> x `seq` (f `app` return x)
      Var lvl i -> f `app` Curry (return (Var lvl i)))))

{-# INLINE condSeq #-}
condSeq :: Curry Bool -> Curry b -> Curry b
condSeq a b = do
  a' <- a
  if a' then b else mzero

{-# INLINABLE primitive1 #-}
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
      Val x -> return $ Val $ fromForeign $ hsF $ toForeign x
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

{-# SPECIALISE primitive2 :: (SBV Integer -> SBV Integer -> SBV Integer)
                          -> (Integer -> Integer -> Integer)
                          -> Curry (Integer :-> Integer :-> Integer) #-}
{-# SPECIALISE primitive2 :: (SBV Char -> SBV Char -> SBV Char)
                          -> (Char -> Char -> Char)
                          -> Curry (Char :-> Char :-> Char) #-}
{-# INLINABLE primitive2 #-}
primitive2 :: forall a b c
            . ( HasPrimitiveInfo a, ForeignType a
              , HasPrimitiveInfo b, ForeignType b
              , Curryable c, ForeignType c )
           => (SBV a -> SBV b -> SBV c)
           -> (Foreign a -> Foreign b -> Foreign c)
           -> Curry (a :-> b :-> c)
primitive2 sbvF hsF =
  return . Func $ share >=> \ca -> Curry (deref ca >>= \a ->
  return . Val . Func $ share >=> \cb -> Curry (deref cb >>= \b ->
    case (# a, b #) of
      (# Val x, Val y #) -> return $ Val $ fromForeign $ hsF (toForeign x) (toForeign y)
      _ -> case (# primitiveInfo @a, primitiveInfo @b, primitiveInfo @c #) of
            (# Primitive, Primitive, Primitive #) -> do
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
            _ -> error "internalError: primitive2: non-primitive type"))

{-# SPECIALISE primitive2 :: (SBV Integer -> SBV Integer -> SBV Integer)
                          -> (Integer -> Integer -> Integer)
                          -> Curry (Integer :-> Integer :-> Integer) #-}
{-# SPECIALISE primitive2 :: (SBV Char -> SBV Char -> SBV Char)
                          -> (Char -> Char -> Char)
                          -> Curry (Char :-> Char :-> Char) #-}
{-# INLINABLE primitive2Bool #-}
primitive2Bool :: forall a b c c'
            . ( HasPrimitiveInfo a, ForeignType a
              , HasPrimitiveInfo b, ForeignType b
              , HasPrimitiveInfo c', ForeignType c
              , HsEquivalent c' ~ c, Foreign c ~ Bool
              , FromHs c')
           => (SBV a -> SBV b -> SBV Bool)
           -> (Foreign a -> Foreign b -> Bool)
           -> Curry (a :-> b :-> c')
primitive2Bool sbvF hsF =
  return . Func $ share >=> \ca -> Curry (deref ca >>= \a ->
  return . Val . Func $ share >=> \cb -> Curry (deref cb >>= \b ->
    case (# a, b #) of
      (# Val x, Val y #) -> return $ Val $ from $ fromForeign $ hsF (toForeign x) (toForeign y)
      _ ->  case (# primitiveInfo @a, primitiveInfo @b #) of
              (# Primitive, Primitive #) -> do
                  NDState { .. } <- get
                  let c = sbvF (toSBV a) (toSBV b)
                  let csv' = foldr Set.insert constrainedVars (allVars a ++ allVars b)
                  let cst1 = insertConstraint c constraintStore
                  let cst2 = insertConstraint (sNot c) constraintStore
                  mplusLevel currentLevel
                    (do s' <- get
                        put (s' { constraintStore = cst1, constrainedVars = csv' })
                        checkConsistency
                        return (Val (from $ fromForeign True)))
                    (do s' <- get
                        put (s' { constraintStore = cst2, constrainedVars = csv' })
                        checkConsistency
                        return (Val (from $ fromForeign False)))
              _ -> error "internalError: primitive2: non-primitive type"))

allVars :: CurryVal a -> [Integer]
allVars (Var _ i) = [i]
allVars _         = []


-- allow defaulting of type variables with a kind that has a maximum of 10 arguments.
mkAllAnyDefinitions 10

{-
-- Testing Code:

-- If this does not compile, defaultiong does not work correctly.
-- This is commented out to not pollute the module.
-- Note that -XQuantifiedConstraints is required for this to work.

type Test = Any (Int -> Int -> Int)

-- test :: Curry Int
-- test = lengthC (nilC :: Curry (L Any Any))

data L f a = N | C (Curry (f a a)) (Curry (L f a))

nilC :: Curry (L f a)
nilC = return N

lengthC :: forall f a. (forall x y. (Curryable x, Curryable y) => Curryable (f x y), Curryable a)
        => Curry (L f a) -> Curry Int
lengthC l = l >>= \case
  N -> return 0
  C _ xs -> returnFunc (fmap (+1)) `app` lengthC xs

data Ln a = L (Curry (Ln a))
data LD a = LD (LD a)
type instance HsEquivalent Ln = LD
data Flat a = Flat (HsEquivalent a)

instance FromHs (Ln a) where
  from = undefined

test2 :: forall a. Curryable a => Curry (Flat (Ln a) :-> Int)
test2 = returnFunc (\x' -> x' >>= \(Flat (LD x)) ->
  let x_nd = fromHaskell x :: Curry (Ln a) -- needs type annotation
  in return 1)
-}

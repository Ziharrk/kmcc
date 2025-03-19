{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans            #-}
-- Common subexpression elinimation is disallowed due to unsafePerformIO stuff.
-- Experimentally verified that it is needed here.
{-# OPTIONS_GHC -fno-cse                #-}
module MemoizedCurry
  ( module MemoizedCurry
  , MonadShare(..)
  ) where

import qualified Data.Map                           as Map
import           Data.Map                           (Map)
import qualified Data.Set                           as Set
import           Data.Set                           (Set)
import           Data.IORef                         (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.SBV                           ( SBool,
                                                      SymVal(literal),
                                                      SBV,
                                                      constrain,
                                                      EqSymbolic((.===), (./==)),
                                                      sym, (.=>) )
import           Data.SBV.Control                   ( checkSatAssuming,
                                                      getValue,
                                                      CheckSatResult(..),
                                                      Query )
import           Control.Applicative                (Alternative(..))
import           Control.Monad                      (MonadPlus(..), ap, unless, msum)
import           Control.Monad.Fix                  (MonadFix(..), fix)
import           Control.Monad.Codensity            (Codensity(..), lowerCodensity)
import           Control.Monad.State                (StateT(..), MonadState(..), evalStateT, gets, modify)
import           Control.Monad.Trans                (MonadTrans(..))
import           Control.DeepSeq                    (NFData(..))
import           GHC.Magic                          (noinline)
import           GHC.Read                           (expectP)
import           GHC.Show                           (showLitString)
import           GHC.IO                             (unsafePerformIO)
import           GHC.Stack                          (HasCallStack)
import           Text.Read.Lex as L
import           Text.Read                          (ReadPrec, readPrec, reset, pfail, lexP, (+++), parens)
import           Unsafe.Coerce                      (unsafeCoerce)
import           Classes                            (MonadShare(..), MonadFree(..))
import qualified Tree

import Narrowable
import Solver

-- Changes to the paper:
-- - The performance optimization that was teasered
-- - Unification, lazy unification
-- - Set functions
-- - Constraint solving for operations on variables of primitive types
-- - Performance optimizations using `Codensity` where possible.

--------------------------------------------------------------------------------
-- Lifted function type

infixr 0 :->
newtype (:->) a b = Func (Curry a -> Curry b)

--------------------------------------------------------------------------------
-- Define a Tree that is polymorphic in the annotations at leafs and
data Tree l = Single l
            | Fail
            | Choice (Tree l) (Tree l)
  deriving (Functor, Show)

instance Applicative Tree where
  pure = Single
  Single f   <*> a = fmap f a
  Fail       <*> _ = Fail
  Choice l r <*> a = Choice (l <*> a) (r <*> a)

instance Monad Tree where
  Single a   >>= f = f a
  Fail       >>= _ = Fail
  Choice l r >>= f = Choice (l >>= f) (r >>= f)

instance MonadFix Tree where
  mfix f = case fix (f . unSingle) of
    Fail       -> Fail
    Single   x -> Single x
    Choice _ _ -> Choice (mfix (lT . f)) (mfix (rT . f))
    where
      unSingle (Single x) = x
      unSingle _ = error "Not a Leaf in mfix"
      lT (Choice x _) = x
      lT _ = error "Not a Node in mfix"
      rT (Choice _ x) = x
      rT _ = error "Not a Node in mfix"

-- For efficiency reasons, we will use Codensity here to get faster tree traversals.
-- We can convert freely between Search and Tree.
type Search = Codensity Tree

--------------------------------------------------------------------------------
-- Existential type that, so that we can later put data of arbirtrary types in the same structure.

data Untyped = forall a. Untyped a

typed :: Untyped -> a
typed (Untyped x) = unsafeCoerce x

untyped :: a -> Untyped
untyped = Untyped

--------------------------------------------------------------------------------
-- Defining the type for identifiers and
-- a function to generate a new identifier from its predecessor.

type ID = Integer

nextID :: ID -> ID
nextID = succ

--------------------------------------------------------------------------------
-- The Heap is just a map from identifiers to arbitrary values of the given type.

type Heap a = Map ID a

emptyHeap :: Heap a
emptyHeap = mempty

insertHeap :: ID -> a -> Heap a -> Heap a
insertHeap = Map.insert

lookupHeap :: ID -> Heap a -> Maybe a
lookupHeap = Map.lookup

--------------------------------------------------------------------------------
-- Our Monad also contains an extension where free variables
-- on primitive types (e.g. Numeric types) use constraint solving.
-- This improves performance for numerical operations on free variables,
-- since we can then instantiate variables less eagerly.
-- The implementation of those numerical operations is not shown in this file.
-- For a taste you could look at https://github.com/cau-placc/inversion-plugin,
-- where we did this in a different context.
-- To solve constraints, we use the SBV library,
-- which we use to check consistency of a set of constraints
-- and retrieve models that satisfy the constraints.

type Constraint = SBool

type ConstraintStore = [Constraint]

insertConstraint :: Constraint -> ConstraintStore -> ConstraintStore
insertConstraint = (:)

mkStateImplic :: ID -> SBool -> Query ()
mkStateImplic currID c = constrain $ varToSBV currID .=> c

{-# NOINLINE isConsistent #-}
isConsistent :: SolverState -> ID -> Set ID -> ConstraintStore -> Bool
isConsistent solSt currID parents cst
  | null cst  = True
  | otherwise = unsafePerformIO $ evalSymbolic solSt $ do
  mapM_ (mkStateImplic currID) cst
  checkSatAssuming (map varToSBV (currID : Set.toList parents)) >>= \case
    Sat -> return True
    _   -> return False

checkConsistency :: ND ()
checkConsistency = do
  s@NDState{..} <- get
  unless (isConsistent solverState branchID parentIDs constraintStore) mzero
  put s { constraintStore = [] }

toSBV :: SymVal a => CurryVal a -> SBV a
toSBV (Var i) = varToSBV i
toSBV (Val a) = literal a

varToSBV :: SymVal a => ID -> SBV a
varToSBV i = sym $ "x" ++ (if i < 0 then "n" else "") ++ show (abs i)

--------------------------------------------------------------------------------
-- Our State contains the stuff from the paper:
-- - idSupply for fresh IDs
-- - varHeap to store variable bindings
-- - branchID to distinguish branches
-- - parentIDs to see which branches this one originated from
-- Additionally, we need:
-- - the constraintStore to store constraints on primitive variables
-- - the currentLevel for set functions (see later)
-- - setComputation as a flag for set functions as well

data NDState = NDState {
    idSupply        :: IORef ID,
    varHeap         :: Heap Untyped,
    constraintStore :: ConstraintStore,
    constrainedVars :: Set ID,
    branchID        :: ID,
    parentIDs       :: Set ID,
    solverState     :: SolverState
  }

--------------------------------------------------------------------------------
-- Here are a few function to generate the initial state and
-- to generate new IDs in a monadic context

-- The weird fromEnum x + 1 is to make sure that the initial state is 1,
-- but the expression cannot float out of the function.
-- This ensures safety in the presence of unsafePerformIO.
{-# NOINLINE initialNDState #-}
initialNDState :: () -> NDState
initialNDState x = unsafePerformIO $ do
  r <- newIORef (toInteger (fromEnum x + 1))
  NDState r emptyHeap mempty Set.empty 0 Set.empty <$> startSolver

{-# NOINLINE freshIDFromState #-}
freshIDFromState :: NDState -> ID
freshIDFromState NDState { .. } = unsafePerformIO $
  atomicModifyIORef' idSupply (\j -> (nextID j, j))

-- {-# NOINLINE freshID #-}
-- NOINLINE not required, because freshIDFromState is the unsafe part
freshID :: MonadState NDState m => m ID
freshID = gets freshIDFromState

advanceNDState :: NDState -> NDState
advanceNDState s =
  let i = freshIDFromState s
      ps = Set.insert (branchID s) (parentIDs s)
  in s { branchID = i, parentIDs = ps }

--------------------------------------------------------------------------------
-- Before defining the actual memoizing curry monad, we define this intermediate monad,
-- which is basically the same as `StateT NDState (Tree Level (NDState, Level)) a`.
-- Meaning, we have a state monad withb the above state and a Tree that is annotated with:
-- - Level at Choices/Nodes (NOT Fingerprints), which are used for set functions
-- - Level and State at Failures, which are used for set functions as well.
-- We just use Codensity for performance.
-- Codensity with Reader is semantically equivalent to State.

newtype ND a = ND {
    unND :: StateT NDState Search a
  } deriving newtype ( Functor, Applicative, Monad, MonadFix
                     , MonadState NDState )

instance MonadFix m => MonadFix (Codensity m) where
    mfix f = Codensity $ \k -> mfix (lowerCodensity . f) >>= k

--------------------------------------------------------------------------------
-- Two convenience function to run an ND action to get the resulting tree.
-- The latter of these functions also gives the end state at each leaf.

evalND :: ND a -> NDState -> Tree a
evalND (ND a) s = lowerCodensity (evalStateT a s)

runND :: ND a -> NDState -> Tree (NDState, a)
runND a = evalND (a >>= \a' -> get >>= \st -> return (st, a'))

--------------------------------------------------------------------------------

instance Alternative ND where
  empty = mzero

  a <|> b = mplus a b

instance MonadPlus ND where
  mzero = ND $ lift $ lift Fail
  mplus (ND ma1) (ND ma2) = ND $ StateT $ \ndState1 -> Codensity $ \sc ->
    let i1 = freshIDFromState ndState1
        ps = Set.insert (branchID ndState1) (parentIDs ndState1)
        s1 = ndState1 { branchID = i1, parentIDs = ps }
        i2 = noinline const (freshIDFromState ndState1) i1
        s2 = ndState1 { branchID = i2, parentIDs = ps }
        t1 = runStateT ma1 s1
        t2 = runStateT ma2 s2
    in Choice (runCodensity t1 sc) (runCodensity t2 sc)

--------------------------------------------------------------------------------
-- We also need a Shareable constraint for the type of variables.
data CurryVal a = Val a
                | (HasPrimitiveInfo a) => Var ID

deriving instance Show a => Show (CurryVal a)

--------------------------------------------------------------------------------
-- Now the type of our final monad just uses `CurryVal` as the return type
-- together with the `ND` monad.

newtype Curry a = Curry {
    unCurry :: ND (CurryVal a)
  }

-------------------------------------------------------------------------------
-- For evalutation of Curry values we provide a funtion
-- that converts results to a non-polymorphic, "normal" tree.

evalCurry :: Curry a -> Tree a
evalCurry = fmap snd . runCurry

evalCurryWith :: Curry a -> NDState -> Tree a
evalCurryWith ma s = snd <$> runCurryWith ma s

evalCurryTreeWith :: Curry a -> NDState -> Tree.Tree a
evalCurryTreeWith ma s = snd <$> runCurryTreeWith ma s

evalCurryTree :: Curry a -> Tree.Tree a
evalCurryTree = fmap snd . runCurryTree

runCurry :: Curry a -> Tree (NDState, a)
runCurry (Curry ma) =
  unVal <$> runND (ma >>= \a -> checkConsistency >> return a) (initialNDState ())
  where
    unVal (s, Val x) = (s, x)
    unVal (_, Var _) = error "evalCurry: Variable"

runCurryWith :: Curry a -> NDState -> Tree (NDState, a)
runCurryWith (Curry ma) s' =
  unVal <$> runND (ma >>= \a -> checkConsistency >> return a) s'
  where
    unVal (s, Val x) = (s, x)
    unVal (_, Var _) = error "evalCurry: Variable"

runCurryTreeWith :: Curry a -> NDState -> Tree.Tree (NDState, a)
runCurryTreeWith ma s = convertTree $ runCurryWith ma s
  where
    convertTree (Single x)   = Tree.Leaf x
    convertTree Fail         = Tree.Empty
    convertTree (Choice l r) = Tree.Node (convertTree l) (convertTree r)

runCurryTree :: Curry a -> Tree.Tree (NDState, a)
runCurryTree ma = runCurryTreeWith ma (initialNDState ())

--------------------------------------------------------------------------------
-- Instantiation of variables follows a similar scheme as in the paper,
-- we only differentiate between Primitive types and NonPrimitives.
-- Non-Primitives are istantiated like in the paper.
-- Primitives are generated with a helper function that is the same for all primitive types.
-- In addition to generating a value for a primitive variable, we also add a constraint
-- that restricts the given variable to exactly that value.

instantiate :: forall a. HasPrimitiveInfo a => ID -> Curry a
instantiate i = Curry $
  case primitiveInfo @a of
    NoPrimitive -> msum $ flip map narrow $ \x -> unCurry $ do
                      sX <- x
                      modify (addToVarHeap i (return sX))
                      return sX
    Primitive   -> do
      s@NDState { constraintStore = cst } <- get
      put s { constraintStore = [], constrainedVars = Set.insert i (constrainedVars s) }
      narrowPrimitive cst i

--------------------------------------------------------------------------------
-- Instantiation of primitive variables is done by querying the SMT solver for
-- a single solution to all constraints
-- and then asking for the value of the required variable in that solution.
-- If no solution exists, there are no values that we can instantiate to.
-- If a solution existed, we return the value from the solution and
-- ask recursively for another solution where the variable is not equal to any previous solution.
-- For that, we accumulate inequalties on the given variable in the constraint store.

narrowPrimitive :: SymVal a => ConstraintStore -> ID -> ND (CurryVal a)
narrowPrimitive cst i = do
  NDState { .. } <- get
  let x = unsafePerformIO $ evalSymbolic solverState $ do
            mapM_ (mkStateImplic branchID) cst
            checkSatAssuming (map varToSBV (branchID : Set.toList parentIDs)) >>= \case
              Sat -> do
                v <- getValue (varToSBV i)
                return (Just v)
              _ -> return Nothing
  case x of
    Nothing -> mzero
    Just v  -> mplus (modify (modifyHeap v) >> return (Val v))
                     (narrowPrimitive [varToSBV i ./== toSBV (Val v)] i)
  where
    modifyHeap v s =
      addToVarHeap i (return v) s
        { constraintStore = insertConstraint (varToSBV i .=== toSBV (Val v)) (constraintStore s),
          constrainedVars = Set.insert i (constrainedVars s)
        }

--------------------------------------------------------------------------------
-- To define the Monad istance, we also use the deref function from the paper appendix.
-- That function recursively checks if a variable is bound.
-- The recursion is necessary, since a variable can be bound to another variable.
-- Bind and return are then easy to define.

deref :: Curry a -> ND (CurryVal a)
deref (Curry m) = do
  fl <- m
  case fl of
    v@(Var i) -> get >>= \ndState ->
      case lookupHeap i (varHeap ndState) of
        Nothing  -> return v
        Just res -> do
          let s' = advanceNDState ndState
          s' `seq` put s'
          deref (typed res)
    x@(Val _) -> return x

{-# INLINE[1] pureCurry #-}
pureCurry :: a -> Curry a
pureCurry = Curry . return . Val

{-# INLINE[1] bind #-}
bind :: Curry t -> (t -> Curry a) -> Curry a
ma `bind` f = Curry $ do
  a <- deref ma
  unCurry $ case a of
    Val x -> f x
    Var i -> instantiate i >>= f

instance Monad Curry where
  {-# INLINE (>>=) #-}
  (>>=) = bind

instance Functor Curry where
  {-# INLINE fmap #-}
  fmap = mapCurry

{-# INLINE mapCurry #-}
mapCurry :: (a -> b) -> Curry a -> Curry b
mapCurry f = (>>= return . f)

{-# INLINE mapCurryPartial #-}
mapCurryPartial :: (a -> b) -> Curry (a :-> b)
mapCurryPartial f = return $ Func $ \a -> mapCurry f a

{-# RULES
"ret/bind" forall x f. pureCurry x `bind` f = f x
"share/ret" forall x f. memo (pureCurry x) `bind` f = f (pureCurry x)
"bind/ret" forall x. x `bind` pureCurry = x
  #-}

instance Applicative Curry where
  {-# INLINE pure #-}
  pure = pureCurry
  (<*>) = ap

instance MonadState NDState Curry where
  get = Curry (gets Val)
  put s = Curry (put s >> return (Val ()))

instance MonadFix Curry where
  mfix f = Curry $ mfix (unCurry . f . unVal)
    where
      unVal (Val x) = x
      unVal _ = error "Not a Val in mfix"
--------------------------------------------------------------------------------
-- Non-determinisitic choice and failure are simply defined using the
-- `MonadPlus` instance of `ND`.

(?) :: Curry a -> Curry a -> Curry a
Curry ma ? Curry mb = Curry $ mplus ma mb

instance Alternative Curry where
  empty = Curry mzero
  (<|>) = (?)

instance MonadPlus Curry

--------------------------------------------------------------------------------
-- Free variables are created by getting a freshID and the current level to annotate the variable with.

instance MonadFree Curry where
  type FreeConstraints Curry a = HasPrimitiveInfo a
  free = do
    ndState <- get
    let key = freshIDFromState ndState
    freeWith key

freeWith :: HasPrimitiveInfo a => ID -> Curry a
freeWith = Curry . return . Var

--------------------------------------------------------------------------------
-- Sharing/memoization works as written in the paper,
-- with the optimization that a shareArgs on values that have been shared already are omitted.

instance MonadShare Curry where
  share :: Curry a -> Curry (Curry a)
  share = memo

{-# NOINLINE memo #-}
-- | Memorize a value or variable for explicit sharing.
memo :: Curry a  -> Curry (Curry a)
memo (Curry m) = Curry $ do
  ndState1 <- get
  -- ndState1 needs to be used inside the unsafePerformIO to prevent taskMap from
  -- floating out of the memo entirely.
  -- That would cause each memo to use the same IORef.
  let taskMap = unsafePerformIO
                    $ noinline const (newIORef Map.empty) ndState1
  return $ Val $ Curry $ do
    ndState2 <- get
    case lookupTaskResult taskMap (branchID ndState2) (parentIDs ndState2)  of
      Just (y, False) -> return y
      Just (y, True)  -> put (advanceNDState ndState2) >> return y
      Nothing -> do
        y <- m
        ndState3 <- get
        let wasND   = branchID ndState2 /= branchID ndState3
            insertID = if wasND
                          then branchID ndState3
                          else branchID ndState1
            insertH = insertHeap insertID (y, wasND)
        unsafePerformIO (atomicModifyIORef' taskMap (\x -> (insertH x, return y)))

--------------------------------------------------------------------------------
-- We could define lookupTaskResult exactly as in the paper,
-- but a small optimization is to get any value (we chose the maximium) from the
-- task result map after restricting the mep to all keys that are valid in the current branch.
-- There should only ever be one valid result for a branch.
-- Thus, choosing the maximum is ok.
-- This saves a few lookup operations for a single restrictKeys operation.

{-# NOINLINE lookupTaskResult #-}
lookupTaskResult :: IORef (Heap a) -> ID -> Set ID -> Maybe a
lookupTaskResult ref i s = do
  -- msum $ map (`Map.lookup` trMap) $ Set.toList allIDs
  (_k, v) <- Map.lookupMax trMap
  return v
  where
    allIDs = Set.insert i s
    trMap = Map.restrictKeys (unsafePerformIO (readIORef ref)) allIDs

--------------------------------------------------------------------------------
-- Unification proceeds as shown in the appendix of the paper.
-- Our class uses Generics so that we can easily derive instances.
-- Since we also provide a "lazy" unification operator,
-- we also have a function for that in the type class.

class Unifiable a where
  unifyWith :: (forall x. (HasPrimitiveInfo x, Unifiable x)
                        => Curry x -> Curry x -> Curry Bool)
            -> a -> a -> Curry Bool

  lazyUnifyVar :: a -> ID -> Curry Bool

--------------------------------------------------------------------------------
-- Unify itself is implemented as shown in the paper.

unify :: forall a. (HasPrimitiveInfo a, Unifiable a)
      => Curry a -> Curry a -> Curry Bool
ma1 `unify` ma2 = Curry $ do
  a1 <- deref ma1
  a2 <- deref ma2
  unCurry $ case (a1, a2) of
    (Var i1, Var i2)
      | i1 == i2 -> return True
      | Primitive <- primitiveInfo @a
        -> Curry $ do
          let cs = toSBV (Var i1) .=== toSBV a2
          modify (\s@NDState { .. } -> addToVarHeap i1 (Curry (return a2)) s
                    { constraintStore = insertConstraint cs constraintStore
                    , constrainedVars = Set.insert i1 (Set.insert i2 constrainedVars)
                    })
          _ <- checkConsistency
          return (Val True)
      | otherwise -> do
        modify (addToVarHeap i1 (Curry (return a2)))
        return True
    (Val x, Val y)  -> unifyWith unify x y
    (Var i1, Val y) -> unifyVar i1 y
    (Val x, Var i2) -> unifyVar i2 x
  where
    unifyVar :: ID -> a -> Curry Bool
    unifyVar i v = case primitiveInfo @a of
      NoPrimitive -> do
        s <- get
        let x = narrowConstr v
        sX <- x
        put (addToVarHeap i (return sX) s)
        _ <- unifyWith unify sX v
        return True
      Primitive   -> Curry $ do
        s <- get
        let cs1 = toSBV (Var i) .=== toSBV (Val v)
            s1 = addToVarHeap i (return v) s
                      { constraintStore = insertConstraint cs1 (constraintStore s)
                      , constrainedVars = Set.insert i (constrainedVars s)
                      }
        put s1 >> checkConsistency >> return (Val True)

(=:=) :: (HasPrimitiveInfo a, Unifiable a) => Curry (a :-> a :-> Bool)
(=:=) = return . Func $ \a -> return . Func $ \b -> unify a b

--------------------------------------------------------------------------------
-- Lazy unification is used to implement functional patterns.
-- It tries to look just at its first argument.
-- In consequence, it can happen that a variable is bound to a failing computations.
-- Such a unification succeeds with this lazy unification,
-- but fails in the "normal" stricter unification.

-- TODO: primitives
unifyL :: forall a. (HasPrimitiveInfo a, Unifiable a) => Curry a -> Curry a -> Curry Bool
ma1 `unifyL` ma2 = Curry $ do
  a1 <- deref ma1
  case a1 of
    Var i1
      | Primitive <- primitiveInfo @a -> do
        a2 <- deref ma2
        case a2 of
          Var i2 | i1 == i2  -> return (Val True)
                 | otherwise -> do
            let cs = toSBV (Var @a i1) .=== toSBV a2
            modify (\s@NDState { .. } -> addToVarHeap i1 (Curry (return a2)) s
                      { constraintStore = insertConstraint cs constraintStore
                      , constrainedVars = Set.insert i1 (Set.insert i2 constrainedVars)
                      })
            _ <- checkConsistency
            return (Val True)
          Val _ -> do
            let cs = toSBV (Var i1) .=== toSBV a2
            modify (\s@NDState { .. } -> addToVarHeap i1 (Curry (return a2)) s
                      { constraintStore = insertConstraint cs constraintStore
                      , constrainedVars = Set.insert i1 constrainedVars
                      })
            _ <- checkConsistency
            return (Val True)
      | otherwise -> unCurry $ do
        ma2' <- share ma2
        modify (addToVarHeap i1 ma2')
        return True
    Val x -> do
      a2 <- deref ma2
      unCurry $ case a2 of
        Var i2 -> lazyUnifyVar x i2
        Val y  -> unifyWith unifyL x y

addToVarHeap :: ID -> Curry a -> NDState -> NDState
addToVarHeap i v ndState =
  ndState { varHeap = insertHeap i (Untyped v) (varHeap ndState) }

(=:<=) :: (HasPrimitiveInfo a, Unifiable a) => Curry (a :-> a :-> Bool)
(=:<=) = return . Func $ \a -> return . Func $ \b -> unifyL a b

showShape :: Tree y -> String
showShape (Single _) = "Single"
showShape Fail = "Fail"
showShape (Choice l r) = "Choice " ++ " (" ++ showShape l ++ ") (" ++ showShape r ++ ")"

-- Lift a tree computation to a Curry computation
treeToCurry :: Tree a -> Curry a
treeToCurry = Curry . ND . lift . lift . fmap Val

mkList :: [Curry a] -> ListC a
mkList = foldr (\e xs -> ConsC e (return xs)) NilC

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Everything beyond this pint is not really interesting.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

instance Unifiable Integer where
  unifyWith _ x y = if x == y then return True else mzero

  lazyUnifyVar n i = Curry $ do
      let cs = toSBV (Var i) .=== toSBV (Val n)
      modify (\s@NDState { .. } -> addToVarHeap i (return n) s
                  { constraintStore = insertConstraint cs constraintStore
                  , constrainedVars = Set.insert i constrainedVars
                  })
      _ <- checkConsistency
      return (Val True)

--------------------------------------------------------------------------------
-- Some example data types.

data ListC a = NilC | ConsC (Curry a) (Curry (ListC a))

type family HsEquivalent (a :: k) = (b :: k) | b -> a
type instance HsEquivalent (a b) = HsEquivalent a (HsEquivalent b)

class ToHs a where
  to :: a -> Curry (HsEquivalent a)

class FromHs a where
  from :: HasCallStack => HsEquivalent a -> a
  elimFlat :: a -> a

class ShowFree a where
  showsFreePrec :: Int -> a -> ShowSFree
  showFree :: a -> [(Integer, String)] -> Curry String
  showFree x fm = snd $ showsFreePrec 0 x (fm, return "")

showsStringCurry :: String -> ShowSFree
showsStringCurry s (fm, x) = (fm, fmap (++s) x)

type ShowSFree = ([(Integer, String)], Curry String) -> ([(Integer, String)], Curry String)

-- Class to pull all non-determinisim to the top
class NormalForm a where
  nfWith :: (forall x. NormalForm x => Curry x -> ND (Either (CurryVal x) (HsEquivalent x)))
         -> a -> ND (Either (CurryVal a) (HsEquivalent a))

class ShowTerm a where
  showTerm :: Int -> HsEquivalent a -> ShowS
  showTermList :: [HsEquivalent a] -> ShowS
  showTermList = showList__ (showTerm 0)
    where
      showList__ _     []     s = "[]" ++ s
      showList__ showx (x:xs) s = '[' : showx x (showl xs)
        where
          showl []     = ']' : s
          showl (y:ys) = ',' : showx y (showl ys)


class ReadTerm a where
  readTerm :: ReadPrec (HsEquivalent a)
  readTermList :: ReadPrec [HsEquivalent a]
  readTermList = readTermListDefault

readTermListDefault :: ReadTerm a => ReadPrec [HsEquivalent a]
readTermListDefault = list readTerm
  where
    list readx =
      parens
      ( do expectP (L.Punc "[")
           listRest False +++ listNext
      )
      where
        listRest started =
          do L.Punc c <- lexP
             case c of
                "]"           -> return []
                "," | started -> listNext
                _             -> pfail
        listNext =
          do x  <- reset readx
             xs <- listRest True
             return (x:xs)

class NFDataC a where
  rnfC :: (HsEquivalent a ~ a') => a' -> ()

class ( ToHs a, FromHs a, Unifiable a, NormalForm a
      , HasPrimitiveInfo a, ShowFree a, NFDataC a
      , ReadTerm a, ShowTerm a ) => Curryable a

type instance HsEquivalent Integer = Integer

instance ToHs Integer where
  to = return

instance FromHs Integer where
  from = id
  {-# INLINE elimFlat #-}
  elimFlat = id

instance ShowFree Integer where
  showsFreePrec _ x = showsStringCurry (show x)

instance NormalForm Integer where
  nfWith _ !x = return (Right x)

instance ShowTerm Integer where
  showTerm = showsPrec

instance ReadTerm Integer where
  readTerm = readPrec

instance NFDataC Integer where
  rnfC = rnf

instance Curryable Integer

type instance HsEquivalent Double = Double

instance ToHs Double where
  to = return

instance FromHs Double where
  from = id
  elimFlat = id

instance HasPrimitiveInfo Double where
  primitiveInfo = Primitive

instance Unifiable Double where
  unifyWith _ x y = if x == y then return True else mzero

  lazyUnifyVar n i = Curry $ do
      let cs = toSBV (Var i) .=== toSBV (Val n)
      modify (\s@NDState { .. } -> addToVarHeap i (return n) s
                  { constraintStore = insertConstraint cs constraintStore
                  , constrainedVars = Set.insert i constrainedVars
                  })
      _ <- checkConsistency
      return (Val True)

instance NormalForm Double where
  nfWith _ !x = return (Right x)

instance ShowFree Double where
  showsFreePrec _ x = showsStringCurry (show x)

instance ShowTerm Double where
  showTerm = showsPrec

instance ReadTerm Double where
  readTerm = readPrec

instance NFDataC Double where
  rnfC = rnf

instance Curryable Double

type instance HsEquivalent Char = Char

instance ToHs Char where
  to = return

instance FromHs Char where
  from = id
  elimFlat = id

instance HasPrimitiveInfo Char where
  primitiveInfo = Primitive

instance Unifiable Char where
  unifyWith _ x y = if x == y then return True else mzero

  lazyUnifyVar n i = Curry $ do
      let cs = toSBV (Var i) .=== toSBV (Val n)
      modify (\s@NDState { .. } -> addToVarHeap i (return n) s
                  { constraintStore = insertConstraint cs constraintStore
                  , constrainedVars = Set.insert i constrainedVars
                  })
      _ <- checkConsistency
      return (Val True)

instance NormalForm Char where
  nfWith _ !x = return (Right x)

instance ShowFree Char where
  showsFreePrec _ x = showsStringCurry (show x)

instance ShowTerm Char where
  showTerm = showsPrec
  showTermList cs = showChar '"' . showLitString cs . showChar '"'

instance ReadTerm Char where
  readTerm = readPrec
  readTermList =
    parens
    ( do L.String s <- lexP
         return s
     +++
      readTermListDefault
    )

instance NFDataC Char where
  rnfC = rnf

instance Curryable Char

type instance HsEquivalent IO = IO

instance ToHs (IO a) where
  to = error "FFI Error: 'To' Conversion on IO"

instance FromHs a => FromHs (IO a) where
  from x = from <$> x
  elimFlat = id

instance HasPrimitiveInfo (IO a) where
  primitiveInfo = NoPrimitive

instance Narrowable (IO a) where
  narrow = error "narrowing an IO action is not possible"
  narrowConstr _ = error "narrowing an IO action is not possible"

instance Unifiable (IO a) where
  unifyWith _ _ _ = error "unifying an IO action is not possible"
  lazyUnifyVar _ _ = error "lazily unifying an IO action is not possible"

instance NormalForm (IO a) where
  nfWith _ !x = return (Left (Val x))

instance ShowFree (IO a) where
  showsFreePrec _ _ = showsStringCurry "<<IO>>"

instance ShowTerm (IO a) where
  showTerm _ _ = showString "<<IO>>"

instance ReadTerm (IO a) where
  readTerm = pfail

instance NFDataC a => NFDataC (IO a) where
  rnfC a = a `seq` ()

instance Curryable a => Curryable (IO a)

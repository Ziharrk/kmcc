{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.State as P
import qualified Control.Exception as P
import qualified Data.List as P
import qualified GHC.IO.Exception as P
import qualified GHC.Magic as P
import qualified System.IO.Unsafe as P
import BasicDefinitions
import Prelude ((.), ($), ($!), (+), (-), (*), (/), (==), (<=),(>>=))

-- -----------------------------------------------------------------------------
-- higher-order representation
-- -----------------------------------------------------------------------------

type Apply (a :: k1 -> k2) = (a :: k1 -> k2)

type ApplyND (a :: k1 -> k2) = (a :: k1 -> k2)

apply# :: (a -> b) -> a -> b
apply# = ($)

applyND# :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
applyND# = returnFunc (returnFunc . app)

-- -----------------------------------------------------------------------------
-- Int representation
-- -----------------------------------------------------------------------------

type Int# = P.Integer

type IntND# = P.Integer

eqInt# :: Int -> Int -> Bool
eqInt# = liftForeign2 (==)

eqIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND BoolND))
eqIntND# = BasicDefinitions.liftConvert2 eqInt#

ltEqInt# :: Int -> Int -> Bool
ltEqInt# = liftForeign2 (<=)

ltEqIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND BoolND))
ltEqIntND# = BasicDefinitions.liftConvert2 ltEqInt#

-- -----------------------------------------------------------------------------
-- Float representation
-- -----------------------------------------------------------------------------

type Float# = P.Double

type FloatND# = P.Double

eqFloat# :: Float -> Float -> Bool
eqFloat# = liftForeign2 (==)

eqFloatND# :: Curry (LiftedFunc FloatND (LiftedFunc FloatND BoolND))
eqFloatND# = BasicDefinitions.liftConvert2 eqFloat#

ltEqFloat# :: Float -> Float -> Bool
ltEqFloat# = liftForeign2 (<=)

ltEqFloatND# :: Curry (LiftedFunc FloatND (LiftedFunc FloatND BoolND))
ltEqFloatND# = BasicDefinitions.liftConvert2 ltEqFloat#

-- ---------------------------------------------------------------------------
-- Char representation
-- ---------------------------------------------------------------------------

type Char# = P.Char

type CharND# = P.Char

eqChar# :: Char -> Char -> Bool
eqChar# = liftForeign2 (==)

eqCharND# :: Curry (LiftedFunc CharND (LiftedFunc CharND BoolND))
eqCharND# = BasicDefinitions.liftConvert2 eqChar#

ltEqChar# :: Char -> Char -> Bool
ltEqChar# = liftForeign2 (<=)

ltEqCharND# :: Curry (LiftedFunc CharND (LiftedFunc CharND BoolND))
ltEqCharND# = BasicDefinitions.liftConvert2 ltEqChar#

-- ---------------------------------------------------------------------------
-- IO representation
-- ---------------------------------------------------------------------------

type IO# = P.IO

type IOND# = P.IO

-- ---------------------------------------------------------------------------
-- Function representation
-- ---------------------------------------------------------------------------

type CArrow# = (->)

type CArrowND# = BasicDefinitions.LiftedFunc

-- -----------------------------------------------------------------------------
-- ShowFree for Lists, Tuples and Strings (overlaps!)
-- -----------------------------------------------------------------------------

data ListInfo = FullList | FreeElem | FreeList
  deriving (P.Eq, P.Ord)

instance Curryable a => ShowFree (CListND a) where
  showsFreePrec _ CListND s = BasicDefinitions.showsStringCurry "[]" s
  showsFreePrec _ xs      s = (P.fst s,) $ do
    (ys, b) <- gatherContents (`BasicDefinitions.showFree` P.fst s) (`BasicDefinitions.showFree` P.fst s) P.id xs
    case b of
      FullList -> P.snd $ BasicDefinitions.showsStringCurry ("["  P.++ P.intercalate "," ys P.++ "]") s
      FreeElem -> P.snd $ BasicDefinitions.showsStringCurry ("["  P.++ P.intercalate "," ys P.++ "]") s
      FreeList -> P.snd $ BasicDefinitions.showsStringCurry (          P.intercalate ":" ys         ) s

instance {-# OVERLAPS #-} ShowFree (CListND CharND) where
  showsFreePrec _ CListND s = BasicDefinitions.showsStringCurry "\"\"" s
  showsFreePrec _ xs      s = (P.fst s,) $ do
    (ys, b) <- gatherContents (\c -> P.return [c]) (P.return . P.show) (\s -> "'" P.++ P.show (P.head s) P.++ "'") xs
    case b of
      FullList -> P.snd $ BasicDefinitions.showsStringCurry ("\"" P.++ P.concat ys          P.++ "\"") s
      FreeElem -> P.snd $ BasicDefinitions.showsStringCurry ("["  P.++ P.intercalate "," ys P.++ "]" ) s
      FreeList -> P.snd $ BasicDefinitions.showsStringCurry (          P.intercalate ":" ys          ) s

-- first arg: use this to show the element if it is a full list
-- second arg: use this to show the element if there is a free element/list
-- third arg: use this to convert an arg produced by the first arg to one by the second arg.
gatherContents :: Curryable a => (a -> Curry P.String) -> (a -> Curry P.String) -> (P.String -> P.String)
               -> CListND a -> BasicDefinitions.Curry ([P.String], ListInfo)
gatherContents _ _ _ CListND = P.return ([], FullList)
gatherContents f g h (CConsFlat# x xs) = gatherContents f g h (CConsND (fromHaskell x) (fromHaskell xs))
gatherContents f g h (CConsND x xs) = BasicDefinitions.Curry $ do
  c <- deref x
  rest <- deref xs
  unCurry (case rest of
    BasicDefinitions.Var _ i -> case c of
      BasicDefinitions.Var _ j ->
        P.return (["_" P.++ P.show j, "_" P.++ P.show i], FreeList)
      BasicDefinitions.Val _ u -> do
        e <- g u
        P.return ([e, "_" P.++ P.show i], FreeList)
    BasicDefinitions.Val _ v -> do
      (ys, b) <- gatherContents f g h v
      case c of
        BasicDefinitions.Var _ j -> case b of
          FullList -> do
            let ys' = P.fmap h ys
            P.return (("_" P.++ P.show j) : ys', FreeElem)
          _        ->
            P.return (("_" P.++ P.show j) : ys , b)
        BasicDefinitions.Val _ u -> case b of
          FullList -> do
            e <- f u
            P.return (e : ys, b)
          _        -> do
            e <- g u
            P.return (e : ys, b))

instance ShowFree CUnitND where
  showsFreePrec _ CUnitND = BasicDefinitions.showsStringCurry "()"

instance (Curryable x, Curryable y) => ShowFree (CTuple2ND x y) where
  showsFreePrec _ (CTuple2ND x y) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple2Flat# x y) = showsFreePrec p' (CTuple2ND (fromHaskell x) (fromHaskell y))

instance (Curryable x, Curryable y, Curryable z) => ShowFree (CTuple3ND x y z) where
  showsFreePrec _ (CTuple3ND x y z) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple3Flat# x y z) = showsFreePrec p' (CTuple3ND (fromHaskell x) (fromHaskell y) (fromHaskell z))

instance (Curryable x, Curryable y, Curryable z, Curryable w) => ShowFree (CTuple4ND x y z w) where
  showsFreePrec _ (CTuple4ND x y z w) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple4Flat# x y z w) = showsFreePrec p' (CTuple4ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v) => ShowFree (CTuple5ND x y z w v) where
  showsFreePrec _ (CTuple5ND x y z w t) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple5Flat# x y z w t) = showsFreePrec p' (CTuple5ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u) => ShowFree (CTuple6ND x y z w v u) where
  showsFreePrec _ (CTuple6ND x y z w t s) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple6Flat# x y z w t s) = showsFreePrec p' (CTuple6ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t) => ShowFree (CTuple7ND x y z w v u t) where
  showsFreePrec _ (CTuple7ND x y z w t s r) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple7Flat# x y z w t s r) = showsFreePrec p' (CTuple7ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s) => ShowFree (CTuple8ND x y z w v u t s) where
  showsFreePrec _ (CTuple8ND x y z w t s r q) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple8Flat# x y z w t s r q) = showsFreePrec p' (CTuple8ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r) => ShowFree (CTuple9ND x y z w v u t s r) where
  showsFreePrec _ (CTuple9ND x y z w t s r q p) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple9Flat# x y z w t s r q p) = showsFreePrec p' (CTuple9ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q) => ShowFree (CTuple10ND x y z w v u t s r q) where
  showsFreePrec _ (CTuple10ND x y z w t s r q p o) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple10Flat# x y z w t s r q p o) = showsFreePrec p' (CTuple10ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p) => ShowFree (CTuple11ND x y z w v u t s r q p) where
  showsFreePrec _ (CTuple11ND x y z w t s r q p o n) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple11Flat# x y z w t s r q p o n) = showsFreePrec p' (CTuple11ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p, Curryable o) => ShowFree (CTuple12ND x y z w v u t s r q p o) where
  showsFreePrec _ (CTuple12ND x y z w t s r q p o n m) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 m .
    showsStringCurry "," .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple12Flat# x y z w t s r q p o n m) = showsFreePrec p' (CTuple12ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n) (fromHaskell m))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p, Curryable o, Curryable n) => ShowFree (CTuple13ND x y z w v u t s r q p o n) where
  showsFreePrec _ (CTuple13ND x y z w t s r q p o n m l) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 l .
    showsStringCurry "," .
    showsFreePrecCurry 0 m .
    showsStringCurry "," .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple13Flat# x y z w t s r q p o n m l) = showsFreePrec p' (CTuple13ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n) (fromHaskell m) (fromHaskell l))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p, Curryable o, Curryable n, Curryable m) => ShowFree (CTuple14ND x y z w v u t s r q p o n m) where
  showsFreePrec _ (CTuple14ND x y z w t s r q p o n m l k) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 k .
    showsStringCurry "," .
    showsFreePrecCurry 0 l .
    showsStringCurry "," .
    showsFreePrecCurry 0 m .
    showsStringCurry "," .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple14Flat# x y z w t s r q p o n m l k) = showsFreePrec p' (CTuple14ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n) (fromHaskell m) (fromHaskell l) (fromHaskell k))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p, Curryable o, Curryable n, Curryable m, Curryable l) => ShowFree (CTuple15ND x y z w v u t s r q p o n m l) where
  showsFreePrec _ (CTuple15ND x y z w t s r q p o n m l k j) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 j .
    showsStringCurry "," .
    showsFreePrecCurry 0 k .
    showsStringCurry "," .
    showsFreePrecCurry 0 l .
    showsStringCurry "," .
    showsFreePrecCurry 0 m .
    showsStringCurry "," .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple15Flat# x y z w t s r q p o n m l k j) = showsFreePrec p' (CTuple15ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n) (fromHaskell m) (fromHaskell l) (fromHaskell k) (fromHaskell j))

-- -----------------------------------------------------------------------------
-- Foreign Conversion
-- -----------------------------------------------------------------------------

instance ForeignType a => ForeignType (CList a) where
  type Foreign (CList a) = [Foreign a]
  toForeign CList = []
  toForeign (CCons x xs) = toForeign x : toForeign xs
  fromForeign [] = CList
  fromForeign (x:xs) = CCons (fromForeign x) (fromForeign xs)

instance (ForeignType a, ForeignType b) => ForeignType (CTuple2 a b) where
  type Foreign (CTuple2 a b) = (Foreign a, Foreign b)
  toForeign (CTuple2 a b) = (toForeign a, toForeign b)
  fromForeign (a, b) = CTuple2 (fromForeign a) (fromForeign b)

instance ForeignType Bool where
  type Foreign Bool = P.Bool
  toForeign False = P.False
  toForeign True = P.True
  fromForeign P.False = False
  fromForeign P.True = True

instance ForeignType CUnit where
  type Foreign CUnit = ()
  toForeign CUnit = ()
  fromForeign () = CUnit

-- -----------------------------------------------------------------------------
-- Primitive operations: General
-- -----------------------------------------------------------------------------

amp# :: Bool -> Bool -> Bool
amp# True  True  = True
amp# False False = False
amp# _     _     = False

ampND# :: Curry (LiftedFunc BoolND (LiftedFunc BoolND BoolND))
ampND# = BasicDefinitions.liftConvert2 amp#

eqcolonlteqND# :: Curryable a => Curry (LiftedFunc a (LiftedFunc a BoolND))
eqcolonlteqND# = BasicDefinitions.returnFunc (\a1 -> BasicDefinitions.returnFunc
  (BasicDefinitions.unifyL a1 P.>=> (BasicDefinitions.fromHaskell . fromForeign)))

eqcoloneqND# :: Curryable a => Curry (LiftedFunc a (LiftedFunc a BoolND))
eqcoloneqND# = BasicDefinitions.returnFunc (\a1 -> BasicDefinitions.returnFunc
  (BasicDefinitions.unify a1 P.>=> (BasicDefinitions.fromHaskell . fromForeign)))

cond# :: Bool -> a -> a
cond# True a = a
cond# _    _ = failed

condND# :: Curry (LiftedFunc BoolND (LiftedFunc a a))
condND# = returnFunc (\a -> a >>= \case
  TrueND -> returnFunc P.id
  FalseND -> failedND)

dollarbang# :: (a -> b) -> a -> b
dollarbang# = ($!)

dollarbangND# :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarbangND# = BasicDefinitions.dollarBangNDImpl

dollarbangbang# :: (a -> b) -> a -> b
dollarbangbang# = ($!)

dollarbangbangND# :: BasicDefinitions.Curryable a => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarbangbangND# = BasicDefinitions.dollarBangBangNDImpl

dollarhashhash# :: (a -> b) -> a -> b
dollarhashhash# = ($!)

dollarhashhashND# :: BasicDefinitions.Curryable a => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarhashhashND# = BasicDefinitions.dollarHashHashNDImpl

ensureNotFree# :: a -> a
ensureNotFree# !x = x

ensureNotFreeND# :: Curry (LiftedFunc a a)
ensureNotFreeND# = P.return (Func (\x -> x P.>>= P.noinline P.return))

-- -----------------------------------------------------------------------------
-- Primitive operations: Characters
-- -----------------------------------------------------------------------------

primuscoreord# :: Char -> Int
primuscoreord# = P.fromIntegral . P.fromEnum

primuscoreordND# :: Curry (LiftedFunc CharND IntND)
primuscoreordND# = BasicDefinitions.liftConvert1 primuscoreord#

primuscorechr# :: Int -> Char
primuscorechr# = P.toEnum . P.fromIntegral

primuscorechrND# :: Curry (LiftedFunc IntND CharND)
primuscorechrND# = BasicDefinitions.liftConvert1 primuscorechr#

-- -----------------------------------------------------------------------------
-- Primitive operations: Arithmetics
-- -----------------------------------------------------------------------------

primuscoreshowCharLiteral# :: Char -> CList Char
primuscoreshowCharLiteral# = liftForeign1 P.show

primuscoreshowIntLiteral# :: Int -> CList Char
primuscoreshowIntLiteral# = liftForeign1 P.show

primuscoreshowFloatLiteral# :: Float -> CList Char
primuscoreshowFloatLiteral# = liftForeign1 P.show

primuscoreshowStringLiteral# :: CList Char -> CList Char
primuscoreshowStringLiteral# = liftForeign1 P.show

primuscorereadCharLiteral# :: CList Char -> CList (CTuple2 Char (CList Char))
primuscorereadCharLiteral# = liftForeign1 P.reads

primuscorereadStringLiteral# :: CList Char -> CList (CTuple2 (CList Char) (CList Char))
primuscorereadStringLiteral# = liftForeign1 P.reads

primuscorereadNatLiteral# :: CList Char -> CList (CTuple2 Int (CList Char))
primuscorereadNatLiteral# = liftForeign1 P.reads

primuscorereadFloatLiteral# :: CList Char -> CList (CTuple2 Float (CList Char))
primuscorereadFloatLiteral# = liftForeign1 P.reads

plusInt# :: Int -> Int -> Int
plusInt# = (+)

minusInt# :: Int -> Int -> Int
minusInt# = (-)

timesInt# :: Int -> Int -> Int
timesInt# = (*)

primuscoreplusFloat# :: Float -> Float -> Float
primuscoreplusFloat# = (+)

primuscoreminusFloat# :: Float -> Float -> Float
primuscoreminusFloat# = (-)

primuscoretimesFloat# :: Float -> Float -> Float
primuscoretimesFloat# = (*)

negateFloat# :: Float -> Float
negateFloat# = P.negate

primuscoreintToFloat# :: Int -> Float
primuscoreintToFloat# = P.fromIntegral

primuscoredivFloat# :: Float -> Float -> Float
primuscoredivFloat# = (/)

divInt# :: Int -> Int -> Int
divInt# = P.div

modInt# :: Int -> Int -> Int
modInt# = P.mod

quotInt# :: Int -> Int -> Int
quotInt# = P.quot

remInt# :: Int -> Int -> Int
remInt# = P.rem

primuscoretruncateFloat# :: Float -> Int
primuscoretruncateFloat# = P.truncate

primuscoreroundFloat# :: Float -> Int
primuscoreroundFloat# = P.round

primuscorelogFloat# :: Float -> Float
primuscorelogFloat# = P.log

primuscoreexpFloat# :: Float -> Float
primuscoreexpFloat# = P.exp

primuscoresqrtFloat# :: Float -> Float
primuscoresqrtFloat# = P.sqrt

primuscoresinFloat# :: Float -> Float
primuscoresinFloat# = P.sin

primuscorecosFloat# :: Float -> Float
primuscorecosFloat# = P.cos

primuscoretanFloat# :: Float -> Float
primuscoretanFloat# = P.tan

primuscoreasinFloat# :: Float -> Float
primuscoreasinFloat# = P.asin

primuscoreacosFloat# :: Float -> Float
primuscoreacosFloat# = P.acos

primuscoreatanFloat# :: Float -> Float
primuscoreatanFloat# = P.atan

primuscoreasinhFloat# :: Float -> Float
primuscoreasinhFloat# = P.asinh

primuscoreacoshFloat# :: Float -> Float
primuscoreacoshFloat# = P.acosh

primuscoreatanhFloat# :: Float -> Float
primuscoreatanhFloat# = P.atanh

primuscoresinhFloat# :: Float -> Float
primuscoresinhFloat# = P.sinh

primuscorecoshFloat# :: Float -> Float
primuscorecoshFloat# = P.cosh

primuscoretanhFloat# :: Float -> Float
primuscoretanhFloat# = P.tanh

primuscoreshowCharLiteralND# :: Curry (LiftedFunc CharND (CListND CharND))
primuscoreshowCharLiteralND# = liftConvert1 (liftForeign1 P.show)

primuscoreshowIntLiteralND# :: Curry (LiftedFunc IntND (CListND CharND))
primuscoreshowIntLiteralND# = liftConvert1 (liftForeign1 P.show)

primuscoreshowFloatLiteralND# :: Curry (LiftedFunc FloatND (CListND CharND))
primuscoreshowFloatLiteralND# = liftConvert1 (liftForeign1 P.show)

primuscoreshowStringLiteralND# :: Curry (LiftedFunc (CListND CharND) (CListND CharND))
primuscoreshowStringLiteralND# = liftConvert1 (liftForeign1 P.show)

primuscorereadCharLiteralND# :: Curry (LiftedFunc (CListND CharND) (CListND (CTuple2ND CharND (CListND CharND))))
primuscorereadCharLiteralND# = liftConvert1 (liftForeign1 P.reads)

primuscorereadStringLiteralND# :: Curry (LiftedFunc (CListND CharND) (CListND (CTuple2ND (CListND CharND) (CListND CharND))))
primuscorereadStringLiteralND# = liftConvert1 (liftForeign1 P.reads)

primuscorereadNatLiteralND# :: Curry (LiftedFunc (CListND CharND) (CListND (CTuple2ND IntND (CListND CharND))))
primuscorereadNatLiteralND# = liftConvert1 (liftForeign1 P.reads)

primuscorereadFloatLiteralND# :: Curry (LiftedFunc (CListND CharND) (CListND (CTuple2ND FloatND (CListND CharND))))
primuscorereadFloatLiteralND# = liftConvert1 (liftForeign1 P.reads)

plusIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND IntND))
plusIntND# = liftConvert2 (+)

minusIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND IntND))
minusIntND# = liftConvert2 (-)

timesIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND IntND))
timesIntND# = liftConvert2 (*)

primuscoreplusFloatND# :: Curry (LiftedFunc FloatND (LiftedFunc FloatND FloatND))
primuscoreplusFloatND# = liftConvert2 (+)

primuscoreminusFloatND# :: Curry (LiftedFunc FloatND (LiftedFunc FloatND FloatND))
primuscoreminusFloatND# = liftConvert2 (-)

primuscoretimesFloatND# :: Curry (LiftedFunc FloatND (LiftedFunc FloatND FloatND))
primuscoretimesFloatND# = liftConvert2 (*)

negateFloatND# :: Curry (LiftedFunc FloatND FloatND)
negateFloatND# = liftConvert1 P.negate

primuscoreintToFloatND# :: Curry (LiftedFunc IntND FloatND)
primuscoreintToFloatND# = liftConvert1 P.fromIntegral

primuscoredivFloatND# :: Curry (LiftedFunc FloatND (LiftedFunc FloatND FloatND))
primuscoredivFloatND# = liftConvert1 (/)

divIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND IntND))
divIntND# = liftConvert1 P.div

modIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND IntND))
modIntND# = liftConvert1 P.mod

quotIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND IntND))
quotIntND# = liftConvert1 P.quot

remIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND IntND))
remIntND# = liftConvert1 P.rem

primuscoretruncateFloatND# :: Curry (LiftedFunc FloatND IntND)
primuscoretruncateFloatND# = liftConvert1 P.truncate

primuscoreroundFloatND# :: Curry (LiftedFunc FloatND IntND)
primuscoreroundFloatND# = liftConvert1 P.round

primuscorelogFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscorelogFloatND# = liftConvert1 P.log

primuscoreexpFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoreexpFloatND# = liftConvert1 P.exp

primuscoresqrtFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoresqrtFloatND# = liftConvert1 P.sqrt

primuscoresinFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoresinFloatND# = liftConvert1 P.sin

primuscorecosFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscorecosFloatND# = liftConvert1 P.cos

primuscoretanFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoretanFloatND# = liftConvert1 P.tan

primuscoreasinFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoreasinFloatND# = liftConvert1 P.asin

primuscoreacosFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoreacosFloatND# = liftConvert1 P.acos

primuscoreatanFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoreatanFloatND# = liftConvert1 P.atan

primuscoreasinhFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoreasinhFloatND# = liftConvert1 P.asinh

primuscoreacoshFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoreacoshFloatND# = liftConvert1 P.acosh

primuscoreatanhFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoreatanhFloatND# = liftConvert1 P.atanh

primuscoresinhFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoresinhFloatND# = liftConvert1 P.sinh

primuscorecoshFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscorecoshFloatND# = liftConvert1 P.cosh

primuscoretanhFloatND# :: Curry (LiftedFunc FloatND FloatND)
primuscoretanhFloatND# = liftConvert1 P.tanh

-- -----------------------------------------------------------------------------
-- Primitive operations: IO stuff
-- -----------------------------------------------------------------------------

bindIO# :: IO a -> (a -> IO b) -> IO b
bindIO# = (>>=)

bindIOND# :: Curry (LiftedFunc (IO a) (LiftedFunc (LiftedFunc a (IO b)) (IO b)))
bindIOND# = BasicDefinitions.bindIONDImpl

returnIO# :: a -> IO a
returnIO# = P.pure

returnIOND# :: Curry (LiftedFunc a (IO a))
returnIOND# = BasicDefinitions.returnIONDImpl

getChar# :: IO Char
getChar# = P.getChar

getCharND# :: Curry (IO Char)
getCharND# = P.return P.getChar

primuscoreputChar# :: Char -> IO CUnit
primuscoreputChar# = liftForeign1 P.putChar

primuscoreputCharND# :: Curry (LiftedFunc CharND (IO CUnitND))
primuscoreputCharND# = liftConvertIO1 primuscoreputChar#

primuscorereadFile# :: CList Char -> IO (CList Char)
primuscorereadFile# = liftForeign1 P.readFile

primuscorereadFileND# :: Curry (LiftedFunc (CListND CharND) (IO (CListND CharND)))
primuscorereadFileND# = liftConvertIO1 primuscorereadFile#

primuscorewriteFile# :: CList Char -> CList Char -> IO CUnit
primuscorewriteFile# = liftForeign2 P.writeFile

primuscorewriteFileND# :: Curry (LiftedFunc (CListND CharND) (LiftedFunc (CListND CharND) (IO CUnitND)))
primuscorewriteFileND# = liftConvertIO2 primuscorewriteFile#

primuscoreappendFile# :: CList Char -> CList Char -> IO CUnit
primuscoreappendFile# = liftForeign2 P.appendFile

primuscoreappendFileND# :: Curry (LiftedFunc (CListND CharND) (LiftedFunc (CListND CharND) (IO CUnitND)))
primuscoreappendFileND# = liftConvertIO2 primuscoreappendFile#

instance ForeignType IOError where
  type Foreign IOError = P.IOException
  fromForeign (P.IOError _ P.UserError _ s _ _) = UserError (fromForeign s)
  fromForeign (P.IOError _ P.OtherError _ s _ _)
    | "FAILERR_ " `P.isPrefixOf` s = FailError    (fromForeign (P.drop 9 s))
    | "NDERR_ "   `P.isPrefixOf` s = NondetError  (fromForeign (P.drop 7 s))
    | "IOERR_ "   `P.isPrefixOf` s = IOError      (fromForeign (P.drop 7 s))
  fromForeign (P.IOError _ _ _ s _ _) = IOError (fromForeign s)

  toForeign (IOError s)     = P.IOError P.Nothing P.OtherError "" ("IOERR_ " P.++ toForeign s) P.Nothing P.Nothing
  toForeign (UserError s)   = P.IOError P.Nothing P.UserError "" (toForeign s) P.Nothing P.Nothing
  toForeign (FailError s)   = P.IOError P.Nothing P.OtherError "" ("FAILERR_ " P.++ toForeign s) P.Nothing P.Nothing
  toForeign (NondetError s) = P.IOError P.Nothing P.OtherError "" ("NDERR_ " P.++ toForeign s) P.Nothing P.Nothing

primuscoreioError# :: IOError -> IO a
primuscoreioError# err = P.throw (toForeign err)

primuscoreioErrorND# :: Curry (LiftedFunc IOErrorND (IO a))
primuscoreioErrorND# = P.return (Func (\err -> do
  e <- BasicDefinitions.ensureOneResult (toHaskell err :: Curry IOError)
  P.return (P.throw (toForeign e :: P.IOException))))

catch# :: IO a -> (IOError -> IO a) -> IO a
catch# io cont = P.catch io (cont . fromForeign)

catchND# :: Curry (LiftedFunc (IO a) (LiftedFunc (LiftedFunc IOErrorND (IO a)) (IO a)))
catchND# = P.return (Func (\ioND -> P.return (Func (\contND -> do
  io <- BasicDefinitions.ensureOneResult ioND
  Func cont <- BasicDefinitions.ensureOneResult contND
  let res = P.unsafePerformIO (P.unsafeInterleaveIO (P.try io))
  case res of
    P.Left e -> cont (fromHaskell (fromForeign e))
    P.Right x -> P.return (P.return x)))))

-- -----------------------------------------------------------------------------
-- Primitive operations: Exception handling
-- -----------------------------------------------------------------------------

primuscoreerror# :: CList Char -> a
primuscoreerror# xs = P.error (toForeign xs)

primuscoreerrorND# :: Curry (LiftedFunc (CListND CharND) a)
primuscoreerrorND# = P.return $ Func $ toHaskell M.>=> \xs' -> primuscoreerror# xs'

failed# :: a
failed# = P.throw Failed

failedND# :: Curry a
failedND# = P.mzero

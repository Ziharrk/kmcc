{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
import qualified Prelude as P
import Prelude ((.), ($), ($!), (+), (-), (*), (/), (>>=))

-- -----------------------------------------------------------------------------
-- higher-order representation
-- -----------------------------------------------------------------------------

type Apply (a :: k1 -> k2) = (a :: k1 -> k2)

apply# :: (a -> b) -> a -> b
apply# = ($)

-- -----------------------------------------------------------------------------
-- Int representation
-- -----------------------------------------------------------------------------

type Int# = P.Integer

eqInt# :: Int# -> Int# -> Bool
eqInt# = (P.==)

ltEqInt# :: Int# -> Int# -> Bool
ltEqInt# = (P.<=)

-- -----------------------------------------------------------------------------
-- Float representation
-- -----------------------------------------------------------------------------

type Float# = P.Double

eqFloat# :: Float# -> Float# -> Bool
eqFloat# = (P.==)

ltEqFloat# :: Float# -> Float# -> Bool
ltEqFloat# = (P.<=)

-- ---------------------------------------------------------------------------
-- Char representation
-- ---------------------------------------------------------------------------

type Char# = P.Char

eqChar# :: Char# -> Char# -> Bool
eqChar# = (P.==)

ltEqChar# :: Char# -> Char# -> Bool
ltEqChar# = (P.<=)

-- ---------------------------------------------------------------------------
-- IO representation
-- ---------------------------------------------------------------------------

type IO# = P.IO

-- ---------------------------------------------------------------------------
-- Function representation
-- ---------------------------------------------------------------------------

type CArrow# = (->)


-- -----------------------------------------------------------------------------
-- Primitive operations: General
-- -----------------------------------------------------------------------------

instance ConvertHs a => ConvertHs (CList a) where
  type HsEquivalent (CList a) = [HsEquivalent a]
  to [] = CList
  to (x:xs) = CCons (to x) (to xs)
  from CList = []
  from (CCons x xs) = from x : from xs

instance (ConvertHs a, ConvertHs b) => ConvertHs (CTuple2 a b) where
  type HsEquivalent (CTuple2 a b) = (HsEquivalent a, HsEquivalent b)
  to (x, y) = CTuple2 (to x) (to y)
  from (CTuple2 x y) = (from x, from y)

instance ConvertHs CUnit where
  type HsEquivalent CUnit = ()
  to () = CUnit
  from CUnit = ()

amp# :: Bool -> Bool -> Bool
amp# = (P.&)

eqcolonlteq# :: a -> a -> Bool
eqcolonlteq# a1 a2 = if a1 == a2 then True else failed

eqcoloneq# :: a -> a -> Bool
eqcoloneq# a1 a2 = if a1 == a2 then True else failed

cond# :: Bool -> a -> a
cond# b a = if b then a else failed

dollarbang# :: (a -> b) -> a -> b
dollarbang# = ($!)

dollarbangbang# :: (a -> b) -> a -> b
dollarbangbang# = ($!)

dollarhashhash# :: (a -> b) -> a -> b
dollarhashhash# = ($!)

ensureNotFree# :: a -> a
ensureNotFree# !x = x

-- -----------------------------------------------------------------------------
-- Primitive operations: Characters
-- -----------------------------------------------------------------------------

primuscoreord# :: Char -> Int
primuscoreord# = P.fromEnum

primuscorechr# :: Int -> Char
primuscorechr# = P.toEnum

-- -----------------------------------------------------------------------------
-- Primitive operations: Arithmetics
-- -----------------------------------------------------------------------------

primuscoreshowCharLiteral# :: Char -> CList Char
primuscoreshowCharLiteral# = to . P.show . from

primuscoreshowIntLiteral# :: Int -> CList Char
primuscoreshowIntLiteral# = to . P.show . from

primuscoreshowFloatLiteral# :: Float -> CList Char
primuscoreshowFloatLiteral# = to . P.show . from

primuscoreshowStringLiteral# :: CList Char -> CList Char
primuscoreshowStringLiteral# = to . P.show . from

primuscorereadCharLiteral# :: CList Char -> CList (CTuple2 Char (CList Char))
primuscorereadCharLiteral# = to . P.reads . from

primuscorereadStringLiteral# :: CList Char -> CList (CTuple2 (CList Char) (CList Char))
primuscorereadStringLiteral# = to . P.reads . from

primuscorereadNatLiteral# :: CList Char -> CList (CTuple2 Int (CList Char))
primuscorereadNatLiteral# = to . P.reads . from

primuscorereadFloatLiteral# :: CList Char -> CList (CTuple2 Float (CList Char))
primuscorereadFloatLiteral# = to . P.reads . from

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

-- -----------------------------------------------------------------------------
-- Primitive operations: IO stuff
-- -----------------------------------------------------------------------------

instance ConvertHs IOError where
  type HsEquivalent IOError = P.IOError
  to (P.IOError _ P.UserError _ s _ _) = UserError (to s)
  to (P.IOError _ P.UserError _ s _ _) = UserError (to s)
  to (P.IOError _ _ _ s _ _) = IOError (to s)

  from (IOError s) = P.IOError P.Nothing P.SystemError Nothing (from s) Nothing Nothing
  from (UserError s) = P.IOError P.Nothing P.UserError Nothing (from s) Nothing Nothing
  from (FailError s) = P.IOError P.Nothing P.UserError Nothing (from s) Nothing Nothing
  from (NondetError s) = P.IOError P.Nothing P.UserError Nothing (from s) Nothing Nothing

bindIO# :: IO a -> (a -> IO b) -> IO b
bindIO# = (>>=)

returnIO# :: a -> IO a
returnIO# = P.pure

getChar# :: IO Char
getChar# = P.getChar

primuscoreputChar# :: Char -> IO CUnit
primuscoreputChar# = P.fmap to P.putChar

primuscorereadFile# :: CList Char -> IO (CList Char)
primuscorereadFile# = P.fmap to P.readFile . from

primuscorewriteFile# :: CList Char -> CList Char -> IO CUnit
primuscorewriteFile# x = P.fmap to . P.writeFile (from x) . from

primuscoreappendFile# :: CList Char -> CList Char -> IO CUnit
primuscoreappendFile# x = P.fmap to . P.appendFile (from x) . from

primuscoreioError# :: IOError -> IO a
primuscoreioError# = P.ioError . from

catch# :: IO a -> (IOError -> IO a) -> IO a
catch# act cont = P.catch act (cont . to)

-- -----------------------------------------------------------------------------
-- Primitive operations: Exception handling
-- -----------------------------------------------------------------------------

primuscoreerror# :: CList Char -> a
primuscoreerror# = P.error . from

failed# :: a
failed# = P.throw Failed

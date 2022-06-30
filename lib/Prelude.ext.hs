{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Exception as P
import qualified GHC.IO.Exception as P
import BasicDefinitions
import Prelude ((.), ($), ($!), (+), (-), (*), (/), (>>=))

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
eqInt# x y = to (x P.== y)

eqIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND BoolND))
eqIntND# = BasicDefinitions.liftConvert2 eqInt#

ltEqInt# :: Int -> Int -> Bool
ltEqInt# x y = to (x P.<= y)

ltEqIntND# :: Curry (LiftedFunc IntND (LiftedFunc IntND BoolND))
ltEqIntND# = BasicDefinitions.liftConvert2 ltEqInt#

-- -----------------------------------------------------------------------------
-- Float representation
-- -----------------------------------------------------------------------------

type Float# = P.Double

type FloatND# = P.Double

eqFloat# :: Float -> Float -> Bool
eqFloat# x y = to (x P.== y)

eqFloatND# :: Curry (LiftedFunc FloatND (LiftedFunc FloatND BoolND))
eqFloatND# = BasicDefinitions.liftConvert2 eqFloat#

ltEqFloat# :: Float -> Float -> Bool
ltEqFloat# x y = to (x P.<= y)

ltEqFloatND# :: Curry (LiftedFunc FloatND (LiftedFunc FloatND BoolND))
ltEqFloatND# = BasicDefinitions.liftConvert2 ltEqFloat#

-- ---------------------------------------------------------------------------
-- Char representation
-- ---------------------------------------------------------------------------

type Char# = P.Char

type CharND# = P.Char

eqChar# :: Char -> Char -> Bool
eqChar# x y = to (x P.== y)

eqCharND# :: Curry (LiftedFunc CharND (LiftedFunc CharND BoolND))
eqCharND# = BasicDefinitions.liftConvert2 eqChar#

ltEqChar# :: Char -> Char -> Bool
ltEqChar# x y = to (x P.<= y)

ltEqCharND# :: Curry (LiftedFunc CharND (LiftedFunc CharND BoolND))
ltEqCharND# = BasicDefinitions.liftConvert2 ltEqChar#

-- ---------------------------------------------------------------------------
-- IO representation
-- ---------------------------------------------------------------------------

type IO# = P.IO

type IO_ND# = P.IO

-- ---------------------------------------------------------------------------
-- Function representation
-- ---------------------------------------------------------------------------

type CArrow# = (->)

type CArrowND# = (->)

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

instance ConvertHs Bool where
  type HsEquivalent Bool = P.Bool
  to P.True = True
  to P.False = False
  from True = P.True
  from False = P.False

amp# :: Bool -> Bool -> Bool
amp# True  True  = True
amp# False False = False
amp# _     _     = False

ampND# :: Curry (LiftedFunc BoolND (LiftedFunc BoolND BoolND))
ampND# = BasicDefinitions.liftConvert2 amp#

eqcolonlteq# :: a -> a -> Bool
eqcolonlteq# a1 a2 = P.undefined -- TODO

eqcolonlteqND# :: Curry (LiftedFunc a (LiftedFunc a BoolND))
eqcolonlteqND# = P.undefined -- TODO

eqcoloneq# :: a -> a -> Bool
eqcoloneq# a1 a2 = P.undefined -- TODO

eqcoloneqND# :: Curry (LiftedFunc a (LiftedFunc a BoolND))
eqcoloneqND# = P.undefined -- TODO

cond# :: Bool -> a -> a
cond# True a = a
cond# _    _ = failed

condND# :: Curry (LiftedFunc BoolND (LiftedFunc a a))
condND# = returnFunc (\a -> a >>= \case
  TrueND -> returnFunc P.id
  FalseND -> failedND)

dollarbang# :: (a -> b) -> a -> b
dollarbang# = ($!)

dollarbang# :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarbang# = P.undefined -- TODO

dollarbangbang# :: (a -> b) -> a -> b
dollarbangbang# = ($!)

dollarbangbang# :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarbangbang# = P.undefined -- TODO

dollarhashhash# :: (a -> b) -> a -> b
dollarhashhash# = ($!)

dollarhashhashND# :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarhashhashND# = P.undefined -- TODO

ensureNotFree# :: a -> a
ensureNotFree# !x = x

ensureNotFreeND# :: Curry (LiftedFunc a a)
ensureNotFreeND# = P.undefined -- TODO

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

  from (IOError s) = P.IOError P.Nothing P.SystemError "" (from s) P.Nothing P.Nothing
  from (UserError s) = P.IOError P.Nothing P.UserError "" (from s) P.Nothing P.Nothing
  from (FailError s) = P.IOError P.Nothing P.UserError "" (from s) P.Nothing P.Nothing
  from (NondetError s) = P.IOError P.Nothing P.UserError "" (from s) P.Nothing P.Nothing

bindIO# :: IO a -> (a -> IO b) -> IO b
bindIO# = (>>=)

bindIOND# :: Curry (LiftedFunc (IO a) (LiftedFunc (LiftedFunc a (IO b)) (IO b)))
bindIOND# = P.undefined -- TODO

returnIO# :: a -> IO a
returnIO# = P.pure

returnIOND :: Curry (LiftedFunc a (IO a))
returnIOND = P.undefined -- TODO

getChar# :: IO Char
getChar# = P.getChar

getCharND# :: Curry (IO Char)
getCharND# = return P.getChar

primuscoreputChar# :: Char -> IO CUnit
primuscoreputChar# x = P.fmap to (P.putChar x)

primuscoreputChar# :: Curry (LiftedFunc CharND (IO (CListND CharND)))
primuscoreputChar# = liftConvertIO1 primuscoreputChar#

primuscorereadFile# :: CList Char -> IO (CList Char)
primuscorereadFile# = P.fmap to . P.readFile . from

primuscorereadFileND# :: Curry (LiftedFunc (CListND CharND) (IO (CListND CharND)))
primuscorereadFileND# = liftConvertIO1 primuscorereadFileND#

primuscorewriteFile# :: CList Char -> CList Char -> IO CUnit
primuscorewriteFile# x = P.fmap to . P.writeFile (from x) . from

primuscorewriteFileND# :: Curry (LiftedFunc (CListND CharND) (LiftedFunc (CListND CharND) (IO CUnitND)))
primuscorewriteFileND# = liftConvertIO2 primuscorewriteFile#

primuscoreappendFile# :: CList Char -> CList Char -> IO CUnit
primuscoreappendFile# x = P.fmap to . P.appendFile (from x) . from

primuscoreappendFileND# :: Curry (LiftedFunc (CListND CharND) (LiftedFunc (CListND CharND) (IO CUnitND)))
primuscoreappendFileND# = liftConvertIO2 primuscoreappendFile#

primuscoreioError# :: IOError -> IO a
primuscoreioError# = P.ioError . from

primuscoreioErrorND# :: Curry (LiftedFunc IOErrorND (IO a))
primuscoreioErrorND = liftConvertIO1 primuscoreioError#

catch# :: IO a -> (IOError -> IO a) -> IO a
catch# act cont = P.catch act (cont . to)

catchND# :: Curry (LiftedFunc (IO a) (LiftedFunc (LiftedFunc IOErrorND (IO a)) (IO a)))
catchND# = liftConvertIO2 catch#

-- -----------------------------------------------------------------------------
-- Primitive operations: Exception handling
-- -----------------------------------------------------------------------------

primuscoreerror# :: CList Char -> a
primuscoreerror# = P.error . from

primuscoreerrorND# :: Curry (LiftedFunc (CListND CharND) a)
primuscoreerrorND# = returnFunc (\x -> x >>= P.error . from)

failed# :: a
failed# = P.throw Failed

failedND :: Curry a
failedND = P.mzero

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

dollarbangND# :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarbangND# = P.undefined -- TODO

dollarbangbang# :: (a -> b) -> a -> b
dollarbangbang# = ($!)

dollarbangbangND# :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarbangbangND# = P.undefined -- TODO

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

type instance HsEquivalent IOError = P.IOException

bindIO# :: IO a -> (a -> IO b) -> IO b
bindIO# = (>>=)

bindIOND# :: Curry (LiftedFunc (IO a) (LiftedFunc (LiftedFunc a (IO b)) (IO b)))
bindIOND# = P.undefined -- TODO

returnIO# :: a -> IO a
returnIO# = P.pure

returnIOND# :: Curry (LiftedFunc a (IO a))
returnIOND# = P.undefined -- TODO

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
primuscorereadFileND# = liftConvertIO1 primuscorereadFileND#

primuscorewriteFile# :: CList Char -> CList Char -> IO CUnit
primuscorewriteFile# = liftForeign2 P.writeFile

primuscorewriteFileND# :: Curry (LiftedFunc (CListND CharND) (LiftedFunc (CListND CharND) (IO CUnitND)))
primuscorewriteFileND# = liftConvertIO2 primuscorewriteFile#

primuscoreappendFile# :: CList Char -> CList Char -> IO CUnit
primuscoreappendFile# = liftForeign2 P.appendFile

primuscoreappendFileND# :: Curry (LiftedFunc (CListND CharND) (LiftedFunc (CListND CharND) (IO CUnitND)))
primuscoreappendFileND# = liftConvertIO2 primuscoreappendFile#

primuscoreioError# :: IOError -> IO a
primuscoreioError# = P.undefined -- TODO

primuscoreioErrorND# :: Curry (LiftedFunc IOErrorND (IO a))
primuscoreioErrorND# = P.undefined -- TODO

catch# :: IO a -> (IOError -> IO a) -> IO a
catch# act cont = P.undefined -- TODO

catchND# :: Curry (LiftedFunc (IO a) (LiftedFunc (LiftedFunc IOErrorND (IO a)) (IO a)))
catchND# = P.undefined -- TODO

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

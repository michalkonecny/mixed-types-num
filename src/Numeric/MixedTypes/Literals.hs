{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Literals
    Description :  Fixed-type numeric literals, conversions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    This module defines fixed-type integer and rational literals.
    This is useful when deriving the type of an expression bottom-up.
    Eg we would not be able to write @1 < x@
    when the type of @<@ does not force the two sides to be of the
    same type.  We would need to write eg @(1::Integer) < x@ with
    Prelude's generic literals.

    Moreover, convenient conversion functions are provided for
    the most common numeric types.  Thus one can say eg:

    * @take (int 1)@
    * @integer (length list)@.
    * @double 0.5@

    To avoid integer overflow, no aritmetic operations return 'Int'.
    Nevertheless, one can usually mix 'Int' with other types in expressions.

    Any approximate arithmetic, ie arithmetic involving Doubles, returns
    values of type 'Double'.
    'Double' values cannot be easily converted to exact
    types such as 'Rational' or 'Integer' so that all such
    conversions are clearly visible as labelled as inexact.
-}

module Numeric.MixedTypes.Literals
(
  -- * Fixed-type literals
  fromInteger, fromRational
  -- * Generalised if-then-else
  , HasIfThenElse(..), HasIfThenElseSameType
  -- * Convenient conversions
  , CanBeInteger, integer, integers, HasIntegers, fromInteger_, HasIntegersWithSample, fromIntegerWithSample
  , CanBeInt, int, ints
  , CanBeRational, rational, rationals, HasRationals, fromRational_, HasRationalsWithSample, fromRationalWithSample
  , CanBeDouble, double, doubles
  , ConvertibleExactly(..), convertExactly, convertExactlyTargetSample
  , ConvertResult, ConvertError, convError
  -- * Prelude List operations versions without Int
  , (!!), length, replicate, take, drop, splitAt
  -- * Testing support functions
  , T(..), tInt, tInteger, tCNInteger, tRational, tCNRational, tDouble
  , tBool, tMaybe, tMaybeBool, tMaybeMaybeBool
  , specCanBeInteger
  , printArgsIfFails2
  -- * Helper functions
  , convertFirst, convertSecond
  , convertFirstUsing, convertSecondUsing
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

-- import Data.Convertible (Convertible(..), convert, ConvertResult, ConvertError, convError)
import Data.Convertible.Base
import Data.Convertible.Instances.Num ()

import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck
-- import Control.Exception (evaluate)

import Numeric.CollectErrors (CN)
import Control.CollectErrors

{-| Replacement for 'Prelude.fromInteger' using the RebindableSyntax extension.
    This version of fromInteger arranges that integer literals
    are always of type 'Integer'.
-}
fromInteger :: Integer -> Integer
fromInteger = id

{-| Replacement for 'Prelude.fromRational' using the RebindableSyntax extension.
    This version of fromRational arranges that rational literals are
    always of type 'Rational'. -}
fromRational :: Rational -> Rational
fromRational = id

{-|
  Restore if-then-else with RebindableSyntax
-}
class HasIfThenElse b t where
  type IfThenElseType b t
  type IfThenElseType b t = t
  ifThenElse :: b -> t -> t -> IfThenElseType b t

type HasIfThenElseSameType b t =
  (HasIfThenElse b t, IfThenElseType b t ~ t)

instance HasIfThenElse Bool t where
  ifThenElse b e1 e2
    | b = e1
    | otherwise = e2

instance 
  (HasIfThenElse b t, CanTakeErrors es (IfThenElseType b t), CanBeErrors es) 
  =>
  (HasIfThenElse (CollectErrors es b) t)
  where
  type IfThenElseType (CollectErrors es b) t = IfThenElseType b t
  ifThenElse (CollectErrors (Just b) es) e1 e2 = 
    takeErrors es $ ifThenElse b e1 e2
  ifThenElse (CollectErrors _ es) _ _ = 
    takeErrorsNoValue es

_testIf1 :: String
_testIf1 = if True then "yes" else "no"

{---- Numeric conversions -----}

type CanBeInteger t = ConvertibleExactly t Integer
integer :: (CanBeInteger t) => t -> Integer
integer = convertExactly
integers :: (CanBeInteger t) => [t] -> [Integer]
integers = map convertExactly

type HasIntegers t = ConvertibleExactly Integer t
fromInteger_ :: (HasIntegers t) => Integer -> t
fromInteger_ = convertExactly

type HasIntegersWithSample t = ConvertibleExactly (t, Integer) t
fromIntegerWithSample :: (HasIntegersWithSample t) => t -> Integer -> t
fromIntegerWithSample sampleT n = convertExactly (sampleT, n)

(!!) :: (CanBeInteger n) => [a] -> n -> a
list !! ix = List.genericIndex list (integer ix)
-- list !! ix = List.genericIndex list (P.max 0 ((integer ix) P.- 1)) -- deliberately wrong - test the test!

length :: (Foldable t) => t a -> Integer
length = integer . P.length

replicate :: (CanBeInteger n) => n -> a -> [a]
replicate = P.replicate . int . integer

take :: (CanBeInteger n) => n -> [a] -> [a]
take = P.take . int . integer

drop :: (CanBeInteger n) => n -> [a] -> [a]
drop = P.drop . int . integer

splitAt :: (CanBeInteger n) => n -> [a] -> ([a],[a])
splitAt = P.splitAt . int . integer

{-|
  HSpec properties that each implementation of CanBeInteger should satisfy.
 -}
specCanBeInteger ::
  (CanBeInteger t, Show t, Arbitrary t) =>
  T t -> Spec
specCanBeInteger (T typeName :: T t) =
  describe "generic list index (!!)" $ do
    it (printf "works using %s index" typeName) $ do
      property $ \ (x :: t) -> let xi = integer x in (xi P.>= 0) ==> ([0..xi] !! x) ==$ xi
  where
  (==$) = printArgsIfFails2 "==" (P.==)

printArgsIfFails2 ::
  (Testable prop, Show a, Show b) =>
  String -> (a -> b -> prop) -> (a -> b -> Property)
printArgsIfFails2 relName rel a b =
  counterexample argsReport $ a `rel` b
  where
  argsReport =
    "FAILED REL: (" ++ show a ++ ") " ++ relName ++ " (" ++ show b ++ ")"

type CanBeInt t = ConvertibleExactly t Int
int :: (CanBeInt t) => t -> Int
int = convertExactly
ints :: (CanBeInt t) => [t] -> [Int]
ints = map convertExactly

type CanBeRational t = ConvertibleExactly t Rational
rational :: (CanBeRational t) => t -> Rational
rational = convertExactly
rationals :: (CanBeRational t) => [t] -> [Rational]
rationals = map convertExactly

type HasRationals t = ConvertibleExactly Rational t
fromRational_ :: (HasRationals t) => Rational -> t
fromRational_ = convertExactly

type HasRationalsWithSample t = ConvertibleExactly (t, Rational) t
fromRationalWithSample :: (HasRationalsWithSample t) => t -> Rational -> t
fromRationalWithSample sampleT q = convertExactly (sampleT, q)

type CanBeDouble t = Convertible t Double
double :: (CanBeDouble t) => t -> Double
double = convert
doubles :: (CanBeDouble t) => [t] -> [Double]
doubles = map convert

{-|
Define our own ConvertibleExactly since convertible is too relaxed for us.
For example, convertible allows conversion from Rational to Integer,
rounding to nearest integer.  We prefer to allow only exact conversions.
-}
class ConvertibleExactly t1 t2 where
  safeConvertExactly :: t1 -> ConvertResult t2
  default safeConvertExactly :: (Convertible t1 t2) => t1 -> ConvertResult t2
  safeConvertExactly = safeConvert

convertExactly :: (ConvertibleExactly t1 t2) => t1 -> t2
convertExactly a =
  case safeConvertExactly a of
    Right v -> v
    Left err -> error (show err)

convertExactlyTargetSample :: (ConvertibleExactly t1 t2) => t2 -> t1 -> t2
convertExactlyTargetSample _sample = convertExactly

instance ConvertibleExactly Integer Integer -- use CVT instance by default
instance ConvertibleExactly Int Integer

instance ConvertibleExactly Int Int where
  safeConvertExactly n = Right n
instance ConvertibleExactly Rational Rational where
  safeConvertExactly q = Right q

instance ConvertibleExactly Integer Int
instance ConvertibleExactly Int Rational
instance ConvertibleExactly Integer Rational

instance ConvertibleExactly Integer Double where
  safeConvertExactly n =
    do
    d <- safeConvert n
    case P.properFraction d of
      (m, fr) | m P.== n P.&& fr P.== (double 0) -> return d
      _ -> convError "Integer could not be exactly converted to Double" n

instance ConvertibleExactly Int Double where
  safeConvertExactly n =
    do
    d <- safeConvert n
    case P.properFraction d of
      (m, fr) | m P.== n P.&& fr P.== (double 0) -> return d
      _ -> convError "Int could not be exactly converted to Double" n

instance ConvertibleExactly Double Double where
  safeConvertExactly d = Right d

{-- we deliberately do not allow converions from Double to any other type --}

{-- auxiliary type and functions for specifying type(s) to use in tests  --}

{-|
  A runtime representative of type @t@.
  Used for specialising polymorphic tests to concrete types.
-}
data T t = T String

tInt :: T Int
tInt = T "Int"

tInteger :: T Integer
tInteger = T "Integer"

tCNInteger :: T (CN Integer)
tCNInteger = T "(CN Integer)"

tRational :: T Rational
tRational = T "Rational"

tCNRational :: T (CN Rational)
tCNRational = T "(CN Rational)"

tDouble :: T Double
tDouble = T "Double"

tBool :: T Bool
tBool = T "Bool"

tMaybe :: T t -> T (Maybe t)
tMaybe (T tName) = T ("(Maybe " ++ tName ++ ")")

tMaybeBool :: T (Maybe Bool)
tMaybeBool = tMaybe tBool

tMaybeMaybeBool :: T (Maybe (Maybe Bool))
tMaybeMaybeBool = tMaybe tMaybeBool

{---- Auxiliary functions ----}

convertFirstUsing ::
  (a -> b -> b) {-^ conversion function -} ->
  (b -> b -> c) {-^ same-type operation -} ->
  (a -> b -> c) {-^ mixed-type operation -}
convertFirstUsing conv op a b = op (conv a b) b

convertSecondUsing ::
  (a -> b -> a) {-^ conversion function -} ->
  (a -> a -> c) {-^ same-type operation -} ->
  (a -> b -> c) {-^ mixed-type operation -}
convertSecondUsing conv op a b = op a (conv a b)

convertFirst ::
  (ConvertibleExactly a b) =>
  (b -> b -> c) {-^ same-type operation -} ->
  (a -> b -> c) {-^ mixed-type operation -}
convertFirst = convertFirstUsing (\ a _ -> convertExactly a)

convertSecond ::
  (ConvertibleExactly b a) =>
  (a -> a -> c) {-^ same-type operation -} ->
  (a -> b -> c) {-^ mixed-type operation -}
convertSecond = convertSecondUsing (\ _ b -> convertExactly b)

-- instance
--   (ConvertibleExactly t1 t2, CanBeErrors es)
--   =>
--   ConvertibleExactly t1 (CollectErrors es t2)
--   where
--   safeConvertExactly = fmap pure . safeConvertExactly
--

$(declForTypes
  [[t| Bool |], [t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance (ConvertibleExactly $t t, Monoid es) => ConvertibleExactly $t (CollectErrors es t) where
      safeConvertExactly = fmap pure . safeConvertExactly
  |]))

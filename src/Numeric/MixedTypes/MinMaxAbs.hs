{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.MinMaxAbs
    Description :  Bottom-up typed min, max and abs
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.MinMaxAbs
(
  -- * Minimum and maximum
  CanMinMax, CanMinMaxAsymmetric(..), CanMinMaxThis, CanMinMaxSameType
  , minimum, maximum
  -- ** Tests
  , specCanMinMax, specCanMinMaxNotMixed
  -- * Absolute value
  , CanAbs(..), CanAbsSameType
  -- ** Tests
  , specCanNegNum, specCanAbs
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import Control.CollectErrors ( CollectErrors, CanBeErrors )
import qualified Control.CollectErrors as CE

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord

{---- Min and max -----}

type CanMinMax t1 t2 =
  (CanMinMaxAsymmetric t1 t2, CanMinMaxAsymmetric t2 t1,
   MinMaxType t1 t2 ~ MinMaxType t2 t1)

{-|
  A replacement for Prelude's `P.min` and `P.max`.  If @t1 = t2@ and @Ord t1@,
  then one can use the default implementation to mirror Prelude's @min@ and @max@.
-}
class CanMinMaxAsymmetric t1 t2 where
  type MinMaxType t1 t2
  type MinMaxType t1 t2 = t1 -- default
  min :: t1 -> t2 -> MinMaxType t1 t2
  max :: t1 -> t2 -> MinMaxType t1 t2
  default min :: (MinMaxType t1 t2 ~ t1, t1~t2, P.Ord t1) => t1 -> t2 -> MinMaxType t1 t2
  min = P.min
  default max :: (MinMaxType t1 t2 ~ t1, t1~t2, P.Ord t1) => t1 -> t2 -> MinMaxType t1 t2
  max = P.max

type CanMinMaxThis t1 t2 =
  (CanMinMax t1 t2, MinMaxType t1 t2 ~ t1)
type CanMinMaxSameType t =
  CanMinMaxThis t t

maximum :: (CanMinMaxSameType t) => [t] -> t
maximum (x:xs) = List.foldl' max x xs
maximum [] = error $ "maximum: empty list"

minimum :: (CanMinMaxSameType t) => [t] -> t
minimum (x:xs) = List.foldl' min x xs
minimum [] = error $ "minimum: empty list"

{-|
  HSpec properties that each implementation of CanMinMax should satisfy.
 -}
specCanMinMax ::
 (Show t1, Show t2, Show t3, Show (MinMaxType t1 t2),
  Show (MinMaxType t1 t1), Show (MinMaxType t2 t1),
  Show (MinMaxType t1 (MinMaxType t2 t3)),
  Show (MinMaxType (MinMaxType t1 t2) t3), Arbitrary t1,
  Arbitrary t2, Arbitrary t3, CanTestCertainly (EqCompareType t1 t1),
  CanTestCertainly (EqCompareType t2 t2),
  CanTestCertainly (OrderCompareType (MinMaxType t1 t2) t2),
  CanTestCertainly (OrderCompareType (MinMaxType t1 t2) t1),
  CanTestCertainly (EqCompareType (MinMaxType t1 t1) t1),
  CanTestCertainly
    (EqCompareType (MinMaxType t1 t2) (MinMaxType t2 t1)),
  CanTestCertainly (EqCompareType t3 t3),
  CanTestCertainly
    (EqCompareType
       (MinMaxType t1 (MinMaxType t2 t3))
       (MinMaxType (MinMaxType t1 t2) t3)),
  CanTestFinite t1, CanTestFinite t2, CanTestFinite t3,
  HasEqAsymmetric t1 t1, HasEqAsymmetric t2 t2,
  HasEqAsymmetric t3 t3,
  HasEqAsymmetric (MinMaxType t1 t2) (MinMaxType t2 t1),
  HasEqAsymmetric (MinMaxType t1 t1) t1,
  HasEqAsymmetric
    (MinMaxType t1 (MinMaxType t2 t3))
    (MinMaxType (MinMaxType t1 t2) t3),
  HasOrderAsymmetric (MinMaxType t1 t2) t1,
  HasOrderAsymmetric (MinMaxType t1 t2) t2,
  CanMinMaxAsymmetric t1 t1, CanMinMaxAsymmetric t1 t2,
  CanMinMaxAsymmetric t1 (MinMaxType t2 t3),
  CanMinMaxAsymmetric t2 t1, CanMinMaxAsymmetric t2 t3,
  CanMinMaxAsymmetric (MinMaxType t1 t2) t3)
  =>
  T t1 -> T t2 -> T t3 -> Spec
specCanMinMax (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "CanMinMax %s %s, CanMinMax %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "`min` is not larger than its arguments" $ do
      property $ \ (x :: t1) (y :: t2) ->
        -- (x ?==? x) && (y ?==? y) ==> -- avoid NaN
        (isFinite x) && (isFinite y) ==>
          let m = x `min` y in (m ?<=?$ y) .&&. (m ?<=?$ x)
    it "`max` is not smaller than its arguments" $ do
      property $ \ (x :: t1) (y :: t2) ->
        -- (x ?==? x) && (y ?==? y) ==> -- avoid NaN
        (isFinite x) && (isFinite y) ==>
          let m = x `max` y in (m ?>=?$ y) .&&. (m ?>=?$ x)
    it "has idempotent `min`" $ do
      property $ \ (x :: t1) ->
        -- (x ?==? x) ==> -- avoid NaN
        (isFinite x) ==>
          (x `min` x) ?==?$ x
    it "has idempotent `max`" $ do
      property $ \ (x :: t1) ->
        -- (x ?==? x) ==> -- avoid NaN
        (isFinite x) ==>
          (x `max` x) ?==?$ x
    it "has commutative `min`" $ do
      property $ \ (x :: t1) (y :: t2) ->
        -- (x ?==? x) && (y ?==? y) ==> -- avoid NaN
        (isFinite x) && (isFinite y) ==>
          (x `min` y) ?==?$ (y `min` x)
    it "has commutative `max`" $ do
      property $ \ (x :: t1) (y :: t2) ->
        -- (x ?==? x) && (y ?==? y) ==> -- avoid NaN
        (isFinite x) && (isFinite y) ==>
          (x `max` y) ?==?$ (y `max` x)
    it "has associative `min`" $ do
      property $ \ (x :: t1) (y :: t2) (z :: t3) ->
        -- (x ?==? x) && (y ?==? y) && (z ?==? z) ==> -- avoid NaN
        (isFinite x) && (isFinite y) && (isFinite z) ==>
            (x `min` (y `min` z)) ?==?$ ((x `min` y) `min` z)
    it "has associative `max`" $ do
      property $ \ (x :: t1) (y :: t2) (z :: t3) ->
        -- (x ?==? x) && (y ?==? y) && (z ?==? z) ==> -- avoid NaN
        (isFinite x) && (isFinite y) && (isFinite z) ==>
            (x `max` (y `max` z)) ?==?$ ((x `max` y) `max` z)
  where
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)
  (?>=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?>=?$) = printArgsIfFails2 "?>=?" (?>=?)
  (?<=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<=?$) = printArgsIfFails2 "?<=?" (?<=?)
--
{-|
  HSpec properties that each implementation of CanMinMax should satisfy.
 -}
specCanMinMaxNotMixed ::
 (Show t, Show (MinMaxType t t),
  Show (MinMaxType t (MinMaxType t t)),
  Show (MinMaxType (MinMaxType t t) t), Arbitrary t,
  CanTestCertainly (EqCompareType t t),
  CanTestCertainly (OrderCompareType (MinMaxType t t) t),
  CanTestCertainly (EqCompareType (MinMaxType t t) t),
  CanTestCertainly
    (EqCompareType (MinMaxType t t) (MinMaxType t t)),
  CanTestCertainly
    (EqCompareType
       (MinMaxType t (MinMaxType t t))
       (MinMaxType (MinMaxType t t) t)),
  CanTestFinite t,
  HasEqAsymmetric t t, HasEqAsymmetric (MinMaxType t t) t,
  HasEqAsymmetric (MinMaxType t t) (MinMaxType t t),
  HasEqAsymmetric
    (MinMaxType t (MinMaxType t t))
    (MinMaxType (MinMaxType t t) t),
  HasOrderAsymmetric (MinMaxType t t) t,
  CanMinMaxAsymmetric t t,
  CanMinMaxAsymmetric t (MinMaxType t t),
  CanMinMaxAsymmetric (MinMaxType t t) t)
  =>
  T t -> Spec
specCanMinMaxNotMixed t = specCanMinMax t t t

instance CanMinMaxAsymmetric Int Int
instance CanMinMaxAsymmetric Integer Integer
instance CanMinMaxAsymmetric Rational Rational
instance CanMinMaxAsymmetric Double Double

instance CanMinMaxAsymmetric Int Integer where
  type MinMaxType Int Integer = Integer
  min = convertFirst min
  max = convertFirst max
instance CanMinMaxAsymmetric Integer Int where
  type MinMaxType Integer Int = Integer
  min = convertSecond min
  max = convertSecond max

instance CanMinMaxAsymmetric Int Rational where
  type MinMaxType Int Rational = Rational
  min = convertFirst min
  max = convertFirst max
instance CanMinMaxAsymmetric Rational Int where
  type MinMaxType Rational Int = Rational
  min = convertSecond min
  max = convertSecond max

instance CanMinMaxAsymmetric Integer Rational where
  type MinMaxType Integer Rational = Rational
  min = convertFirst min
  max = convertFirst max
instance CanMinMaxAsymmetric Rational Integer where
  type MinMaxType Rational Integer = Rational
  min = convertSecond min
  max = convertSecond max

instance (CanMinMaxAsymmetric a b) => CanMinMaxAsymmetric [a] [b] where
  type MinMaxType [a] [b] = [MinMaxType a b]
  min (x:xs) (y:ys) = (min x y) : (min xs ys)
  min _ _ = []
  max (x:xs) (y:ys) = (max x y) : (max xs ys)
  max _ _ = []

instance (CanMinMaxAsymmetric a b) => CanMinMaxAsymmetric (Maybe a) (Maybe b) where
  type MinMaxType (Maybe a) (Maybe b) = Maybe (MinMaxType a b)
  min (Just x) (Just y) = Just (min x y)
  min _ _ = Nothing
  max (Just x) (Just y) = Just (max x y)
  max _ _ = Nothing

instance
  (CanMinMaxAsymmetric a b, CanBeErrors es)
  =>
  CanMinMaxAsymmetric (CollectErrors es a) (CollectErrors es  b)
  where
  type MinMaxType (CollectErrors es a) (CollectErrors es b) =
    CollectErrors es (MinMaxType a b)
  min = CE.lift2 min
  max = CE.lift2 max

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (CanMinMaxAsymmetric $t b, CanBeErrors es)
      =>
      CanMinMaxAsymmetric $t (CollectErrors es  b)
      where
      type MinMaxType $t (CollectErrors es  b) =
        CollectErrors es (MinMaxType $t b)
      min = CE.liftT1 min
      max = CE.liftT1 max

    instance
      (CanMinMaxAsymmetric a $t, CanBeErrors es)
      =>
      CanMinMaxAsymmetric (CollectErrors es a) $t
      where
      type MinMaxType (CollectErrors es  a) $t =
        CollectErrors es (MinMaxType a $t)
      min = CE.lift1T min
      max = CE.lift1T max

  |]))


{-| Compound type constraint useful for test definition. -}
type CanNegX t =
  (CanNeg t, Show t, Arbitrary t, Show (NegType t))

{----  numeric negation tests and instances -----}

{-|
  HSpec properties that each numeric implementation of CanNeg should satisfy.
 -}
specCanNegNum ::
  (CanNegX t, CanNegX (NegType t),
   HasEqCertainly t (NegType (NegType t)),
   ConvertibleExactly Integer t,
   HasEqCertainly t t,
   HasEqCertainly t (NegType t),
   CanTestFinite t,
   CanTestPosNeg t,
   CanTestPosNeg (NegType t)
  )
  =>
  T t -> Spec
specCanNegNum (T typeName :: T t) =
  describe (printf "CanNeg %s" typeName) $ do
    it "ignores double negation" $ do
      property $ \ (x :: t) ->
        (x ?==? x) ==> -- avoid NaN
          (negate (negate x)) ?==?$ x
    it "takes 0 to 0" $ do
      let z = convertExactly 0 :: t in negate z ?==? z
    it "takes positive to negative" $ do
      property $ \ (x :: t) ->
        (isFinite x) ==> -- avoid NaN
        (isCertainlyPositive x) ==> (isCertainlyNegative (negate x))
    it "takes negative to positive" $ do
      property $ \ (x :: t) ->
        (isFinite x) ==> -- avoid NaN
        (isCertainlyNegative x) ==> (isCertainlyPositive (negate x))
  where
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

instance CanNeg Int where negate = P.negate
instance CanNeg Integer where negate = P.negate
instance CanNeg Rational where negate = P.negate
instance CanNeg Double where negate = P.negate

{----  abs -----}

{-|
  A replacement for Prelude's `P.abs`.  If @Num t@,
  then one can use the default implementation to mirror Prelude's @abs@.
-}
class CanAbs t where
  type AbsType t
  type AbsType t = t -- default
  abs :: t -> AbsType t
  default abs :: (AbsType t ~ t, P.Num t) => t -> AbsType t
  abs = P.abs

type CanAbsSameType t = (CanAbs t, AbsType t ~ t)

instance CanAbs Int
instance CanAbs Integer
instance CanAbs Rational
instance CanAbs Double

instance
  (CanAbs a, CanBeErrors es)
  =>
  CanAbs (CollectErrors es a)
  where
  type AbsType (CollectErrors es a) = CollectErrors es (AbsType a)
  abs = CE.lift abs

type CanAbsX t =
  (CanAbs t,
   CanNegSameType t,
   CanTestPosNeg t,
   CanTestPosNeg (AbsType t),
   HasEqCertainly t t,
   HasEqCertainly t (AbsType t),
   Show t, Arbitrary t, Show (AbsType t))

{-|
  HSpec properties that each implementation of CanAbs should satisfy.
 -}
specCanAbs ::
  (CanAbsX t, CanAbsX (AbsType t), CanTestFinite t)
  =>
  T t -> Spec
specCanAbs (T typeName :: T t) =
  describe (printf "CanAbs %s" typeName) $ do
    it "is idempotent" $ do
      property $ \ (x :: t) ->
        (x ?==? x) ==> -- avoid NaN
          (abs (abs x)) ?==?$ (abs x)
    it "is identity on non-negative arguments" $ do
      property $ \ (x :: t) ->
        (isFinite x) ==>
        isCertainlyNonNegative x  ==> x ?==?$ (abs x)
    it "is negation on non-positive arguments" $ do
      property $ \ (x :: t) ->
        (isFinite x) ==>
        isCertainlyNonPositive x  ==> (negate x) ?==?$ (abs x)
    it "does not give negative results" $ do
      property $ \ (x :: t) -> 
        (isFinite x) ==>
        not $ isCertainlyNegative (abs x)
  where
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

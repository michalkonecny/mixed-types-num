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
  , specCanMinMax, specCanMinMaxNotMixed, CanMinMaxX, CanMinMaxXX
  -- * Absolute value
  , CanAbs(..), CanAbsSameType
  -- ** Tests
  , specCanNegNum, specCanAbs
)
where

import Prelude hiding
  (fromInteger,
   negate,not,(&&),(||),and,or,
   (==), (/=), (>), (<), (<=), (>=),
   abs, min, max, minimum, maximum, (-))
import qualified Prelude as P
import Text.Printf

import qualified Data.List as List

import Test.Hspec
import qualified Test.QuickCheck as QC

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd

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
  default min :: (MinMaxType t1 t2 ~ t1, t1~t2, P.Ord t1) => t1 -> t1 -> t1
  min = P.min
  default max :: (MinMaxType t1 t2 ~ t1, t1~t2, P.Ord t1) => t1 -> t1 -> t1
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

{-| Compound type constraint useful for test definition. -}
type CanMinMaxX t1 t2 =
  (CanMinMax t1 t2,
   Show t1, QC.Arbitrary t1,
   Show t2, QC.Arbitrary t2,
   HasEq t1 (MinMaxType t1 t2),
   HasEq t2 (MinMaxType t1 t2),
   HasEq (MinMaxType t1 t2) (MinMaxType t1 t2),
   HasOrder t1 (MinMaxType t1 t2),
   HasOrder t2 (MinMaxType t1 t2),
   HasOrder (MinMaxType t1 t2) (MinMaxType t1 t2))

{-| Compound type constraint useful for test definition. -}
type CanMinMaxXX t1 t2 =
  (CanMinMaxX t1 t2,
   HasEq (MinMaxType t1 t2) (MinMaxType t2 t1))

{-|
  HSpec properties that each implementation of CanMinMax should satisfy.
 -}
specCanMinMax ::
  (CanMinMaxXX t1 t1,
   CanMinMaxXX t1 t2,
   CanMinMaxXX t1 t3, CanMinMaxXX t2 t3,
   CanMinMaxXX t1 (MinMaxType t2 t3),
   CanMinMaxXX (MinMaxType t1 t2) t3,
   HasEq (MinMaxType t1 (MinMaxType t2 t3)) (MinMaxType (MinMaxType t1 t2) t3))
  =>
  T t1 -> T t2 -> T t3 -> Spec
specCanMinMax (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "CanMinMax %s %s, CanMinMax %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "`min` is not larger than its arguments" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> let m = x `min` y in (m //<= y) && (m //<= x)
    it "`max` is not smaller than its arguments" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> let m = x `max` y in (m //>= y) && (m //>= x)
    it "has idempotent `min`" $ do
      QC.property $ \ (x :: t1) -> (x `min` x) //== x
    it "has idempotent `max`" $ do
      QC.property $ \ (x :: t1) -> (x `max` x) //== x
    it "has commutative `min`" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x `min` y) //== (y `min` x)
    it "has commutative `max`" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x `max` y) //== (y `max` x)
    it "has associative `min`" $ do
      QC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x `min` (y `min` z)) //== ((x `min` y) `min` z)
    it "has associative `max`" $ do
      QC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x `max` (y `max` z)) //== ((x `max` y) `max` z)
--
{-|
  HSpec properties that each implementation of CanMinMax should satisfy.
 -}
specCanMinMaxNotMixed ::
  (CanMinMaxXX t t,
   CanMinMaxXX t (MinMaxType t t),
   HasEq (MinMaxType (MinMaxType t t) t) (MinMaxType t (MinMaxType t t)) )
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

{-| Compound type constraint useful for test definition. -}
type CanNegX t =
  (CanNeg t, Show t, QC.Arbitrary t)

{----  numeric negation tests and instances -----}

{-|
  HSpec properties that each implementation of CanNegSameType should satisfy.
 -}
specCanNegNum ::
  (CanNegX t, CanNegX (NegType t),
   HasEq t (NegType (NegType t)),
   Convertible Integer t,
   HasEq t (NegType t),
   CanTestPosNeg t,
   CanTestPosNeg (NegType t)
  )
  =>
  T t -> Spec
specCanNegNum (T typeName :: T t) =
  describe (printf "CanNeg %s" typeName) $ do
    it "ignores double negation" $ do
      QC.property $ \ (x :: t) -> (negate (negate x)) //== x
    it "takes 0 to 0" $ do
      let z = convert 0 :: t in negate z //== z
    it "takes positive to negative" $ do
      QC.property $ \ (x :: t) ->
        (isCertainlyPositive x) QC.==> (isCertainlyNegative (negate x))
    it "takes negative to positive" $ do
      QC.property $ \ (x :: t) ->
        (isCertainlyNegative x) QC.==> (isCertainlyPositive (negate x))

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
  default abs :: (AbsType t ~ t, P.Num t) => t -> t
  abs = P.abs

type CanAbsSameType t = (CanAbs t, AbsType t ~ t)

instance CanAbs Int
instance CanAbs Integer
instance CanAbs Rational
instance CanAbs Double

type CanAbsX t =
  (CanAbs t, CanTestPosNeg t,
   CanNegSameType t,
   CanTestPosNeg t,
   CanTestPosNeg (AbsType t),
   HasEq t (AbsType t),
   Show t, QC.Arbitrary t)

{-|
  HSpec properties that each implementation of CanAbsSameType should satisfy.
 -}
specCanAbs ::
  (CanAbsX t, CanAbsX (AbsType t),
   HasEq (AbsType (AbsType t)) (AbsType t))
  =>
  T t -> Spec
specCanAbs (T typeName :: T t) =
  describe (printf "CanAbsSameType %s" typeName) $ do
    it "is idempotent" $ do
      QC.property $ \ (x :: t) -> (abs (abs x)) //== (abs x)
    it "is identity on non-negative arguments" $ do
      QC.property $ \ (x :: t) ->
        isCertainlyNonNegative x  QC.==> x //== (abs x)
    it "is negation on non-positive arguments" $ do
      QC.property $ \ (x :: t) ->
        isCertainlyNonPositive x  QC.==> (negate x) //== (abs x)
    it "does not give negative results" $ do
      QC.property $ \ (x :: t) -> not $ isCertainlyNegative (abs x)

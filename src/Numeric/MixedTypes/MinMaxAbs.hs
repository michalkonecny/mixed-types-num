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
    CanMinMax(..), CanMinMaxX, specCanMinMax, specCanMinMaxNotMixed
)
where

import Prelude hiding
  (fromInteger,
   negate,not,(&&),(||),and,or,
   (==), (/=), (>), (<), (<=), (>=),
   abs, min, max, minimum, maximum)
import qualified Prelude as P
import Text.Printf

import Test.Hspec
import qualified Test.QuickCheck as QC
-- import qualified Test.Hspec.SmallCheck as HSC
-- import qualified Test.SmallCheck as SC
-- import qualified Test.SmallCheck.Series as SCS

-- import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd

{---- Min and max -----}

class CanMinMax a b where
  type MinMaxType a b
  type MinMaxType a b = a -- default
  min :: a -> b -> MinMaxType a b
  max :: a -> b -> MinMaxType a b
  default min :: (MinMaxType a b ~ a, a~b, P.Ord a) => a -> a -> a
  min = P.min
  default max :: (MinMaxType a b ~ a, a~b, P.Ord a) => a -> a -> a
  max = P.max

type CanMinMaxX t1 t2 =
  (CanMinMax t1 t2,
   Show t1, QC.Arbitrary t1,
   Show t2, QC.Arbitrary t2,
   HasEq t1 (MinMaxType t1 t2), HasEq (MinMaxType t1 t2) t1,
   HasEq t2 (MinMaxType t1 t2), HasEq (MinMaxType t1 t2) t2,
   HasEq (MinMaxType t1 t2) (MinMaxType t1 t2),
   HasOrder t1 (MinMaxType t1 t2), HasOrder (MinMaxType t1 t2) t1,
   HasOrder t2 (MinMaxType t1 t2), HasOrder (MinMaxType t1 t2) t2,
   HasOrder (MinMaxType t1 t2) (MinMaxType t1 t2))

type CanMinMaxXX t1 t2 =
  (CanMinMaxX t1 t2, CanMinMaxX t2 t1,
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
  String -> t1 ->
  String -> t2 ->
  String -> t3 ->
  Spec
specCanMinMax typeName1 (_typeSample1 :: t1) typeName2 (_typeSample2 :: t2) typeName3 (_typeSample3 :: t3) =
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
  String -> t -> Spec
specCanMinMaxNotMixed typeName typeSample =
  specCanMinMax typeName typeSample typeName typeSample typeName typeSample

instance CanMinMax Int Int
instance CanMinMax Integer Integer
instance CanMinMax Rational Rational
instance CanMinMax Double Double

instance CanMinMax Int Integer where
  type MinMaxType Int Integer = Integer
  min = convertFirst min
  max = convertFirst max
instance CanMinMax Integer Int where
  type MinMaxType Integer Int = Integer
  min = convertSecond min
  max = convertSecond max

instance CanMinMax Int Rational where
  type MinMaxType Int Rational = Rational
  min = convertFirst min
  max = convertFirst max
instance CanMinMax Rational Int where
  type MinMaxType Rational Int = Rational
  min = convertSecond min
  max = convertSecond max

instance CanMinMax Integer Rational where
  type MinMaxType Integer Rational = Rational
  min = convertFirst min
  max = convertFirst max
instance CanMinMax Rational Integer where
  type MinMaxType Rational Integer = Rational
  min = convertSecond min
  max = convertSecond max

instance (CanMinMax a b) => CanMinMax [a] [b] where
  type MinMaxType [a] [b] = [MinMaxType a b]
  min (x:xs) (y:ys) = (min x y) : (min xs ys)
  min _ _ = []
  max (x:xs) (y:ys) = (max x y) : (max xs ys)
  max _ _ = []

instance (CanMinMax a b) => CanMinMax (Maybe a) (Maybe b) where
  type MinMaxType (Maybe a) (Maybe b) = Maybe (MinMaxType a b)
  min (Just x) (Just y) = Just (min x y)
  min _ _ = Nothing
  max (Just x) (Just y) = Just (max x y)
  max _ _ = Nothing

{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Ord
    Description :  Bottom-up typed order comparisons
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Ord
(
  -- * Comparisons in numeric order
  HasOrder, HasOrderAsymmetric(..), (>), (<), (<=), (>=)
  , HasOrderCertainlyAsymmetric, HasOrderCertainly
  , (?<=?), (?<?), (?>=?), (?>?)
  , (!<=!), (!<!), (!>=!), (!>!)
  -- ** Tests
  , specHasOrder, specHasOrderNotMixed, HasOrderX
  -- ** Specific comparisons
  , CanTestPosNeg(..)
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

import Test.Hspec
import qualified Test.QuickCheck as QC

import Numeric.CollectErrors (CollectErrors, EnsureCollectErrors, CanEnsureCollectErrors)
import qualified Numeric.CollectErrors as CN

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
-- import Numeric.MixedTypes.Eq

infix  4  <, <=, >=, >
infix 4 ?<=?, ?<?, ?>=?, ?>?
infix 4 !<=!, !<!, !>=!, !>!

{---- Inequality -----}

type HasOrder t1 t2 =
  (HasOrderAsymmetric t1 t2, HasOrderAsymmetric t2 t1,
   OrderCompareType t1 t2 ~ OrderCompareType t2 t1)

type HasOrderCertainly t1 t2 =
  (HasOrder t1 t2, CanTestCertainly (OrderCompareType t1 t2))

type HasOrderCertainlyAsymmetric t1 t2 =
  (HasOrderAsymmetric t1 t2, CanTestCertainly (OrderCompareType t1 t2))

class (IsBool (OrderCompareType a b)) => HasOrderAsymmetric a b where
    type OrderCompareType a b
    type OrderCompareType a b = Bool -- default
    lessThan :: a -> b -> (OrderCompareType a b)
    -- default lessThan via Prelude for Bool:
    default lessThan :: (OrderCompareType a b ~ Bool, a~b, P.Ord a) => a -> b -> Bool
    lessThan = (P.<)
    greaterThan :: a -> b -> (OrderCompareType a b)
    default greaterThan ::
      (HasOrder b a, OrderCompareType b a ~ OrderCompareType a b) =>
      a -> b -> (OrderCompareType a b)
    greaterThan a b = lessThan b a
    leq :: a -> b -> (OrderCompareType a b)
    -- default lessThan via Prelude for Bool:
    default leq :: (OrderCompareType a b ~ Bool, a~b, P.Ord a) => a -> b -> Bool
    leq = (P.<=)
    geq :: a -> b -> (OrderCompareType a b)
    default geq ::
      (HasOrder b a, OrderCompareType b a ~ OrderCompareType a b) =>
      a -> b -> (OrderCompareType a b)
    geq a b = leq b a

(>) :: (HasOrderAsymmetric a b) => a -> b -> OrderCompareType a b
(>) = greaterThan
(<) :: (HasOrderAsymmetric a b) => a -> b -> OrderCompareType a b
(<) = lessThan

(>=) :: (HasOrderAsymmetric a b) => a -> b -> OrderCompareType a b
(>=) = geq
(<=) :: (HasOrderAsymmetric a b) => a -> b -> OrderCompareType a b
(<=) = leq

(?>?) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a ?>? b = isNotFalse $ a > b

(?<?) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a ?<? b = isNotFalse $ a < b

(?>=?) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a ?>=? b = isNotFalse $ a >= b

(?<=?) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a ?<=? b = isNotFalse $ a <= b

(!>!) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a !>! b = isCertainlyTrue $ a > b

(!<!) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a !<! b = isCertainlyTrue $ a < b

(!>=!) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a !>=! b = isCertainlyTrue $ a >= b

(!<=!) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a !<=! b = isCertainlyTrue $ a <= b

{-| Compound type constraint useful for test definition. -}
type HasOrderX t1 t2 =
  (HasOrderCertainly t1 t2, Show t1, QC.Arbitrary t1, Show t2, QC.Arbitrary t2)

{-|
  HSpec properties that each implementation of 'HasOrder' should satisfy.
 -}
specHasOrder ::
  (HasOrderX t1 t1,
   HasOrderX t1 t2,
   HasOrderX t1 t3, HasOrderX t2 t3,
   CanAndOrX (OrderCompareType t1 t2) (OrderCompareType t2 t3))
  =>
  T t1 -> T t2 -> T t3 -> Spec
specHasOrder (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "HasOrd %s %s, HasOrd %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "has reflexive >=" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyFalse (x >= x)
    it "has reflexive <=" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyFalse (x <= x)
    it "has anti-reflexive >" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyTrue (x > x)
    it "has anti-reflexive <" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyTrue (x < x)
    it "> stronly implies >=" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x > y) `stronglyImplies` (x >= y)
    it "< stronly implies <=" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x < y) `stronglyImplies` (x <= y)
    it "has stronly equivalent > and <" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x < y) `stronglyEquivalentTo` (y > x)
    it "has stronly equivalent >= and <=" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x <= y) `stronglyEquivalentTo` (y >= x)
    it "has stronly transitive <" $ do
      QC.property $ \ (x :: t1) (y :: t2) (z :: t3) -> ((x < y) && (y < z)) `stronglyImplies` (y < z)

{-|
  HSpec properties that each implementation of 'HasOrder' should satisfy.
 -}
specHasOrderNotMixed ::
  (HasOrderX t t,
   CanAndOrX (OrderCompareType t t) (OrderCompareType t t))
  =>
  T t -> Spec
specHasOrderNotMixed t = specHasOrder t t t

instance HasOrderAsymmetric Int Int
instance HasOrderAsymmetric Integer Integer
instance HasOrderAsymmetric Rational Rational
instance HasOrderAsymmetric Double Double

instance HasOrderAsymmetric Int Integer where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Integer Int where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Int Rational where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Rational Int where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Integer Rational where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Rational Integer where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Integer Double where
  lessThan n d = (n <= (P.floor d :: Integer)) && (n < (P.ceiling d :: Integer))
  leq n d = (n <= (P.floor d :: Integer))
instance HasOrderAsymmetric Double Integer where
  lessThan d n = ((P.floor d :: Integer) < n) && ((P.ceiling d :: Integer) <= n)
  leq d n = ((P.ceiling d :: Integer) <= n)

instance HasOrderAsymmetric Int Double where
  lessThan n d = lessThan (integer n) d
  leq n d = leq (integer n) d
instance HasOrderAsymmetric Double Int where
  lessThan d n = lessThan d (integer n)
  leq d n = leq d (integer n)

instance
  (HasOrderAsymmetric a b
  , CanEnsureCollectErrors es (OrderCompareType a b)
  , IsBool (EnsureCollectErrors es (OrderCompareType a b))
  , Monoid es)
  =>
  HasOrderAsymmetric (CollectErrors es a) (CollectErrors es  b)
  where
  type OrderCompareType (CollectErrors es a) (CollectErrors es b) =
    EnsureCollectErrors es (OrderCompareType a b)
  lessThan = CN.lift2ensureCE lessThan
  leq = CN.lift2ensureCE leq
  greaterThan = CN.lift2ensureCE greaterThan
  geq = CN.lift2ensureCE geq

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (HasOrderAsymmetric $t b
      , CanEnsureCollectErrors es (OrderCompareType $t b)
      , IsBool (EnsureCollectErrors es (OrderCompareType $t b))
      , Monoid es)
      =>
      HasOrderAsymmetric $t (CollectErrors es  b)
      where
      type OrderCompareType $t (CollectErrors es  b) =
        EnsureCollectErrors es (OrderCompareType $t b)
      lessThan = CN.unlift2first lessThan
      leq = CN.unlift2first leq
      greaterThan = CN.unlift2first greaterThan
      geq = CN.unlift2first geq

    instance
      (HasOrderAsymmetric a $t
      , CanEnsureCollectErrors es (OrderCompareType a $t)
      , IsBool (EnsureCollectErrors es (OrderCompareType a $t))
      , Monoid es)
      =>
      HasOrderAsymmetric (CollectErrors es a) $t
      where
      type OrderCompareType (CollectErrors es  a) $t =
        EnsureCollectErrors es (OrderCompareType a $t)
      lessThan = CN.unlift2second lessThan
      leq = CN.unlift2second leq
      greaterThan = CN.unlift2second greaterThan
      geq = CN.unlift2second geq

  |]))

class CanTestPosNeg t where
    isCertainlyPositive :: t -> Bool
    isCertainlyNonNegative :: t -> Bool
    isCertainlyNegative :: t -> Bool
    isCertainlyNonPositive :: t -> Bool
    default isCertainlyPositive :: (HasOrderCertainly t Integer) => t -> Bool
    isCertainlyPositive a = isCertainlyTrue $ a > 0
    default isCertainlyNonNegative :: (HasOrderCertainly t Integer) => t -> Bool
    isCertainlyNonNegative a = isCertainlyTrue $ a >= 0
    default isCertainlyNegative :: (HasOrderCertainly t Integer) => t -> Bool
    isCertainlyNegative a = isCertainlyTrue $ a < 0
    default isCertainlyNonPositive :: (HasOrderCertainly t Integer) => t -> Bool
    isCertainlyNonPositive a = isCertainlyTrue $ a <= 0

instance CanTestPosNeg Int
instance CanTestPosNeg Integer
instance CanTestPosNeg Rational
instance CanTestPosNeg Double

instance (CanTestPosNeg t, Monoid es, P.Eq es) => (CanTestPosNeg (CollectErrors es t)) where
  isCertainlyPositive ce = CN.getValueIfNoError ce isCertainlyPositive (const False)
  isCertainlyNonNegative ce = CN.getValueIfNoError ce isCertainlyNonNegative (const False)
  isCertainlyNegative ce = CN.getValueIfNoError ce isCertainlyNegative (const False)
  isCertainlyNonPositive ce = CN.getValueIfNoError ce isCertainlyNonPositive (const False)

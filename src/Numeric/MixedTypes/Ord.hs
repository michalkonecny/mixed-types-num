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
  , HasOrderCertainlyCE, HasOrderCertainlyCN
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

import Numeric.CollectErrors
import Control.CollectErrors

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

type HasOrderCertainlyCE es t1 t2 =
  (HasOrderCertainly t1 t2,
   HasOrderCertainly (EnsureCE es t1) (EnsureCE es t2))
  --  ,
  --  CanTestCertainly (WithoutCE es (OrderCompareType (WithoutCE es t1) (WithoutCE es t2))),
  --  IsBool (WithoutCE es (OrderCompareType (WithoutCE es t1) (WithoutCE es t2))),
  --  CanEnsureCE es (OrderCompareType (WithoutCE es t1) (WithoutCE es t2)),
  --  CanEnsureCE es (WithoutCE es (OrderCompareType (WithoutCE es t1) (WithoutCE es t2))),
  --  WithoutCE es (WithoutCE es (OrderCompareType (WithoutCE es t1) (WithoutCE es t2)))
  --    ~ (WithoutCE es (OrderCompareType (WithoutCE es t1) (WithoutCE es t2))))

type HasOrderCertainlyCN t1 t2 = HasOrderCertainlyCE NumErrors t1 t2

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

instance HasOrderAsymmetric () () where
  lessThan _ _ = False
  leq _ _ = True

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
  , CanEnsureCE es (OrderCompareType a b)
  , IsBool (EnsureCE es (OrderCompareType a b))
  , SuitableForCE es)
  =>
  HasOrderAsymmetric (CollectErrors es a) (CollectErrors es  b)
  where
  type OrderCompareType (CollectErrors es a) (CollectErrors es b) =
    EnsureCE es (OrderCompareType a b)
  lessThan = lift2CE lessThan
  leq = lift2CE leq
  greaterThan = lift2CE greaterThan
  geq = lift2CE geq

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (HasOrderAsymmetric $t b
      , CanEnsureCE es (OrderCompareType $t b)
      , IsBool (EnsureCE es (OrderCompareType $t b))
      , SuitableForCE es)
      =>
      HasOrderAsymmetric $t (CollectErrors es  b)
      where
      type OrderCompareType $t (CollectErrors es  b) =
        EnsureCE es (OrderCompareType $t b)
      lessThan = lift2TLCE lessThan
      leq = lift2TLCE leq
      greaterThan = lift2TLCE greaterThan
      geq = lift2TLCE geq

    instance
      (HasOrderAsymmetric a $t
      , CanEnsureCE es (OrderCompareType a $t)
      , IsBool (EnsureCE es (OrderCompareType a $t))
      , SuitableForCE es)
      =>
      HasOrderAsymmetric (CollectErrors es a) $t
      where
      type OrderCompareType (CollectErrors es  a) $t =
        EnsureCE es (OrderCompareType a $t)
      lessThan = lift2TCE lessThan
      leq = lift2TCE leq
      greaterThan = lift2TCE greaterThan
      geq = lift2TCE geq

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

instance (CanTestPosNeg t, SuitableForCE es) => (CanTestPosNeg (CollectErrors es t)) where
  isCertainlyPositive ce = getValueIfNoErrorCE ce isCertainlyPositive (const False)
  isCertainlyNonNegative ce = getValueIfNoErrorCE ce isCertainlyNonNegative (const False)
  isCertainlyNegative ce = getValueIfNoErrorCE ce isCertainlyNegative (const False)
  isCertainlyNonPositive ce = getValueIfNoErrorCE ce isCertainlyNonPositive (const False)

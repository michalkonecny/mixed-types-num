{-|
    Module      :  Numeric.MixedType.EqOrd
    Description :  Mixed-type comparisons
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.EqOrd
(
    -- * Equality tests
    HasEq(..),  (==), (/=), specHasEq
    , CanTestZero(..)
    -- * Inequality tests
    , HasOrder(..), (>), (<), (<=), (>=), CanTestPosNeg(..)
    -- * Helper functions
    , convertFirst, convertSecond
)
where

import Prelude hiding
  (fromInteger,
   negate,not,(&&),(||),and,or,
   (==), (/=), (>), (<), (<=), (>=))
import qualified Prelude as P
import Text.Printf

import Test.Hspec
import qualified Test.QuickCheck as QC
-- import qualified Test.Hspec.SmallCheck as HSC
-- import qualified Test.SmallCheck as SC
-- import qualified Test.SmallCheck.Series as SCS

import Numeric.MixedTypes.Literals (Convertible, convert, fromInteger)
import Numeric.MixedTypes.Bool

infix  4  ==, /=, <, <=, >=, >

{---- Equality tests -----}

class (IsBool (EqCompareType a b)) => HasEq a b where
    type EqCompareType a b
    type EqCompareType a b = Bool -- default
    equalTo :: a -> b -> (EqCompareType a b)
    -- default equalToA via Prelude for (->) and Bool:
    default equalTo :: (EqCompareType a b ~ Bool, a~b, P.Eq a) => a -> b -> Bool
    equalTo = (P.==)
    notEqualTo :: a -> b -> (EqCompareType a b)
    -- default notEqualToA via equalToA for Bool:
    default notEqualTo ::
        (CanNegSameType (EqCompareType a b)) =>
        a -> b -> (EqCompareType a b)
    notEqualTo a b = not $ equalTo a b

(==) :: (HasEq a b) => a -> b -> EqCompareType a b
(==) = equalTo
(/=) :: (HasEq a b) => a -> b -> EqCompareType a b
(/=) = notEqualTo

type HasEqX t1 t2 = (HasEq t1 t2, Show t1, QC.Arbitrary t1, Show t2, QC.Arbitrary t2)

{-|
  HSpec properties that each implementation of HasEq should satisfy.
 -}
specHasEq ::
  (HasEqX t1 t1,
   HasEqX t1 t2, HasEqX t2 t1,
   HasEqX t1 t3, HasEqX t2 t3,
   CanAndOrX (EqCompareType t1 t2) (EqCompareType t2 t3))
  =>
  String -> t1 ->
  String -> t2 ->
  String -> t3 ->
  Spec
specHasEq typeName1 (_typeSample1 :: t1) typeName2 (_typeSample2 :: t2) typeName3 (_typeSample3 :: t3) =
  describe (printf "HasEq %s %s, HasEq %s %s)" typeName1 typeName2 typeName2 typeName3) $ do
    it "has reflexive ==" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyFalse (x == x)
    it "has anti-reflexive /=" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyTrue (x /= x)
    it "has stronly commutative ==" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x == y) `stronglyEquivalentTo` (y == x)
    it "has stronly commutative /=" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x /= y) `stronglyEquivalentTo` (y /= x)
    it "has stronly transitive ==" $ do
      QC.property $ \ (x :: t1) (y :: t2) (z :: t3) -> ((x == y) && (y == z)) `stronglyImplies` (y == z)

instance HasEq Int Int
instance HasEq Integer Integer
instance HasEq Rational Rational
instance HasEq Double Double

instance HasEq Int Integer where
  equalTo = convertFirst equalTo
instance HasEq Integer Int where
  equalTo = convertSecond equalTo

instance HasEq Int Rational where
  equalTo = convertFirst equalTo
instance HasEq Rational Int where
  equalTo = convertSecond equalTo

instance HasEq Integer Rational where
  equalTo = convertFirst equalTo
instance HasEq Rational Integer where
  equalTo = convertSecond equalTo

instance (HasEq a b) => HasEq [a] [b] where
  type EqCompareType [a] [b] = EqCompareType a b
  equalTo [] [] = convert True
  equalTo (x:xs) (y:ys) = (x == y) && (xs == ys)
  equalTo _ _ = convert False

instance (HasEq a b) => HasEq (Maybe a) (Maybe b) where
  type EqCompareType (Maybe a) (Maybe b) = EqCompareType a b
  equalTo Nothing Nothing = convert True
  equalTo (Just x) (Just y) = (x == y)
  equalTo _ _ = convert False

class CanTestZero t where
    isCertainlyZero :: t -> Bool
    isNonZero :: t -> Bool
    default isCertainlyZero :: (HasEq t Integer) => t -> Bool
    isCertainlyZero a = isCertainlyTrue (a == 0)
    default isNonZero :: (HasEq t Integer) => t -> Bool
    isNonZero a = isCertainlyTrue (a /= 0)

instance CanTestZero Int
instance CanTestZero Integer
instance CanTestZero Rational

{---- Inequality -----}

class (IsBool (OrderCompareType a b)) => HasOrder a b where
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

(>) :: (HasOrder a b) => a -> b -> OrderCompareType a b
(>) = greaterThan
(<) :: (HasOrder a b) => a -> b -> OrderCompareType a b
(<) = lessThan

(>=) :: (HasOrder a b) => a -> b -> OrderCompareType a b
(>=) = geq
(<=) :: (HasOrder a b) => a -> b -> OrderCompareType a b
(<=) = leq

instance HasOrder Int Int
instance HasOrder Integer Integer
instance HasOrder Rational Rational
instance HasOrder Double Double

instance HasOrder Int Integer where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrder Integer Int where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrder Int Rational where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrder Rational Int where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrder Integer Rational where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrder Rational Integer where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

class CanTestPosNeg t where
    isCertainlyPositive :: t -> Bool
    isCertainlyNonNegative :: t -> Bool
    isCertainlyNegative :: t -> Bool
    isCertainlyNonPositive :: t -> Bool
    default isCertainlyPositive :: (HasOrder t Integer) => t -> Bool
    isCertainlyPositive a = isCertainlyTrue $ a > 0
    default isCertainlyNonNegative :: (HasOrder t Integer) => t -> Bool
    isCertainlyNonNegative a = isCertainlyTrue $ a >= 0
    default isCertainlyNegative :: (HasOrder t Integer) => t -> Bool
    isCertainlyNegative a = isCertainlyTrue $ a < 0
    default isCertainlyNonPositive :: (HasOrder t Integer) => t -> Bool
    isCertainlyNonPositive a = isCertainlyTrue $ a <= 0

instance CanTestPosNeg Int
instance CanTestPosNeg Integer
instance CanTestPosNeg Rational

{---- Auxiliary functions ----}

convertFirst :: (Convertible a b) => (b -> b -> c) -> (a -> b -> c)
convertFirst op a b = op (convert a) b

convertSecond :: (Convertible b a) => (a -> a -> c) -> (a -> b -> c)
convertSecond op a b = op a (convert b)

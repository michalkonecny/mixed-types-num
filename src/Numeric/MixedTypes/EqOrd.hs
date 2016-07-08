{-|
    Module      :  Numeric.MixedType.EqOrd
    Description :  Bottom-up typed comparisons
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.EqOrd
(
    -- * Equality tests
    HasEq(..),  (==), (/=)
    , certainlyEqualTo, certainlyNotEqualTo, notDifferentFrom
    , (//==)
    , specHasEq, specHasEqNotMixed, HasEqX
    -- ** Specific equality tests
    , CanTestZero(..), specCanTestZero
    , CanPickNonZero(..), specCanPickNonZero
    -- * Inequality tests
    , HasOrder(..), (>), (<), (<=), (>=)
    , specHasOrder, specHasOrderNotMixed, HasOrderX
    , (//<=), (//<), (//>=), (//>)
    , CanTestPosNeg(..)
    -- * Helper functions
    , convertFirst, convertSecond
)
where

import Prelude hiding
  (fromInteger, fromRational,
   negate,not,(&&),(||),and,or,
   (==), (/=), (>), (<), (<=), (>=))
import qualified Prelude as P
import Text.Printf

import Test.Hspec
import qualified Test.QuickCheck as QC
import Control.Exception (evaluate)

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool

infix  4  ==, /=, <, <=, >=, >, //==, //<=, //<, //>=, //>

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

certainlyEqualTo :: (HasEq a b) => a -> b -> Bool
certainlyEqualTo a b = isCertainlyTrue $ a == b
certainlyNotEqualTo :: (HasEq a b) => a -> b -> Bool
certainlyNotEqualTo a b = isCertainlyTrue $ a /= b
notDifferentFrom :: (HasEq a b) => a -> b -> Bool
notDifferentFrom a b = isNotFalse $ a == b

(//==) :: (HasEq a b) => a -> b -> Bool
(//==) = notDifferentFrom

{-| Compound type constraint useful for test definition. -}
type HasEqX t1 t2 =
  (HasEq t1 t2, Show t1, QC.Arbitrary t1, Show t2, QC.Arbitrary t2)

{-|
  HSpec properties that each implementation of HasEq should satisfy.
 -}
specHasEq ::
  (HasEqX t1 t1,
   HasEqX t1 t2, HasEqX t2 t1,
   HasEqX t1 t3, HasEqX t2 t3,
   CanAndOrX (EqCompareType t1 t2) (EqCompareType t2 t3))
  =>
  T t1 -> T t2 -> T t3 -> Spec
specHasEq (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "HasEq %s %s, HasEq %s %s" typeName1 typeName2 typeName2 typeName3) $ do
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

{-|
  HSpec properties that each implementation of HasEq should satisfy.
 -}
specHasEqNotMixed ::
  (HasEqX t t,
   CanAndOrX (EqCompareType t t) (EqCompareType t t))
  =>
  T t -> Spec
specHasEqNotMixed t = specHasEq t t t

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

{-|
  HSpec properties that each implementation of CanTestZero should satisfy.
 -}
specCanTestZero ::
  (CanTestZero t, Convertible Integer t)
  =>
  T t -> Spec
specCanTestZero (T typeName :: T t) =
  describe (printf "CanTestZero %s" typeName) $ do
    it "converted non-zero Integer is not isCertainlyZero" $ do
      QC.property $ \ (x :: Integer) ->
        x /= 0 QC.==> (not $ isCertainlyZero (convert x :: t))
    it "converted non-zero Integer is isNonZero" $ do
      QC.property $ \ (x :: Integer) ->
        x /= 0 QC.==> (isNonZero (convert x :: t))
    it "converted 0.0 is not isNonZero" $ do
      (isNonZero (convert 0 :: t)) `shouldBe` False

instance CanTestZero Int
instance CanTestZero Integer
instance CanTestZero Rational

class CanPickNonZero t where
  {-|
    Given a list @[(a1,b1),(a2,b2),...]@ and assuming that
    at least one of @a1,a2,...@ is non-zero, pick one of them
    and return the corresponding pair @(ai,bi)@.

    If none of @a1,a2,...@ is zero, either throws an exception
    or loops forever.

    The default implementation is based on a `CanTestZero` instance
    and is not parallel.
   -}
  pickNonZero :: [(t,s)] -> (t,s)
  default pickNonZero :: (CanTestZero t, Show t) => [(t,s)] -> (t,s)
  pickNonZero list =
    case aux list of
      Just result -> result
      Nothing ->
        error $ "pickNonZero: failed to find a non-zero element in "
                  ++ show (map fst list)
    where
      aux ((a,b):rest)
        | isNonZero a = Just (a,b)
        | otherwise = aux rest
      aux [] = Nothing

{-|
  HSpec properties that each implementation of CanPickNonZero should satisfy.
 -}
specCanPickNonZero ::
  (CanPickNonZero t, CanTestZero t, Convertible Integer t, Show t, QC.Arbitrary t)
  =>
  T t -> Spec
specCanPickNonZero (T typeName :: T t) =
  describe (printf "CanPickNonZero %s" typeName) $ do
    it "picks a non-zero element if there is one" $ do
      QC.property $ \ (xs :: [(t, ())]) ->
        or (map (isNonZero . fst) xs) -- if at least one is non-zero
          QC.==> (isNonZero $ fst $ pickNonZero xs)
    it "throws exception when all the elements are 0" $ do
      (evaluate $ pickNonZero [(convert i :: t, ()) | i <- [0,0,0]])
        `shouldThrow` anyException

instance CanPickNonZero Int
instance CanPickNonZero Integer
instance CanPickNonZero Rational

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

(//>) :: (HasOrder a b) => a -> b -> Bool
a //> b = isNotFalse $ a > b

(//<) :: (HasOrder a b) => a -> b -> Bool
a //< b = isNotFalse $ a < b

(//>=) :: (HasOrder a b) => a -> b -> Bool
a //>= b = isNotFalse $ a >= b

(//<=) :: (HasOrder a b) => a -> b -> Bool
a //<= b = isNotFalse $ a <= b

{-| Compound type constraint useful for test definition. -}
type HasOrderX t1 t2 =
  (HasOrder t1 t2, Show t1, QC.Arbitrary t1, Show t2, QC.Arbitrary t2)

{-|
  HSpec properties that each implementation of 'HasOrder' should satisfy.
 -}
specHasOrder ::
  (HasOrderX t1 t1,
   HasOrderX t1 t2, HasOrderX t2 t1,
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

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
  HasEq,  HasEqAsymmetric(..), (==), (/=)
  , notCertainlyDifferentFrom, certainlyEqualTo, certainlyNotEqualTo
  , (?==?), (!==!), (!/=!)
  -- ** Tests
  , specHasEq, specHasEqNotMixed, HasEqX
  , specConversion
  -- ** Specific tests
  , CanTestFinite(..)
  , CanTestZero(..), specCanTestZero
  , CanPickNonZero(..), specCanPickNonZero
  -- * Comparisons in numeric order
  , HasOrder, HasOrderAsymmetric(..), (>), (<), (<=), (>=)
  , (?<=?), (?<?), (?>=?), (?>?)
  , (!<=!), (!<!), (!>=!), (!>!)
  -- ** Tests
  , specHasOrder, specHasOrderNotMixed, HasOrderX
  -- ** Specific comparisons
  , CanTestPosNeg(..)
  -- * Helper ops and functions
  , convertFirst, convertSecond
  , convertFirstUsing, convertSecondUsing
)
where

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

import Test.Hspec
import qualified Test.QuickCheck as QC
import Control.Exception (evaluate)

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool

infix  4  ==, /=, <, <=, >=, >
infix 4 ?==?, ?<=?, ?<?, ?>=?, ?>?
infix 4 !==!, !/=!, !<=!, !<!, !>=!, !>!

{---- Equality tests -----}

type HasEq t1 t2 =
    (HasEqAsymmetric t1 t2,  HasEqAsymmetric t2 t1,
     EqCompareType t1 t2 ~ EqCompareType t2 t1)

class (IsBool (EqCompareType a b)) => HasEqAsymmetric a b where
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

(==) :: (HasEqAsymmetric a b) => a -> b -> EqCompareType a b
(==) = equalTo
(/=) :: (HasEqAsymmetric a b) => a -> b -> EqCompareType a b
(/=) = notEqualTo

certainlyEqualTo :: (HasEqAsymmetric a b) => a -> b -> Bool
certainlyEqualTo a b = isCertainlyTrue $ a == b
certainlyNotEqualTo :: (HasEqAsymmetric a b) => a -> b -> Bool
certainlyNotEqualTo a b = isCertainlyTrue $ a /= b
notCertainlyDifferentFrom :: (HasEqAsymmetric a b) => a -> b -> Bool
notCertainlyDifferentFrom a b = isNotFalse $ a == b

(?==?) :: (HasEqAsymmetric a b) => a -> b -> Bool
(?==?) = notCertainlyDifferentFrom

(!==!) :: (HasEqAsymmetric a b) => a -> b -> Bool
(!==!) = certainlyEqualTo

(!/=!) :: (HasEqAsymmetric a b) => a -> b -> Bool
(!/=!) = certainlyNotEqualTo

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

{-|
  HSpec property of there-and-back conversion.
-}
specConversion :: -- this definition cannot be in Literals because it needs HasEq
  (QC.Arbitrary t1, Show t1, HasEq t1 t1) =>
  T t1 -> T t2 -> (t1 -> t2) -> (t2 -> t1) ->  Spec
specConversion (T typeName1 :: T t1) (T typeName2 :: T t2) conv12 conv21 =
  describe "conversion" $ do
    it (printf "%s -> %s -> %s" typeName1 typeName2 typeName1) $ do
      QC.property $ \ (x1 :: t1) ->
        x1 ?==? (conv21 $ conv12 x1)
        -- case (safeConvertExactly x1 :: ConvertResult t2) of
        --   Right x12 ->
        --     case (safeConvertExactly x12 :: ConvertResult t1) of
        --       Right x121 -> x121 P.== x1
        --       _ -> error (printf "failed to convert %s back to %s" (show x12) (show x1))
        --   _ -> True


instance HasEqAsymmetric () ()
instance HasEqAsymmetric Bool Bool
instance HasEqAsymmetric Char Char

instance HasEqAsymmetric Int Int
instance HasEqAsymmetric Integer Integer
instance HasEqAsymmetric Rational Rational
instance HasEqAsymmetric Double Double

instance HasEqAsymmetric Int Integer where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric Integer Int where
  equalTo = convertSecond equalTo

instance HasEqAsymmetric Int Rational where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric Rational Int where
  equalTo = convertSecond equalTo

instance HasEqAsymmetric Integer Rational where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric Rational Integer where
  equalTo = convertSecond equalTo

instance HasEqAsymmetric Integer Double where
  equalTo n d = ((P.floor d :: Integer) == n) && (n == (P.ceiling d :: Integer))
instance HasEqAsymmetric Double Integer where
  equalTo d n = ((P.floor d :: Integer) == n) && (n == (P.ceiling d :: Integer))

instance HasEqAsymmetric Int Double where
  equalTo n d = equalTo (integer n) d
instance HasEqAsymmetric Double Int where
  equalTo d n = equalTo (integer n) d

instance
  (HasEqAsymmetric a1 b1,
   HasEqAsymmetric a2 b2,
   CanAndOrAsymmetric (EqCompareType a1 b1) (EqCompareType a2 b2),
   IsBool (AndOrType (EqCompareType a1 b1) (EqCompareType a2 b2))
  ) =>
  HasEqAsymmetric (a1,a2) (b1,b2) where
  type EqCompareType (a1,a2) (b1,b2) =
    AndOrType (EqCompareType a1 b1) (EqCompareType a2 b2)
  equalTo (a1,a2) (b1,b2) =
    (a1 == b1) && (a2 == b2)

instance
  (HasEqAsymmetric ((a1,a2), a3) ((b1,b2), b3))
  =>
  HasEqAsymmetric (a1,a2,a3) (b1,b2,b3) where
  type EqCompareType (a1,a2,a3) (b1,b2,b3) =
    EqCompareType ((a1,a2), a3) ((b1,b2), b3)
  equalTo (a1,a2,a3) (b1,b2,b3) =
    ((a1,a2), a3) == ((b1,b2), b3)

instance
  (HasEqAsymmetric ((a1,a2,a3), a4) ((b1,b2,b3), b4))
  =>
  HasEqAsymmetric (a1,a2,a3,a4) (b1,b2,b3,b4) where
  type EqCompareType (a1,a2,a3,a4) (b1,b2,b3,b4) =
    EqCompareType ((a1,a2,a3), a4) ((b1,b2,b3), b4)
  equalTo (a1,a2,a3,a4) (b1,b2,b3,b4) =
    ((a1,a2,a3), a4) == ((b1,b2,b3), b4)

instance
  (HasEqAsymmetric ((a1,a2,a3,a4), a5) ((b1,b2,b3,b4), b5))
  =>
  HasEqAsymmetric (a1,a2,a3,a4,a5) (b1,b2,b3,b4,b5) where
  type EqCompareType (a1,a2,a3,a4,a5) (b1,b2,b3,b4,b5) =
    EqCompareType ((a1,a2,a3,a4), a5) ((b1,b2,b3,b4), b5)
  equalTo (a1,a2,a3,a4,a5) (b1,b2,b3,b4,b5) =
    ((a1,a2,a3,a4), a5) == ((b1,b2,b3,b4), b5)

instance (HasEqAsymmetric a b) => HasEqAsymmetric [a] [b] where
  type EqCompareType [a] [b] = EqCompareType a b
  equalTo [] [] = convertExactly True
  equalTo (x:xs) (y:ys) = (x == y) && (xs == ys)
  equalTo _ _ = convertExactly False

instance (HasEqAsymmetric a b) => HasEqAsymmetric (Maybe a) (Maybe b) where
  type EqCompareType (Maybe a) (Maybe b) = EqCompareType a b
  equalTo Nothing Nothing = convertExactly True
  equalTo (Just x) (Just y) = (x == y)
  equalTo _ _ = convertExactly False

class CanTestFinite t where
  isNaN :: t -> Bool
  default isNaN :: (P.RealFloat t) => t -> Bool
  isNaN = P.isNaN
  isInfinite :: t -> Bool
  default isInfinite :: (P.RealFloat t) => t -> Bool
  isInfinite = P.isInfinite
  isFinite :: t -> Bool
  isFinite x = (not $ isNaN x) && (not $ isInfinite x)

instance CanTestFinite Double
instance CanTestFinite Rational where
  isNaN = const False
  isInfinite = const False

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
  (CanTestZero t, ConvertibleExactly Integer t)
  =>
  T t -> Spec
specCanTestZero (T typeName :: T t) =
  describe (printf "CanTestZero %s" typeName) $ do
    it "converted non-zero Integer is not isCertainlyZero" $ do
      QC.property $ \ (x :: Integer) ->
        x /= 0 QC.==> (not $ isCertainlyZero (convertExactly x :: t))
    it "converted non-zero Integer is isNonZero" $ do
      QC.property $ \ (x :: Integer) ->
        x /= 0 QC.==> (isNonZero (convertExactly x :: t))
    it "converted 0.0 is not isNonZero" $ do
      (isNonZero (convertExactly 0 :: t)) `shouldBe` False

instance CanTestZero Int
instance CanTestZero Integer
instance CanTestZero Rational
instance CanTestZero Double

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
  (CanPickNonZero t, CanTestZero t, ConvertibleExactly Integer t, Show t, QC.Arbitrary t)
  =>
  T t -> Spec
specCanPickNonZero (T typeName :: T t) =
  describe (printf "CanPickNonZero %s" typeName) $ do
    it "picks a non-zero element if there is one" $ do
      QC.property $ \ (xs :: [(t, ())]) ->
        or (map (isNonZero . fst) xs) -- if at least one is non-zero
          QC.==> (isNonZero $ fst $ pickNonZero xs)
    it "throws exception when all the elements are 0" $ do
      (evaluate $ pickNonZero [(convertExactly i :: t, ()) | i <- [0,0,0]])
        `shouldThrow` anyException

instance CanPickNonZero Int
instance CanPickNonZero Integer
instance CanPickNonZero Rational

{---- Inequality -----}

type HasOrder t1 t2 =
  (HasOrderAsymmetric t1 t2, HasOrderAsymmetric t2 t1,
   OrderCompareType t1 t2 ~ OrderCompareType t2 t1)

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

(?>?) :: (HasOrderAsymmetric a b) => a -> b -> Bool
a ?>? b = isNotFalse $ a > b

(?<?) :: (HasOrderAsymmetric a b) => a -> b -> Bool
a ?<? b = isNotFalse $ a < b

(?>=?) :: (HasOrderAsymmetric a b) => a -> b -> Bool
a ?>=? b = isNotFalse $ a >= b

(?<=?) :: (HasOrderAsymmetric a b) => a -> b -> Bool
a ?<=? b = isNotFalse $ a <= b

(!>!) :: (HasOrderAsymmetric a b) => a -> b -> Bool
a !>! b = isCertainlyTrue $ a > b

(!<!) :: (HasOrderAsymmetric a b) => a -> b -> Bool
a !<! b = isCertainlyTrue $ a < b

(!>=!) :: (HasOrderAsymmetric a b) => a -> b -> Bool
a !>=! b = isCertainlyTrue $ a >= b

(!<=!) :: (HasOrderAsymmetric a b) => a -> b -> Bool
a !<=! b = isCertainlyTrue $ a <= b

{-| Compound type constraint useful for test definition. -}
type HasOrderX t1 t2 =
  (HasOrder t1 t2, Show t1, QC.Arbitrary t1, Show t2, QC.Arbitrary t2)

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
instance CanTestPosNeg Double

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

{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Eq
    Description :  Bottom-up typed equality comparisons
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.Eq
(
  -- * Equality checks
  HasEq,  HasEqAsymmetric(..), (==), (/=)
  , HasEqCertainly, HasEqCertainlyAsymmetric
  , HasEqCertainlyCE, HasEqCertainlyCN
  , notCertainlyDifferentFrom, certainlyEqualTo, certainlyNotEqualTo
  , (?==?), (!==!), (!/=!)
  -- ** Tests
  , specHasEq, specHasEqNotMixed
  , specConversion
  -- ** Specific comparisons
  , CanTestNaN(..)
  , CanTestFinite(..)
  , CanTestInteger(..)
  , CanTestZero(..), specCanTestZero
  , CanPickNonZero(..), specCanPickNonZero
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf
import Data.Ratio

import Test.Hspec
import Test.QuickCheck as QC

import Numeric.CollectErrors
import Control.CollectErrors

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool

infix  4  ==, /=
infix 4 ?==?
infix 4 !==!, !/=!

{---- Equality tests -----}

type HasEq t1 t2 =
    (HasEqAsymmetric t1 t2,  HasEqAsymmetric t2 t1,
     EqCompareType t1 t2 ~ EqCompareType t2 t1)

type HasEqCertainlyAsymmetric t1 t2 =
    (HasEqAsymmetric t1 t2, CanTestCertainly (EqCompareType t1 t2))

type HasEqCertainly t1 t2 =
  (HasEq t1 t2, CanTestCertainly (EqCompareType t1 t2))

type HasEqCertainlyCE es t1 t2 =
  (HasEqCertainly t1 t2,
   HasEqCertainly (EnsureCE es t1) (EnsureCE es t2))
  --  HasEqCertainly (WithoutCE es t1) (WithoutCE es t2),
  --  CanTestCertainly (WithoutCE es (EqCompareType (WithoutCE es t1) (WithoutCE es t2))),
  --  IsBool (WithoutCE es (EqCompareType (WithoutCE es t1) (WithoutCE es t2))),
  --  CanEnsureCE es (EqCompareType (WithoutCE es t1) (WithoutCE es t2)),
  --  CanEnsureCE es (WithoutCE es (EqCompareType (WithoutCE es t1) (WithoutCE es t2))),
  --  WithoutCE es (WithoutCE es (EqCompareType (WithoutCE es t1) (WithoutCE es t2)))
  --    ~ (WithoutCE es (EqCompareType (WithoutCE es t1) (WithoutCE es t2))))

type HasEqCertainlyCN t1 t2 = HasEqCertainlyCE NumErrors t1 t2

class (IsBool (EqCompareType a b)) => HasEqAsymmetric a b where
    type EqCompareType a b
    type EqCompareType a b = Bool -- default
    equalTo :: a -> b -> (EqCompareType a b)
    -- default equalToA via Prelude for (->) and Bool:
    default equalTo :: (EqCompareType a b ~ Bool, a~b, P.Eq a) => a -> b -> EqCompareType a b
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

certainlyEqualTo :: (HasEqCertainlyAsymmetric a b) => a -> b -> Bool
certainlyEqualTo a b = isCertainlyTrue $ a == b
certainlyNotEqualTo :: (HasEqCertainlyAsymmetric a b) => a -> b -> Bool
certainlyNotEqualTo a b = isCertainlyTrue $ a /= b
notCertainlyDifferentFrom :: (HasEqCertainlyAsymmetric a b) => a -> b -> Bool
notCertainlyDifferentFrom a b = isNotFalse $ a == b

(?==?) :: (HasEqCertainlyAsymmetric a b) => a -> b -> Bool
(?==?) = notCertainlyDifferentFrom

(!==!) :: (HasEqCertainlyAsymmetric a b) => a -> b -> Bool
(!==!) = certainlyEqualTo

(!/=!) :: (HasEqCertainlyAsymmetric a b) => a -> b -> Bool
(!/=!) = certainlyNotEqualTo

{-|
  HSpec properties that each implementation of HasEq should satisfy.
 -}
specHasEq ::
 (Show t1, Show t2, Show t3, Arbitrary t1, Arbitrary t2,
  Arbitrary t3, CanTestCertainly (EqCompareType t1 t1),
  CanTestCertainly (EqCompareType t1 t2),
  CanTestCertainly (EqCompareType t2 t1),
  CanTestCertainly (EqCompareType t2 t3),
  CanTestCertainly
    (AndOrType (EqCompareType t1 t2) (EqCompareType t2 t3)),
  CanAndOrAsymmetric (EqCompareType t1 t2) (EqCompareType t2 t3),
  HasEqAsymmetric t1 t1, HasEqAsymmetric t1 t2,
  HasEqAsymmetric t2 t1, HasEqAsymmetric t2 t3)
  =>
  T t1 -> T t2 -> T t3 -> Spec
specHasEq (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "HasEq %s %s, HasEq %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "has reflexive ==" $ do
      property $ \ (x :: t1) -> not $ isCertainlyFalse (x == x)
    it "has anti-reflexive /=" $ do
      property $ \ (x :: t1) -> not $ isCertainlyTrue (x /= x)
    it "has stronly commutative ==" $ do
      property $ \ (x :: t1) (y :: t2) -> (x == y) `stronglyEquivalentTo` (y == x)
    it "has stronly commutative /=" $ do
      property $ \ (x :: t1) (y :: t2) -> (x /= y) `stronglyEquivalentTo` (y /= x)
    it "has stronly transitive ==" $ do
      property $ \ (x :: t1) (y :: t2) (z :: t3) -> ((x == y) && (y == z)) `stronglyImplies` (y == z)

{-|
  HSpec properties that each implementation of HasEq should satisfy.
 -}
specHasEqNotMixed ::
  (Show t, Arbitrary t, CanTestCertainly (EqCompareType t t),
   CanTestCertainly
     (AndOrType (EqCompareType t t) (EqCompareType t t)),
   HasEqAsymmetric t t)
  =>
  T t -> Spec
specHasEqNotMixed (t :: T t) = specHasEq t t t

{-|
  HSpec property of there-and-back conversion.
-}
specConversion :: -- this definition cannot be in Literals because it needs HasEq
  (Arbitrary t1, Show t1, HasEqCertainly t1 t1) =>
  T t1 -> T t2 -> (t1 -> t2) -> (t2 -> t1) ->  Spec
specConversion (T typeName1 :: T t1) (T typeName2 :: T t2) conv12 conv21 =
  describe "conversion" $ do
    it (printf "%s -> %s -> %s" typeName1 typeName2 typeName1) $ do
      property $ \ (x1 :: t1) ->
        x1 ?==? (conv21 $ conv12 x1)

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

instance
  (HasEqAsymmetric a b
  , CanEnsureCE es (EqCompareType a b)
  , CanEnsureCE es a, CanEnsureCE es b
  , IsBool (EnsureCE es (EqCompareType a b))
  , SuitableForCE es)
  =>
  HasEqAsymmetric (CollectErrors es a) (CollectErrors es  b)
  where
  type EqCompareType (CollectErrors es  a) (CollectErrors es  b) =
    EnsureCE es (EqCompareType a b)
  equalTo = lift2CE equalTo

$(declForTypes
  [[t| Bool |], [t| Maybe Bool |], [t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (HasEqAsymmetric $t b
      , CanEnsureCE es b
      , CanEnsureCE es (EqCompareType $t b)
      , IsBool (EnsureCE es (EqCompareType $t b))
      , SuitableForCE es)
      =>
      HasEqAsymmetric $t (CollectErrors es  b)
      where
      type EqCompareType $t (CollectErrors es  b) =
        EnsureCE es (EqCompareType $t b)
      equalTo = lift2TLCE equalTo

    instance
      (HasEqAsymmetric a $t
      , CanEnsureCE es a
      , CanEnsureCE es (EqCompareType a $t)
      , IsBool (EnsureCE es (EqCompareType a $t))
      , SuitableForCE es)
      =>
      HasEqAsymmetric (CollectErrors es a) $t
      where
      type EqCompareType (CollectErrors es  a) $t =
        EnsureCE es (EqCompareType a $t)
      equalTo = lift2TCE equalTo

  |]))


{---- Checking whether it is finite -----}

class CanTestNaN t where
  isNaN :: t -> Bool
  default isNaN :: (P.RealFloat t) => t -> Bool
  isNaN = P.isNaN

class CanTestFinite t where
  isInfinite :: t -> Bool
  default isInfinite :: (P.RealFloat t) => t -> Bool
  isInfinite = P.isInfinite
  isFinite :: t -> Bool
  default isFinite :: (P.RealFloat t) => t -> Bool
  isFinite x = (not $ P.isNaN x) && (not $ P.isInfinite x)

instance CanTestNaN Double
instance CanTestFinite Double

instance CanTestNaN Rational where
  isNaN = const False
instance CanTestFinite Rational where
  isInfinite = const False
  isFinite = const True

instance (CanTestNaN t, SuitableForCE es) => (CanTestNaN (CollectErrors es t)) where
  isNaN ce = getValueIfNoErrorCE ce isNaN (const False)

instance (CanTestFinite t, SuitableForCE es) => (CanTestFinite (CollectErrors es t)) where
  isInfinite ce = getValueIfNoErrorCE ce isInfinite (const False)
  isFinite ce = getValueIfNoErrorCE ce isFinite (const False)

{---- Checking whether it is an integer -----}

class CanTestInteger t where
  certainlyNotInteger :: t -> Bool
  certainlyInteger :: t -> Bool
  certainlyInteger s = case certainlyIntegerGetIt s of Just _ -> True; _ -> False
  certainlyIntegerGetIt :: t -> Maybe Integer

instance CanTestInteger Integer where
  certainlyNotInteger _ = False
  certainlyInteger _ = True
  certainlyIntegerGetIt n = Just n

instance CanTestInteger Int where
  certainlyNotInteger _ = False
  certainlyInteger _ = True
  certainlyIntegerGetIt n = Just (integer n)

instance CanTestInteger Rational where
  certainlyNotInteger q = (denominator q /= 1)
  certainlyInteger q = (denominator q == 1)
  certainlyIntegerGetIt q
    | denominator q == 1 = Just (numerator q)
    | otherwise = Nothing

instance CanTestInteger Double where
  certainlyNotInteger d =
    isInfinite d || isNaN d ||
      (P.floor d :: Integer) P.< P.ceiling d
  certainlyIntegerGetIt d
    | isFinite d && (dF P.== dC) = Just dF
    | otherwise = Nothing
    where
      dF = P.floor d
      dC = P.ceiling d

instance (CanTestInteger t, SuitableForCE es) => (CanTestInteger (CollectErrors es t)) where
  certainlyNotInteger ce = getValueIfNoErrorCE ce certainlyNotInteger (const False)
  certainlyIntegerGetIt ce = getValueIfNoErrorCE ce certainlyIntegerGetIt (const Nothing)

{---- Checking whether it is zero -----}

class CanTestZero t where
  isCertainlyZero :: t -> Bool
  isCertainlyNonZero :: t -> Bool
  default isCertainlyZero :: (HasEqCertainly t Integer) => t -> Bool
  isCertainlyZero a = isCertainlyTrue (a == 0)
  default isCertainlyNonZero :: (HasEqCertainly t Integer) => t -> Bool
  isCertainlyNonZero a = isCertainlyTrue (a /= 0)

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
      property $ \ (x :: Integer) ->
        x /= 0 ==> (not $ isCertainlyZero (convertExactly x :: t))
    it "converted non-zero Integer is isCertainlyNonZero" $ do
      property $ \ (x :: Integer) ->
        x /= 0 ==> (isCertainlyNonZero (convertExactly x :: t))
    it "converted 0.0 is not isCertainlyNonZero" $ do
      (isCertainlyNonZero (convertExactly 0 :: t)) `shouldBe` False

instance CanTestZero Int
instance CanTestZero Integer
instance CanTestZero Rational
instance CanTestZero Double

instance (CanTestZero t, SuitableForCE es) => (CanTestZero (CollectErrors es t)) where
  isCertainlyZero ce = getValueIfNoErrorCE ce isCertainlyZero (const False)
  isCertainlyNonZero ce = getValueIfNoErrorCE ce isCertainlyNonZero (const False)


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
  pickNonZero :: [(t,s)] -> Maybe (t,s)
  default pickNonZero :: (CanTestZero t, Show t) => [(t,s)] -> Maybe (t,s)
  pickNonZero list = aux list
    where
      aux ((a,b):rest)
        | isCertainlyNonZero a = Just (a,b)
        | otherwise = aux rest
      aux [] = Nothing

{-|
  HSpec properties that each implementation of CanPickNonZero should satisfy.
 -}
specCanPickNonZero ::
  (CanPickNonZero t, CanTestZero t, ConvertibleExactly Integer t, Show t, Arbitrary t)
  =>
  T t -> Spec
specCanPickNonZero (T typeName :: T t) =
  describe (printf "CanPickNonZero %s" typeName) $ do
    it "picks a non-zero element if there is one" $ do
      property $ \ (xs :: [(t, ())]) ->
        or (map (isCertainlyNonZero . fst) xs) -- if at least one is non-zero
        ==>
        (case pickNonZero xs of
          Just (v, _) -> isCertainlyNonZero v
          _ -> False)
    it "returns Nothing when all the elements are 0" $ do
      case pickNonZero [(convertExactly i :: t, ()) | i <- [0,0,0]] of
        Nothing -> True
        _ -> False

instance CanPickNonZero Int
instance CanPickNonZero Integer
instance CanPickNonZero Rational

instance (CanPickNonZero a, SuitableForCE es) => (CanPickNonZero (CollectErrors es a)) where
  pickNonZero =
    fmap (\(v,s) -> (pure v,s))
    . pickNonZero
    . filterValuesWithoutErrorCE
    . (map (\(vCN,s) -> fmap (\v -> (v,s)) vCN))

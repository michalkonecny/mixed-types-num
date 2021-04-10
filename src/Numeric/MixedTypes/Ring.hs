{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Ring
    Description :  Bottom-up typed multiplication with ring laws
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Ring
(
  -- * Ring
  CanAddSubMulBy, Ring, OrderedRing, OrderedCertainlyRing
  -- * Multiplication
  , CanMul, CanMulAsymmetric(..), CanMulBy, CanMulSameType
  , (*), product
  -- ** Tests
  , specCanMul, specCanMulNotMixed, specCanMulSameType
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
import Numeric.CollectErrors ( CN )

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub

{----- Ring -----}

type CanAddSubMulBy t s =
  (CanAddThis t s, CanSubThis t s, CanSub s t, SubType s t ~ t, CanMulBy t s)

type Ring t =
  (CanNegSameType t, CanAddSameType t, CanSubSameType t, CanMulSameType t,
   HasEq t t,
   HasEq t Integer, CanAddSubMulBy t Integer,
   HasEq t Int, CanAddSubMulBy t Int,
   HasIntegers t)

class
  (Ring t
  , HasEq t t
  , HasEq t Int, HasEq t Integer
  , HasOrder t t
  , HasOrder t Int, HasOrder t Integer)
  => OrderedRing t

instance OrderedRing Integer
instance OrderedRing (CN Integer)
instance OrderedRing Rational
instance OrderedRing (CN Rational)

class
  (Ring t
  , HasEqCertainly t t
  , HasEqCertainly t Int, HasEq t Integer
  , HasOrderCertainly t t
  , HasOrderCertainly t Int, HasOrderCertainly t Integer
  , CanTestPosNeg t)
  => OrderedCertainlyRing t

instance OrderedCertainlyRing Integer
instance OrderedCertainlyRing (CN Integer)
instance OrderedCertainlyRing Rational
instance OrderedCertainlyRing (CN Rational)

{---- Multiplication -----}

type CanMul t1 t2 =
  (CanMulAsymmetric t1 t2, CanMulAsymmetric t2 t1,
   MulType t1 t2 ~ MulType t2 t1)

{-|
  A replacement for Prelude's `P.*`.  If @t1 = t2@ and @Num t1@,
  then one can use the default implementation to mirror Prelude's @*@.
-}
class CanMulAsymmetric t1 t2 where
  type MulType t1 t2
  type MulType t1 t2 = t1 -- default
  mul :: t1 -> t2 -> MulType t1 t2
  default mul :: (MulType t1 t2 ~ t1, t1~t2, P.Num t1) => t1 -> t2 -> MulType t1 t2
  mul = (P.*)

infixl 7  *

(*) :: (CanMulAsymmetric t1 t2) => t1 -> t2 -> MulType t1 t2
(*) = mul

type CanMulBy t1 t2 =
  (CanMul t1 t2, MulType t1 t2 ~ t1)
type CanMulSameType t =
  CanMulBy t t

product :: (CanMulSameType t, ConvertibleExactly Integer t) => [t] -> t
product xs = List.foldl' mul (convertExactly 1) xs

{-|
  HSpec properties that each implementation of CanMul should satisfy.
 -}
specCanMul ::
  (Show t1, Show t2, Show t3, Show (MulType t1 t2),
   Show (MulType t2 t1), Show (MulType t1 (MulType t2 t3)),
   Show (MulType (MulType t1 t2) t3),
   Show (MulType t1 (AddType t2 t3)),
   Show (AddType (MulType t1 t2) (MulType t1 t3)), Arbitrary t1,
   Arbitrary t2, Arbitrary t3, ConvertibleExactly Integer t2,
   CanTestCertainly (EqCompareType (MulType t1 t2) t1),
   CanTestCertainly (EqCompareType (MulType t1 t2) (MulType t2 t1)),
   CanTestCertainly
     (EqCompareType
        (MulType t1 (MulType t2 t3)) (MulType (MulType t1 t2) t3)),
   CanTestCertainly
     (EqCompareType
        (MulType t1 (AddType t2 t3))
        (AddType (MulType t1 t2) (MulType t1 t3))),
   HasEqAsymmetric (MulType t1 t2) t1,
   HasEqAsymmetric (MulType t1 t2) (MulType t2 t1),
   HasEqAsymmetric
     (MulType t1 (MulType t2 t3)) (MulType (MulType t1 t2) t3),
   HasEqAsymmetric
     (MulType t1 (AddType t2 t3))
     (AddType (MulType t1 t2) (MulType t1 t3)),
   CanAddAsymmetric t2 t3,
   CanAddAsymmetric (MulType t1 t2) (MulType t1 t3),
   CanMulAsymmetric t1 t2, CanMulAsymmetric t1 t3,
   CanMulAsymmetric t1 (MulType t2 t3),
   CanMulAsymmetric t1 (AddType t2 t3), CanMulAsymmetric t2 t1,
   CanMulAsymmetric t2 t3, CanMulAsymmetric (MulType t1 t2) t3)
  =>
  T t1 -> T t2 -> T t3 -> Spec
specCanMul (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "CanMul %s %s, CanMul %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "absorbs 1" $ do
      property $ \ (x :: t1) -> let one = (convertExactly 1 :: t2) in (x * one) ?==?$ x
    it "is commutative" $ do
      property $ \ (x :: t1) (y :: t2) -> (x * y) ?==?$ (y * x)
    it "is associative" $ do
      property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x * (y * z)) ?==?$ ((x * y) * z)
    it "distributes over addition" $ do
      property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x * (y + z)) ?==?$ (x * y) + (x * z)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

{-|
  HSpec properties that each implementation of CanMul should satisfy.
 -}
specCanMulNotMixed ::
  (Show t, Show (MulType t t), Show (MulType t (MulType t t)),
   Show (MulType (MulType t t) t), Show (MulType t (AddType t t)),
   Show (AddType (MulType t t) (MulType t t)), Arbitrary t,
   ConvertibleExactly Integer t,
   CanTestCertainly (EqCompareType (MulType t t) t),
   CanTestCertainly (EqCompareType (MulType t t) (MulType t t)),
   CanTestCertainly
     (EqCompareType
        (MulType t (MulType t t)) (MulType (MulType t t) t)),
   CanTestCertainly
     (EqCompareType
        (MulType t (AddType t t)) (AddType (MulType t t) (MulType t t))),
   HasEqAsymmetric (MulType t t) t,
   HasEqAsymmetric (MulType t t) (MulType t t),
   HasEqAsymmetric
     (MulType t (MulType t t)) (MulType (MulType t t) t),
   HasEqAsymmetric
     (MulType t (AddType t t)) (AddType (MulType t t) (MulType t t)),
   CanAddAsymmetric t t, CanAddAsymmetric (MulType t t) (MulType t t),
   CanMulAsymmetric t t, CanMulAsymmetric t (MulType t t),
   CanMulAsymmetric t (AddType t t),
   CanMulAsymmetric (MulType t t) t)
  =>
  T t -> Spec
specCanMulNotMixed (t :: T t) = specCanMul t t t

{-|
  HSpec properties that each implementation of CanMulSameType should satisfy.
 -}
specCanMulSameType ::
  (Show t, ConvertibleExactly Integer t,
   CanTestCertainly (EqCompareType t t), HasEqAsymmetric t t,
   CanMulAsymmetric t t, MulType t t ~ t)
   =>
   T t -> Spec
specCanMulSameType (T typeName :: T t) =
  describe (printf "CanMulSameType %s" typeName) $ do
    it "has product working over integers" $ do
      property $ \ (xsi :: [Integer]) ->
        (product $ (map convertExactly xsi :: [t])) ?==?$ (convertExactly (product xsi) :: t)
    it "has product [] = 1" $ do
        (product ([] :: [t])) ?==?$ (convertExactly 1 :: t)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

instance CanMulAsymmetric Int Int where
  type MulType Int Int = Integer -- do not risk overflow
  mul a b = (integer a) P.* (integer b)
instance CanMulAsymmetric Integer Integer
instance CanMulAsymmetric Rational Rational
instance CanMulAsymmetric Double Double

instance CanMulAsymmetric Int Integer where
  type MulType Int Integer = Integer
  mul = convertFirst mul
instance CanMulAsymmetric Integer Int where
  type MulType Integer Int = Integer
  mul = convertSecond mul

instance CanMulAsymmetric Int Rational where
  type MulType Int Rational = Rational
  mul = convertFirst mul
instance CanMulAsymmetric Rational Int where
  type MulType Rational Int = Rational
  mul = convertSecond mul

instance CanMulAsymmetric Integer Rational where
  type MulType Integer Rational = Rational
  mul = convertFirst mul
instance CanMulAsymmetric Rational Integer where
  type MulType Rational Integer = Rational
  mul = convertSecond mul

instance CanMulAsymmetric Int Double where
  type MulType Int Double = Double
  mul n d = mul (double n) d
instance CanMulAsymmetric Double Int where
  type MulType Double Int = Double
  mul d n = mul d (double n)

instance CanMulAsymmetric Integer Double where
  type MulType Integer Double = Double
  mul n d = mul (double n) d
instance CanMulAsymmetric Double Integer where
  type MulType Double Integer = Double
  mul d n = mul d (double n)

instance CanMulAsymmetric Rational Double where
  type MulType Rational Double = Double
  mul n d = mul (double n) d
instance CanMulAsymmetric Double Rational where
  type MulType Double Rational = Double
  mul d n = mul d (double n)

instance (CanMulAsymmetric a b) => CanMulAsymmetric [a] [b] where
  type MulType [a] [b] = [MulType a b]
  mul (x:xs) (y:ys) = (mul x y) : (mul xs ys)
  mul _ _ = []

instance (CanMulAsymmetric a b) => CanMulAsymmetric (Maybe a) (Maybe b) where
  type MulType (Maybe a) (Maybe b) = Maybe (MulType a b)
  mul (Just x) (Just y) = Just (mul x y)
  mul _ _ = Nothing

instance
  (CanMulAsymmetric a b, CanBeErrors es)
  =>
  CanMulAsymmetric (CollectErrors es a) (CollectErrors es  b)
  where
  type MulType (CollectErrors es a) (CollectErrors es b) =
    CollectErrors es (MulType a b)
  mul = CE.lift2 mul

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (CanMulAsymmetric $t b, CanBeErrors es)
      =>
      CanMulAsymmetric $t (CollectErrors es  b)
      where
      type MulType $t (CollectErrors es  b) =
        CollectErrors es (MulType $t b)
      mul = CE.liftT1 mul

    instance
      (CanMulAsymmetric a $t, CanBeErrors es)
      =>
      CanMulAsymmetric (CollectErrors es a) $t
      where
      type MulType (CollectErrors es  a) $t =
        CollectErrors es (MulType a $t)
      mul = CE.lift1T mul
  |]))

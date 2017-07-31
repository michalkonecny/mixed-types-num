{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Field
    Description :  Bottom-up typed division
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Field
(
  -- * Field
  CanAddSubMulDivCNBy, Field, OrderedField, OrderedCertainlyField
  -- * Division
  , CanDiv(..), CanDivBy, CanDivCNBy, CanDivSameType, CanDivCNSameType
  , CanRecip, CanRecipSameType, CanRecipCNSameType
  , (/), (/!), recip
  , powUsingMulRecip
  -- ** Tests
  , specCanDiv, specCanDivNotMixed
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import Numeric.CollectErrors
import Control.CollectErrors

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxAbs
-- import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Ring

{----- Field -----}

type CanAddSubMulDivCNBy t s =
  (CanAddSubMulBy t s, CanAddSubMulBy (EnsureCN t) s, CanDivCNBy t s)

class
  (Ring t,
   CanDivCNSameType t, CanRecipCNSameType t,
   CanAddSubMulDivCNBy t Rational,
   CanAddSubMulDivCNBy t Integer,
   CanAddSubMulDivCNBy t Int
  )
  =>
  Field t

instance Field Rational
instance Field (CN Rational)

class
  (Field t, OrderedRing t, HasOrder t Rational, HasOrder (EnsureCN t) Rational)
  => OrderedField t

instance OrderedField Rational
instance OrderedField (CN Rational)

class
  (Field t, OrderedCertainlyRing t, HasOrderCertainly t Rational, HasOrderCertainly (EnsureCN t) Rational)
  => OrderedCertainlyField t

instance OrderedCertainlyField Rational
instance OrderedCertainlyField (CN Rational)

{---- Division -----}

{-|
  A replacement for Prelude's binary `P./`.  If @t1 = t2@ and @Fractional t1@,
  then one can use the default implementation to mirror Prelude's @/@.
-}
class CanDiv t1 t2 where
  type DivTypeNoCN t1 t2
  divideNoCN :: t1 -> t2 -> DivTypeNoCN t1 t2
  type DivType t1 t2
  type DivType t1 t2 = EnsureCN (DivTypeNoCN t1 t2)
  divide :: t1 -> t2 -> DivType t1 t2
  default divide ::
    (CanTestZero t2, CanEnsureCN (DivTypeNoCN t1 t2)
    , DivType t1 t2 ~ EnsureCN (DivTypeNoCN t1 t2))
    =>
    t1 -> t2 -> DivType t1 t2
  divide = divideCN divideNoCN

divideCN ::
  (CanTestZero t2, CanEnsureCN t3)
  =>
  (t1 -> t2 -> t3) ->
  t1 -> t2 -> EnsureCN t3
divideCN unsafeDivide a b
  | isCertainlyZero b = noValueNumErrorCertainECN sample_v DivByZero
  | isCertainlyNonZero b = ensureCN $ a `unsafeDivide` b
  | otherwise = noValueNumErrorPotentialECN sample_v DivByZero
  where
  sample_v = Just $ unsafeDivide a b

infixl 7  /,/!

(/) :: (CanDiv t1 t2) => t1 -> t2 -> DivType t1 t2
(/) = divide

(/!) :: (CanDiv t1 t2) => t1 -> t2 -> DivTypeNoCN t1 t2
(/!) = divideNoCN

type CanRecip t =
  (CanDiv Integer t)

type CanRecipSameType t =
  (CanDiv Integer t, DivType Integer t ~ t, DivTypeNoCN Integer t ~ t)

type CanRecipCNSameType t =
  (CanDiv Integer t, DivType Integer t ~ EnsureCN t, DivTypeNoCN Integer t ~ t
  ,CanEnsureCN t
  ,CanDiv Integer (EnsureCN t), DivType Integer (EnsureCN t) ~ EnsureCN t, DivTypeNoCN Integer (EnsureCN t) ~ (EnsureCN t))

recip :: (CanRecip t) => t -> DivType Integer t
recip = divide 1

type CanDivBy t1 t2 =
  (CanDiv t1 t2, DivType t1 t2 ~ t1, DivTypeNoCN t1 t2 ~ t1)
type CanDivSameType t =
  CanDivBy t t

type CanDivCNBy t1 t2 =
  (CanDiv t1 t2, DivType t1 t2 ~ EnsureCN t1, DivTypeNoCN t1 t2 ~ t1
  , CanEnsureCN t1
  , CanDiv (EnsureCN t1) t2, DivType (EnsureCN t1) t2 ~ EnsureCN t1, DivTypeNoCN (EnsureCN t1) t2 ~ (EnsureCN t1))
type CanDivCNSameType t =
  (CanDivCNBy t t
  , CanDiv (EnsureCN t) (EnsureCN t), DivType (EnsureCN t) (EnsureCN t) ~ EnsureCN t, DivTypeNoCN (EnsureCN t) (EnsureCN t) ~ (EnsureCN t))

{-|
  HSpec properties that each implementation of CanDiv should satisfy.
 -}
specCanDiv ::
  (Show t1, Show t2, Show (DivType Integer (DivType Integer t1)),
   Show (DivType t1 t2), Show (DivType t1 t1),
   Show (MulType t1 (DivType t1 t2)), Arbitrary t1, Arbitrary t2,
   ConvertibleExactly Integer t1, ConvertibleExactly Integer t2,
   CanTestCertainly
     (EqCompareType (DivType Integer (DivType Integer t1)) t1),
   CanTestCertainly (EqCompareType (DivType t1 t2) t1),
   CanTestCertainly (EqCompareType (DivType t1 t1) t1),
   CanTestCertainly
     (EqCompareType (DivType t1 t2) (MulType t1 (DivType t1 t2))),
   HasEqAsymmetric (DivType Integer (DivType Integer t1)) t1,
   HasEqAsymmetric (DivType t1 t2) t1,
   HasEqAsymmetric (DivType t1 t2) (MulType t1 (DivType t1 t2)),
   HasEqAsymmetric (DivType t1 t1) t1, CanTestZero t1, CanTestZero t2,
   CanTestZero (DivType Integer t1),
   CanMulAsymmetric t1 (DivType t1 t2), CanDiv t1 t1, CanDiv t1 t2,
   CanDiv Integer t1, CanDiv Integer (DivType Integer t1))
  =>
  T t1 -> T t2 -> Spec
specCanDiv (T typeName1 :: T t1) (T typeName2 :: T t2) =
  describe (printf "CanDiv %s %s" typeName1 typeName2) $ do
    it "recip(recip x) = x" $ do
      property $ \ (x :: t1) ->
        (isCertainlyNonZero x && isCertainlyNonZero (recip x)) ==>
          recip (recip x) ?==?$ x
    it "x/1 = x" $ do
      property $ \ (x :: t1) -> let one = (convertExactly 1 :: t2) in (x / one) ?==?$ x
    it "x/x = 1" $ do
      property $ \ (x :: t1) ->
        (isCertainlyNonZero x) ==>
          let one = (convertExactly 1 :: t1) in (x / x) ?==?$ one
    it "x/y = x*(1/y)" $ do
      property $ \ (x :: t1) (y :: t2) ->
        (isCertainlyNonZero y) ==>
          let one = (convertExactly 1 :: t1) in (x / y) ?==?$ x * (one/y)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

{-|
  HSpec properties that each implementation of CanDiv should satisfy.
 -}
specCanDivNotMixed ::
  (Show t, Show (DivType Integer (DivType Integer t)),
   Show (DivType t t), Show (MulType t (DivType t t)), Arbitrary t,
   ConvertibleExactly Integer t,
   CanTestCertainly
     (EqCompareType (DivType Integer (DivType Integer t)) t),
   CanTestCertainly (EqCompareType (DivType t t) t),
   CanTestCertainly
     (EqCompareType (DivType t t) (MulType t (DivType t t))),
   HasEqAsymmetric (DivType Integer (DivType Integer t)) t,
   HasEqAsymmetric (DivType t t) t,
   HasEqAsymmetric (DivType t t) (MulType t (DivType t t)),
   CanTestZero t, CanTestZero (DivType Integer t),
   CanMulAsymmetric t (DivType t t), CanDiv t t, CanDiv Integer t,
   CanDiv Integer (DivType Integer t))
  =>
  T t -> Spec
specCanDivNotMixed (t :: T t) = specCanDiv t t

instance CanDiv Int Int where
  type DivTypeNoCN Int Int = Rational
  divideNoCN a b = (P./) (rational a) (rational b)

instance CanDiv Integer Integer where
  type DivTypeNoCN Integer Integer = Rational
  divideNoCN a b = (P./) (rational a) (rational b)
instance CanDiv Rational Rational where
  type DivTypeNoCN Rational Rational = Rational
  divideNoCN = (P./)

instance CanDiv Int Integer where
  type DivTypeNoCN Int Integer = Rational
  divideNoCN a b = (P./) (rational a) (rational b)
instance CanDiv Integer Int where
  type DivTypeNoCN Integer Int = Rational
  divideNoCN a b = (P./) (rational a) (rational b)

instance CanDiv Int Rational where
  type DivTypeNoCN Int Rational = Rational
  divideNoCN = convertFirst divideNoCN
instance CanDiv Rational Int where
  type DivTypeNoCN Rational Int = Rational
  divideNoCN = convertSecond divideNoCN

instance CanDiv Integer Rational where
  type DivTypeNoCN Integer Rational = Rational
  divideNoCN = convertFirst divideNoCN
instance CanDiv Rational Integer where
  type DivTypeNoCN Rational Integer = Rational
  divideNoCN = convertSecond divideNoCN

instance CanDiv Double Double where
  type DivTypeNoCN Double Double = Double
  divideNoCN = (P./)
  type DivType Double Double = Double
  divide = (P./)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ t -> [d|

    instance CanDiv $t Double where
      type DivType $t Double = Double
      divide n d = divide (double n) d
      type DivTypeNoCN $t Double = Double
      divideNoCN n d = divide (double n) d
    instance CanDiv Double $t where
      type DivType Double $t = Double
      divide d n = divide d (double n)
      type DivTypeNoCN Double $t = Double
      divideNoCN d n = divide d (double n)
  |]))

instance (CanDiv a b) => CanDiv [a] [b] where
  type DivTypeNoCN [a] [b] = [DivTypeNoCN a b]
  divideNoCN (x:xs) (y:ys) = (divideNoCN x y) : (divideNoCN xs ys)
  divideNoCN _ _ = []
  type DivType [a] [b] = [DivType a b]
  divide (x:xs) (y:ys) = (divide x y) : (divide xs ys)
  divide _ _ = []

instance (CanDiv a b) => CanDiv (Maybe a) (Maybe b) where
  type DivType (Maybe a) (Maybe b) = Maybe (DivType a b)
  divide (Just x) (Just y) = Just (divide x y)
  divide _ _ = Nothing
  type DivTypeNoCN (Maybe a) (Maybe b) = Maybe (DivTypeNoCN a b)
  divideNoCN (Just x) (Just y) = Just (divideNoCN x y)
  divideNoCN _ _ = Nothing

instance
  (CanDiv a b
  , CanEnsureCE es a, CanEnsureCE es b
  , CanEnsureCE es (DivType a b)
  , CanEnsureCE es (DivTypeNoCN a b)
  , SuitableForCE es)
  =>
  CanDiv (CollectErrors es a) (CollectErrors es  b)
  where
  type DivType (CollectErrors es a) (CollectErrors es b) =
    EnsureCE es (DivType a b)
  divide = lift2CE divide
  type DivTypeNoCN (CollectErrors es a) (CollectErrors es b) =
    EnsureCE es (DivTypeNoCN a b)
  divideNoCN = lift2CE divideNoCN

powUsingMulRecip ::
  (CanBeInteger e,
   CanRecipCNSameType t, CanMulSameType t, CanEnsureCN t)
   =>
   t -> t -> e -> EnsureCN t
powUsingMulRecip one x nPre
  | n < 0 = recip $ powUsingMul one x (negate n)
  | otherwise = ensureCN $ powUsingMul one x n
  where
  n = integer nPre

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (CanDiv $t b
      , CanEnsureCE es b
      , CanEnsureCE es (DivType $t b)
      , CanEnsureCE es (DivTypeNoCN $t b)
      , SuitableForCE es)
      =>
      CanDiv $t (CollectErrors es  b)
      where
      type DivType $t (CollectErrors es  b) =
        EnsureCE es (DivType $t b)
      divide = lift2TLCE divide
      type DivTypeNoCN $t (CollectErrors es  b) =
        EnsureCE es (DivTypeNoCN $t b)
      divideNoCN = lift2TLCE divideNoCN

    instance
      (CanDiv a $t
      , CanEnsureCE es a
      , CanEnsureCE es (DivType a $t)
      , CanEnsureCE es (DivTypeNoCN a $t)
      , SuitableForCE es)
      =>
      CanDiv (CollectErrors es a) $t
      where
      type DivType (CollectErrors es  a) $t =
        EnsureCE es (DivType a $t)
      divide = lift2TCE divide
      type DivTypeNoCN (CollectErrors es  a) $t =
        EnsureCE es (DivTypeNoCN a $t)
      divideNoCN = lift2TCE divideNoCN
  |]))

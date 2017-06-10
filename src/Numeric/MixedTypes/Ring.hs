{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Ring
    Description :  Bottom-up typed multiplication and exponent
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Ring
(
  -- * Ring
  CanAddSubMulBy, Ring, CertainlyEqRing, OrderedRing, OrderedCertainlyRing
  -- * Multiplication
  , CanMul, CanMulAsymmetric(..), CanMulBy, CanMulSameType
  , (*), product
  -- ** Tests
  , specCanMul, specCanMulNotMixed, specCanMulSameType, CanMulX
  -- * Exponentiation
  , CanPow(..), CanPowBy, CanPowCNBy
  , (^), (^^), (**)
  , powUsingMul
  -- ** Tests
  , specCanPow, CanPowX
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import Numeric.CollectErrors
import Control.CollectErrors

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub

{----- Ring -----}

type CanAddSubMulBy t s =
  (CanAddThis t s, CanSubThis t s, CanMulBy t s)

type Ring t =
  (CanNegSameType t, CanAddSameType t, CanSubSameType t, CanMulSameType t,
   CanPowCNBy t Integer, CanPowCNBy t Int,
   HasEq t t,
   HasEq t Integer, CanAddSubMulBy t Integer,
   HasEq t Int, CanAddSubMulBy t Int,
   HasIntegers t)

type CertainlyEqRing t =
  (Ring t, HasEqCertainly t t, HasEqCertainly t Int, HasEqCertainly t Integer)

type OrderedRing t =
  (Ring t, HasOrder t t, HasOrder t Int, HasOrder t Integer)

type OrderedCertainlyRing t =
  (CertainlyEqRing t, HasOrderCertainly t t, HasOrderCertainly t Int, HasOrderCertainly t Integer,
  CanTestPosNeg t)

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
  default mul :: (MulType t1 t2 ~ t1, t1~t2, P.Num t1) => t1 -> t1 -> t1
  mul = (P.*)

infixl 8  ^, ^^
infixl 7  *

(*) :: (CanMulAsymmetric t1 t2) => t1 -> t2 -> MulType t1 t2
(*) = mul

(^) :: (CanPow t1 t2) => t1 -> t2 -> PowType t1 t2
(^) = pow

{-| A synonym of `^` -}
(^^) :: (CanPow t1 t2) => t1 -> t2 -> PowType t1 t2
(^^) = (^)

{-| A synonym of `^` -}
(**) :: (CanPow t1 t2) => t1 -> t2 -> (PowType t1 t2)
(**) = (^)

type CanMulBy t1 t2 =
  (CanMul t1 t2, MulType t1 t2 ~ t1)
type CanMulSameType t =
  CanMulBy t t

product :: (CanMulSameType t, ConvertibleExactly Integer t) => [t] -> t
product xs = List.foldl' mul (convertExactly 1) xs

{-| Compound type constraint useful for test definition. -}
type CanMulX t1 t2 =
  (CanMul t1 t2,
   Show t1, Arbitrary t1,
   Show t2, Arbitrary t2,
   Show (MulType t1 t2),
   HasEqCertainly t1 (MulType t1 t2),
   HasEqCertainly t2 (MulType t1 t2),
   HasEqCertainly (MulType t1 t2) (MulType t1 t2),
   HasOrderCertainly t1 (MulType t1 t2),
   HasOrderCertainly t2 (MulType t1 t2),
   HasOrderCertainly (MulType t1 t2) (MulType t1 t2))

{-|
  HSpec properties that each implementation of CanMul should satisfy.
 -}
specCanMul ::
  (CanMulX t1 t2,
   CanMulX t1 t3,
   CanMulX t2 t3,
   CanMulX t1 (MulType t2 t3),
   CanMulX (MulType t1 t2) t3,
   HasEqCertainly (MulType t1 (MulType t2 t3)) (MulType (MulType t1 t2) t3),
   CanAdd t2 t3,
   CanMulX t1 (AddType t2 t3),
   CanAddX (MulType t1 t2) (MulType t1 t3),
   HasEqCertainly (MulType t1 (AddType t2 t3)) (AddType (MulType t1 t2) (MulType t1 t3)),
   ConvertibleExactly Integer t2)
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
  (CanMulX t t,
   CanMulX t (MulType t t),
   HasEqCertainly (MulType (MulType t t) t) (MulType t (MulType t t)),
   CanAdd t t,
   CanMulX t (AddType t t),
   CanAddX (MulType t t) (MulType t t),
   HasEqCertainly (MulType t (AddType t t)) (AddType (MulType t t) (MulType t t)),
   ConvertibleExactly Integer t)
  =>
  T t -> Spec
specCanMulNotMixed t = specCanMul t t t

{-|
  HSpec properties that each implementation of CanMulSameType should satisfy.
 -}
specCanMulSameType ::
  (ConvertibleExactly Integer t, Show t,
   HasEqCertainly t t, CanMulSameType t)
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
  (CanMulAsymmetric a b
  , CanEnsureCE es (MulType a b)
  , SuitableForCE es)
  =>
  CanMulAsymmetric (CollectErrors es a) (CollectErrors es  b)
  where
  type MulType (CollectErrors es a) (CollectErrors es b) =
    EnsureCE es (MulType a b)
  mul = lift2CE mul

{---- Exponentiation -----}

{-|
  A replacement for Prelude's binary `P.^` and `P.^^`.  If @Num t1@ and @Integral t2@,
  then one can use the default implementation to mirror Prelude's @^@.
-}
class CanPow t1 t2 where
  type PowType t1 t2
  type PowType t1 t2 = t1 -- default
  pow :: t1 -> t2 -> PowType t1 t2
  -- default pow :: (PowType t1 t2 ~ t1, P.Num t1, P.Integral t2) => t1 -> t2 -> t1
  -- pow = (P.^)

integerPowCN ::
  (HasOrderCertainly b Integer, HasOrderCertainly e Integer,
   HasEqCertainly b Integer, HasEqCertainly e Integer,
   CanEnsureCN r)
  =>
  (b -> e -> r) -> b -> e -> EnsureCN r
integerPowCN unsafeIntegerPow b n
  | n !<! 0 =
    noValueNumErrorCertainECN sample_v $ OutOfRange "illegal integer pow: negative exponent"
  | n !==! 0 && b !==! 0 =
    noValueNumErrorCertainECN sample_v $ OutOfRange "illegal integer pow: 0^0"
  | n ?<? 0 =
    noValueNumErrorPotentialECN sample_v $ OutOfRange "illegal integer pow: negative exponent"
  | n ?==? 0 && b ?==? 0 =
    noValueNumErrorPotentialECN sample_v $ OutOfRange "illegal integer pow: 0^0"
  | otherwise =
    ensureCN $ unsafeIntegerPow b n
  where
  sample_v = Just (unsafeIntegerPow b n)

powCN ::
  (HasOrderCertainly b Integer, HasOrderCertainly e Integer,
   HasEqCertainly b Integer, CanTestInteger e,
   CanEnsureCN r)
  =>
  (b -> e -> r) -> b -> e -> EnsureCN r
powCN unsafePow b e
  | b !==! 0 && e !<=! 0 =
    noValueNumErrorCertainECN sample_v $ OutOfRange "illegal pow: 0^e with e <= 0"
  | b !<! 0 && certainlyNotInteger e =
    noValueNumErrorCertainECN sample_v $ OutOfRange "illegal pow: b^e with b < 0 and e non-integer"
  | b ?==? 0 && e ?<=? 0 =
    noValueNumErrorPotentialECN sample_v $ OutOfRange "illegal pow: 0^e with e <= 0"
  | b ?<? 0 && not (certainlyInteger e) =
    noValueNumErrorPotentialECN sample_v $ OutOfRange "illegal pow: b^e with b < 0 and e non-integer"
  | otherwise =
    ensureCN $ unsafePow b e
  where
  sample_v = Just (unsafePow b e)

powUsingMul ::
  (CanBeInteger e,
   CanMulSameType t, HasIntegers t)
   =>
   t -> e -> t
powUsingMul x nPre
  | n < 0 = error $ "powUsingMul is not defined for negative exponent " ++ show n
  | n == 0 = convertExactly 1
  | otherwise = aux n
  where
    n = integer nPre
    aux m
      | m == 1 = x
      | even m =
        let s = aux (m `div` 2) in s * s
      | otherwise =
        let s = aux ((m-1) `div` 2) in x * s * s

type CanPowBy t1 t2 =
  (CanPow t1 t2, PowType t1 t2 ~ t1)

type CanPowCNBy t1 t2 =
  (CanPow t1 t2, PowType t1 t2 ~ EnsureCN t1)

{-| Compound type constraint useful for test definition. -}
type CanPowX t1 t2 =
  (CanPow t1 t2,
   Show t1, Arbitrary t1,
   Show t2, Arbitrary t2,
   Show (PowType t1 t2))

{-|
  HSpec properties that each implementation of CanPow should satisfy.
 -}
specCanPow ::
  (CanPowX t1 t2,
   HasEqCertainly t1 (PowType t1 t2),
   ConvertibleExactly Integer t1,
   ConvertibleExactly Integer t2,
   CanTestPosNeg t2,
   CanAdd t2 Integer,
   CanMulX t1 (PowType t1 t2),
   CanPowX t1 (AddType t2 Integer),
   HasEqCertainly (MulType t1 (PowType t1 t2)) (PowType t1 (AddType t2 Integer)))
  =>
  T t1 -> T t2 -> Spec
specCanPow (T typeName1 :: T t1) (T typeName2 :: T t2) =
  describe (printf "CanPow %s %s" typeName1 typeName2) $ do
    it "x^0 = 1" $ do
      property $ \ (x :: t1) ->
        let one = (convertExactly 1 :: t1) in
        let z = (convertExactly 0 :: t2) in
        (x ^ z) ?==?$ one
    it "x^1 = x" $ do
      property $ \ (x :: t1) ->
        let one = (convertExactly 1 :: t2) in
        (x ^ one) ?==?$ x
    it "x^(y+1) = x*x^y" $ do
      property $ \ (x :: t1) (y :: t2) ->
        (isCertainlyNonNegative y) ==>
          x * (x ^ y) ?==?$ (x ^ (y + 1))
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

instance CanPow Integer Integer where
  type PowType Integer Integer = CN Integer
  pow = integerPowCN (P.^)
instance CanPow Integer Int where
  type PowType Integer Int = CN Integer
  pow = integerPowCN (P.^)
instance CanPow Int Integer where
  type PowType Int Integer = CN Integer
  pow x n = pow (integer x) n
instance CanPow Int Int where
  type PowType Int Int = CN Integer
  pow x n = pow (integer x) n
instance CanPow Rational Int where
  type PowType Rational Int = CN Rational
  pow = powCN (P.^^)
instance CanPow Rational Integer where
  type PowType Rational Integer = CN Rational
  pow = powCN (P.^^)
instance CanPow Double Int where pow = (P.^^)
instance CanPow Double Integer where pow = (P.^^)

-- instance (CanPow a b) => CanPow [a] [b] where
--   type PowType [a] [b] = [PowType a b]
--   pow (x:xs) (y:ys) = (pow x y) : (pow xs ys)
--   pow _ _ = []

instance (CanPow a b) => CanPow (Maybe a) (Maybe b) where
  type PowType (Maybe a) (Maybe b) = Maybe (PowType a b)
  pow (Just x) (Just y) = Just (pow x y)
  pow _ _ = Nothing

instance
  (CanPow a b
  , CanEnsureCE es (PowType a b)
  , SuitableForCE es)
  =>
  CanPow (CollectErrors es a) (CollectErrors es  b)
  where
  type PowType (CollectErrors es a) (CollectErrors es b) =
    EnsureCE es (PowType a b)
  pow = lift2CE pow

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (CanPow $t b
      , CanEnsureCE es (PowType $t b)
      , SuitableForCE es)
      =>
      CanPow $t (CollectErrors es  b)
      where
      type PowType $t (CollectErrors es  b) =
        EnsureCE es (PowType $t b)
      pow = lift2TLCE pow

    instance
      (CanPow a $t
      , CanEnsureCE es (PowType a $t)
      , SuitableForCE es)
      =>
      CanPow (CollectErrors es a) $t
      where
      type PowType (CollectErrors es  a) $t =
        EnsureCE es (PowType a $t)
      pow = lift2TCE pow

    instance
      (CanMulAsymmetric $t b
      , CanEnsureCE es (MulType $t b)
      , SuitableForCE es)
      =>
      CanMulAsymmetric $t (CollectErrors es  b)
      where
      type MulType $t (CollectErrors es  b) =
        EnsureCE es (MulType $t b)
      mul = lift2TLCE mul

    instance
      (CanMulAsymmetric a $t
      , CanEnsureCE es (MulType a $t)
      , SuitableForCE es)
      =>
      CanMulAsymmetric (CollectErrors es a) $t
      where
      type MulType (CollectErrors es  a) $t =
        EnsureCE es (MulType a $t)
      mul = lift2TCE mul
  |]))

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
  Ring
  -- * Multiplication
  , CanMul, CanMulAsymmetric(..), CanMulBy, CanMulSameType
  , (*), product
  -- ** Tests
  , specCanMul, specCanMulNotMixed, specCanMulSameType, CanMulX
  -- * Exponentiation
  , CanPow(..), CanPowBy
  , (^), (^^)
  -- ** Tests
  , specCanPow, CanPowX
)
where

import Prelude hiding
  (fromInteger,
   negate,not,(&&),(||),and,or,
   (==), (/=), (>), (<), (<=), (>=),
   abs, min, max, minimum, maximum,
   (-), (+), sum, (*), (^), (^^), product)
import qualified Prelude as P
import Text.Printf

import qualified Data.List as List

import Test.Hspec
import qualified Test.QuickCheck as QC

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd
-- import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub

{----- Ring -----}

type Ring t =
    (CanNegSameType t, CanAddSameType t, CanSubSameType t, CanMulSameType t,
     CanPowBy t Integer, CanPowBy t Int,
     HasEq t t, Convertible Integer t)

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

type CanMulBy t1 t2 =
  (CanMul t1 t2, MulType t1 t2 ~ t1)
type CanMulSameType t =
  CanMulBy t t

product :: (CanMulSameType t, Convertible Integer t) => [t] -> t
product xs = List.foldl' mul (convert 1) xs

{-| Compound type constraint useful for test definition. -}
type CanMulX t1 t2 =
  (CanMul t1 t2,
   Show t1, QC.Arbitrary t1,
   Show t2, QC.Arbitrary t2,
   HasEq t1 (MulType t1 t2),
   HasEq t2 (MulType t1 t2),
   HasEq (MulType t1 t2) (MulType t1 t2),
   HasOrder t1 (MulType t1 t2),
   HasOrder t2 (MulType t1 t2),
   HasOrder (MulType t1 t2) (MulType t1 t2))

{-|
  HSpec properties that each implementation of CanMul should satisfy.
 -}
specCanMul ::
  (CanMulX t1 t2,
   CanMulX t1 t3,
   CanMulX t2 t3,
   CanMulX t1 (MulType t2 t3),
   CanMulX (MulType t1 t2) t3,
   HasEq (MulType t1 (MulType t2 t3)) (MulType (MulType t1 t2) t3),
   CanAdd t2 t3,
   CanMulX t1 (AddType t2 t3),
   CanAdd (MulType t1 t2) (MulType t1 t3),
   HasEq (MulType t1 (AddType t2 t3)) (AddType (MulType t1 t2) (MulType t1 t3)),
   Convertible Integer t2)
  =>
  T t1 -> T t2 -> T t3 -> Spec
specCanMul (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "CanMul %s %s, CanMul %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "absorbs 1" $ do
      QC.property $ \ (x :: t1) -> let one = (convert 1 :: t2) in (x * one) //== x
    it "is commutative" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x * y) //== (y * x)
    it "is associative" $ do
      QC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x * (y * z)) //== ((x * y) * z)
    it "distributes over addition" $ do
      QC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x * (y + z)) //== (x * y) + (x * z)

{-|
  HSpec properties that each implementation of CanMul should satisfy.
 -}
specCanMulNotMixed ::
  (CanMulX t t,
   CanMulX t (MulType t t),
   HasEq (MulType (MulType t t) t) (MulType t (MulType t t)),
   CanAdd t t,
   CanMulX t (AddType t t),
   CanAdd (MulType t t) (MulType t t),
   HasEq (MulType t (AddType t t)) (AddType (MulType t t) (MulType t t)),
   Convertible Integer t)
  =>
  T t -> Spec
specCanMulNotMixed t = specCanMul t t t

{-|
  HSpec properties that each implementation of CanMulSameType should satisfy.
 -}
specCanMulSameType ::
  (Convertible Integer t,
   HasEq t t, CanMulSameType t)
   =>
   T t -> Spec
specCanMulSameType (T typeName :: T t) =
  describe (printf "CanMulSameType %s" typeName) $ do
    it "has product working over integers" $ do
      QC.property $ \ (xsi :: [Integer]) ->
        (product $ (map convert xsi :: [t])) //== (convert (product xsi) :: t)
    it "has product [] = 1" $ do
        (product ([] :: [t])) //== (convert 1 :: t)

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
  mul = convertFirst mul
instance CanMulAsymmetric Double Int where
  type MulType Double Int = Double
  mul = convertSecond mul

instance CanMulAsymmetric Integer Double where
  type MulType Integer Double = Double
  mul = convertFirst mul
instance CanMulAsymmetric Double Integer where
  type MulType Double Integer = Double
  mul = convertSecond mul

instance CanMulAsymmetric Rational Double where
  type MulType Rational Double = Double
  mul = convertFirst mul
instance CanMulAsymmetric Double Rational where
  type MulType Double Rational = Double
  mul = convertSecond mul

instance (CanMulAsymmetric a b) => CanMulAsymmetric [a] [b] where
  type MulType [a] [b] = [MulType a b]
  mul (x:xs) (y:ys) = (mul x y) : (mul xs ys)
  mul _ _ = []

instance (CanMulAsymmetric a b) => CanMulAsymmetric (Maybe a) (Maybe b) where
  type MulType (Maybe a) (Maybe b) = Maybe (MulType a b)
  mul (Just x) (Just y) = Just (mul x y)
  mul _ _ = Nothing

{---- Exponentiation -----}

{-|
  A replacement for Prelude's binary `P.^` and `P.^^`.  If @Num t1@ and @Integral t2@,
  then one can use the default implementation to mirror Prelude's @^@.
-}
class CanPow t1 t2 where
  type PowType t1 t2
  type PowType t1 t2 = t1 -- default
  pow :: t1 -> t2 -> PowType t1 t2
  default pow :: (PowType t1 t2 ~ t1, P.Num t1, P.Integral t2) => t1 -> t2 -> t1
  pow = (P.^)

(^) :: (CanPow t1 t2) => t1 -> t2 -> PowType t1 t2
(^) = pow

{-| A synonym of `^` -}
(^^) :: (CanPow t1 t2) => t1 -> t2 -> PowType t1 t2
(^^) = (^)

type CanPowBy t1 t2 =
  (CanPow t1 t2, PowType t1 t2 ~ t1)

{-| Compound type constraint useful for test definition. -}
type CanPowX t1 t2 =
  (CanPow t1 t2,
   Show t1, QC.Arbitrary t1,
   Show t2, QC.Arbitrary t2)

{-|
  HSpec properties that each implementation of CanPow should satisfy.
 -}
specCanPow ::
  (CanPowX t1 t2,
   HasEq t1 (PowType t1 t2),
   Convertible Integer t1,
   Convertible Integer t2,
   CanTestPosNeg t2,
   CanAdd t2 Integer,
   CanMul t1 (PowType t1 t2),
   CanPow t1 (AddType t2 Integer),
   HasEq (MulType t1 (PowType t1 t2)) (PowType t1 (AddType t2 Integer)))
  =>
  T t1 -> T t2 -> Spec
specCanPow (T typeName1 :: T t1) (T typeName2 :: T t2) =
  describe (printf "CanPow %s %s" typeName1 typeName2) $ do
    it "x^0 = 1" $ do
      QC.property $ \ (x :: t1) ->
        let one = (convert 1 :: t1) in
        let z = (convert 0 :: t2) in
        (x ^ z) //== one
    it "x^1 = x" $ do
      QC.property $ \ (x :: t1) ->
        let one = (convert 1 :: t2) in
        (x ^ one) //== x
    it "x^(y+1) = x*x^y" $ do
      QC.property $ \ (x :: t1) (y :: t2) ->
        (isCertainlyNonNegative y) QC.==>
          x * (x ^ y) //== (x ^ (y + 1))

instance CanPow Integer Int
instance CanPow Integer Integer
instance CanPow Rational Int where pow = (P.^^)
instance CanPow Rational Integer where pow = (P.^^)
instance CanPow Double Int where pow = (P.^^)
instance CanPow Double Integer where pow = (P.^^)
{- No exponentiation of Int to avoid overflows. -}

-- instance (CanPow a b) => CanPow [a] [b] where
--   type PowType [a] [b] = [PowType a b]
--   pow (x:xs) (y:ys) = (pow x y) : (pow xs ys)
--   pow _ _ = []

instance (CanPow a b) => CanPow (Maybe a) (Maybe b) where
  type PowType (Maybe a) (Maybe b) = Maybe (PowType a b)
  pow (Just x) (Just y) = Just (pow x y)
  pow _ _ = Nothing

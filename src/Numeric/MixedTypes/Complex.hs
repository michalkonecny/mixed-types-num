{-|
    Module      :  Numeric.MixedType.Complex
    Description :  Instances for Data.Complex
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Instances for "Data.Complex".
-}

module Numeric.MixedTypes.Complex
(
)
where

import Numeric.MixedTypes.PreludeHiding
-- import qualified Prelude as P
-- import Text.Printf

import Data.Complex

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Ring
import Numeric.MixedTypes.Field
import Numeric.MixedTypes.Elementary

instance (ConvertibleExactly Integer t) => (ConvertibleExactly Integer (Complex t))
  where
  safeConvertExactly n =
    do
    nT <- safeConvertExactly n
    zT <- safeConvertExactly 0
    return $ nT :+ zT

instance (ConvertibleExactly Int t) => (ConvertibleExactly Int (Complex t))
  where
  safeConvertExactly n =
    do
    nT <- safeConvertExactly n
    zT <- safeConvertExactly (int 0)
    return $ nT :+ zT

instance (ConvertibleExactly Rational t) => (ConvertibleExactly Rational (Complex t))
  where
  safeConvertExactly r =
    do
    rT <- safeConvertExactly r
    zT <- safeConvertExactly 0.0
    return $ rT :+ zT

instance (HasEqAsymmetric a b) => HasEqAsymmetric (Complex a) (Complex b) where
  type EqCompareType (Complex a) (Complex b) = EqCompareType a b
  equalTo (a1 :+ i1) (a2 :+ i2) = (a1 == a2) && (i1 == i2)

instance (HasEqAsymmetric Integer b) => HasEqAsymmetric Integer (Complex b) where
  type EqCompareType Integer (Complex b) = EqCompareType Integer b
  equalTo n (a2 :+ i2) = (n == a2) && (0 == i2)

instance (HasEqAsymmetric a Integer) => HasEqAsymmetric (Complex a) Integer where
  type EqCompareType (Complex a) Integer = EqCompareType a Integer
  equalTo (a1 :+ i1) n = (a1 == n) && (i1 == 0)

instance (HasEqAsymmetric Rational b) => HasEqAsymmetric Rational (Complex b) where
  type EqCompareType Rational (Complex b) = EqCompareType Rational b
  equalTo n (a2 :+ i2) = (n == a2) && (0.0 == i2)

instance (HasEqAsymmetric a Rational) => HasEqAsymmetric (Complex a) Rational where
  type EqCompareType (Complex a) Rational = EqCompareType a Rational
  equalTo (a1 :+ i1) n = (a1 == n) && (i1 == 0.0)

instance (HasEqAsymmetric Int b) => HasEqAsymmetric Int (Complex b) where
  type EqCompareType Int (Complex b) = EqCompareType Int b
  equalTo n (a2 :+ i2) = (n == a2) && ((int 0) == i2)

instance (HasEqAsymmetric a Int) => HasEqAsymmetric (Complex a) Int where
  type EqCompareType (Complex a) Int = EqCompareType a Int
  equalTo (a1 :+ i1) n = (a1 == n) && (i1 == (int 0))

instance (HasEqAsymmetric Double b) => HasEqAsymmetric Double (Complex b) where
  type EqCompareType Double (Complex b) = EqCompareType Double b
  equalTo n (a2 :+ i2) = (n == a2) && ((double 0) == i2)

instance (HasEqAsymmetric a Double) => HasEqAsymmetric (Complex a) Double where
  type EqCompareType (Complex a) Double = EqCompareType a Double
  equalTo (a1 :+ i1) n = (a1 == n) && (i1 == (double 0))

instance (CanTestInteger t, CanTestZero t) => CanTestInteger (Complex t) where
  certainlyNotInteger (a :+ i) =
    certainlyNotInteger a || isNonZero i
  certainlyIntegerGetIt (a :+ i) =
    case (certainlyIntegerGetIt a, certainlyIntegerGetIt i) of
      (Just aN, Just iN) | iN == 0 -> Just aN
      _ -> Nothing

instance CanNeg t => CanNeg (Complex t) where
  type NegType (Complex t) = Complex (NegType t)
  negate (a :+ i) = (-a) :+ (-i)

instance (CanAddAsymmetric a b) => CanAddAsymmetric (Complex a) (Complex b) where
  type AddType (Complex a) (Complex b) = Complex (AddType a b)
  add (a1 :+ i1) (a2 :+ i2) = (a1 + a2) :+ (i1 + i2)

instance (CanAddAsymmetric Integer b) => CanAddAsymmetric Integer (Complex b) where
  type AddType Integer (Complex b) = Complex (AddType Integer b)
  add n (a2 :+ i2) = (n + a2) :+ (0 + i2)

instance (CanAddAsymmetric a Integer) => CanAddAsymmetric (Complex a) Integer where
  type AddType (Complex a) Integer = Complex (AddType a Integer)
  add (a1 :+ i1) n = (a1 + n) :+ (i1 + 0)

instance (CanAddAsymmetric Rational b) => CanAddAsymmetric Rational (Complex b) where
  type AddType Rational (Complex b) = Complex (AddType Rational b)
  add n (a2 :+ i2) = (n + a2) :+ (0.0 + i2)

instance (CanAddAsymmetric a Rational) => CanAddAsymmetric (Complex a) Rational where
  type AddType (Complex a) Rational = Complex (AddType a Rational)
  add (a1 :+ i1) n = (a1 + n) :+ (i1 + 0.0)

instance (CanAddAsymmetric Int b) => CanAddAsymmetric Int (Complex b) where
  type AddType Int (Complex b) = Complex (AddType Int b)
  add n (a2 :+ i2) = (n + a2) :+ ((int 0) + i2)

instance (CanAddAsymmetric a Int) => CanAddAsymmetric (Complex a) Int where
  type AddType (Complex a) Int = Complex (AddType a Int)
  add (a1 :+ i1) n = (a1 + n) :+ (i1 + (int 0))

instance (CanAddAsymmetric Double b) => CanAddAsymmetric Double (Complex b) where
  type AddType Double (Complex b) = Complex (AddType Double b)
  add n (a2 :+ i2) = (n + a2) :+ ((double 0) + i2)

instance (CanAddAsymmetric a Double) => CanAddAsymmetric (Complex a) Double where
  type AddType (Complex a) Double = Complex (AddType a Double)
  add (a1 :+ i1) n = (a1 + n) :+ (i1 + (double 0))

instance (CanSub a b) => CanSub (Complex a) (Complex b) where
  type SubType (Complex a) (Complex b) = Complex (SubType a b)
  sub (a1 :+ i1) (a2 :+ i2) = (a1 - a2) :+ (i1 - i2)

instance (CanSub Integer b) => CanSub Integer (Complex b) where
  type SubType Integer (Complex b) = Complex (SubType Integer b)
  sub n (a2 :+ i2) = (n - a2) :+ (0 - i2)

instance (CanSub a Integer) => CanSub (Complex a) Integer where
  type SubType (Complex a) Integer = Complex (SubType a Integer)
  sub (a1 :+ i1) n = (a1 - n) :+ (i1 - 0)

instance (CanSub Rational b) => CanSub Rational (Complex b) where
  type SubType Rational (Complex b) = Complex (SubType Rational b)
  sub n (a2 :+ i2) = (n - a2) :+ (0.0 - i2)

instance (CanSub a Rational) => CanSub (Complex a) Rational where
  type SubType (Complex a) Rational = Complex (SubType a Rational)
  sub (a1 :+ i1) n = (a1 - n) :+ (i1 - 0.0)

instance (CanSub Int b) => CanSub Int (Complex b) where
  type SubType Int (Complex b) = Complex (SubType Int b)
  sub n (a2 :+ i2) = (n - a2) :+ ((int 0) - i2)

instance (CanSub a Int) => CanSub (Complex a) Int where
  type SubType (Complex a) Int = Complex (SubType a Int)
  sub (a1 :+ i1) n = (a1 - n) :+ (i1 - (int 0))

instance (CanSub Double b) => CanSub Double (Complex b) where
  type SubType Double (Complex b) = Complex (SubType Double b)
  sub n (a2 :+ i2) = (n - a2) :+ ((double 0) - i2)

instance (CanSub a Double) => CanSub (Complex a) Double where
  type SubType (Complex a) Double = Complex (SubType a Double)
  sub (a1 :+ i1) n = (a1 - n) :+ (i1 - (double 0))

instance
  (CanMulAsymmetric a b
  , CanAddSameType (MulType a b), CanSubSameType (MulType a b))
  =>
  CanMulAsymmetric (Complex a) (Complex b)
  where
  type MulType (Complex a) (Complex b) = Complex (MulType a b)
  mul (a1 :+ i1) (a2 :+ i2) =
    (a1*a2 - i1*i2) :+ (a1*i2 + i1*a2)

instance
  (CanMulAsymmetric Integer b) => CanMulAsymmetric Integer (Complex b)
  where
  type MulType Integer (Complex b) = Complex (MulType Integer b)
  mul n (a2 :+ i2) = (n*a2) :+ (n*i2)

instance
  (CanMulAsymmetric a Integer) => CanMulAsymmetric (Complex a) Integer
  where
  type MulType (Complex a) Integer = Complex (MulType a Integer)
  mul (a1 :+ i1) n = (a1*n) :+ (i1*n)

instance
  (CanMulAsymmetric Int b) => CanMulAsymmetric Int (Complex b)
  where
  type MulType Int (Complex b) = Complex (MulType Int b)
  mul n (a2 :+ i2) = (n*a2) :+ (n*i2)

instance
  (CanMulAsymmetric a Int) => CanMulAsymmetric (Complex a) Int
  where
  type MulType (Complex a) Int = Complex (MulType a Int)
  mul (a1 :+ i1) n = (a1*n) :+ (i1*n)

instance
  (CanMulAsymmetric Rational b) => CanMulAsymmetric Rational (Complex b)
  where
  type MulType Rational (Complex b) = Complex (MulType Rational b)
  mul n (a2 :+ i2) = (n*a2) :+ (n*i2)

instance
  (CanMulAsymmetric a Rational) => CanMulAsymmetric (Complex a) Rational
  where
  type MulType (Complex a) Rational = Complex (MulType a Rational)
  mul (a1 :+ i1) n = (a1*n) :+ (i1*n)

instance
  (CanMulAsymmetric Double b) => CanMulAsymmetric Double (Complex b)
  where
  type MulType Double (Complex b) = Complex (MulType Double b)
  mul n (a2 :+ i2) = (n*a2) :+ (n*i2)

instance
  (CanMulAsymmetric a Double) => CanMulAsymmetric (Complex a) Double
  where
  type MulType (Complex a) Double = Complex (MulType a Double)
  mul (a1 :+ i1) n = (a1*n) :+ (i1*n)

instance
  (CanMulAsymmetric a b
  , CanAddSameType (MulType a b), CanSubSameType (MulType a b)
  , CanMulAsymmetric b b, CanAddSameType (MulType b b)
  , CanDiv (MulType a b) (MulType b b))
  =>
  CanDiv (Complex a) (Complex b)
  where
  type DivType (Complex a) (Complex b) = Complex (DivType (MulType a b) (MulType b b))
  divide (a1 :+ i1) (a2 :+ i2) =
    let d = a2*a2 + i2*i2 in
    ((a1*a2 + i1*i2)/d) :+ ((i1*a2-a1*i2)/d)

instance
  (CanMulAsymmetric Integer b
  , CanMulAsymmetric b b, CanAddSameType (MulType b b)
  , CanDiv (MulType Integer b) (MulType b b))
  =>
  CanDiv Integer (Complex b)
  where
  type DivType Integer (Complex b) = Complex (DivType (MulType Integer b) (MulType b b))
  divide n (a2 :+ i2) =
    let d = a2*a2 + i2*i2 in
    ((n*a2)/d) :+ (((-n)*i2)/d)

instance
  (CanMulAsymmetric Rational b
  , CanMulAsymmetric b b, CanAddSameType (MulType b b)
  , CanDiv (MulType Rational b) (MulType b b))
  =>
  CanDiv Rational (Complex b)
  where
  type DivType Rational (Complex b) = Complex (DivType (MulType Rational b) (MulType b b))
  divide n (a2 :+ i2) =
    let d = a2*a2 + i2*i2 in
    ((n*a2)/d) :+ (((-n)*i2)/d)

instance
  (CanMulAsymmetric Int b
  , CanMulAsymmetric b b, CanAddSameType (MulType b b)
  , CanDiv (MulType Int b) (MulType b b))
  =>
  CanDiv Int (Complex b)
  where
  type DivType Int (Complex b) = Complex (DivType (MulType Int b) (MulType b b))
  divide n (a2 :+ i2) =
    let d = a2*a2 + i2*i2 in
    ((n*a2)/d) :+ (((-n)*i2)/d)

instance
  (CanMulAsymmetric Double b
  , CanMulAsymmetric b b, CanAddSameType (MulType b b)
  , CanDiv (MulType Double b) (MulType b b))
  =>
  CanDiv Double (Complex b)
  where
  type DivType Double (Complex b) = Complex (DivType (MulType Double b) (MulType b b))
  divide n (a2 :+ i2) =
    let d = a2*a2 + i2*i2 in
    ((n*a2)/d) :+ (((-n)*i2)/d)

instance
  (CanDiv a Integer) => CanDiv (Complex a) Integer
  where
  type DivType (Complex a) Integer = Complex (DivType a Integer)
  divide (a1 :+ i1) n = (a1/n) :+ (i1/n)

instance
  (CanDiv a Int) => CanDiv (Complex a) Int
  where
  type DivType (Complex a) Int = Complex (DivType a Int)
  divide (a1 :+ i1) n = (a1/n) :+ (i1/n)

instance
  (CanDiv a Rational) => CanDiv (Complex a) Rational
  where
  type DivType (Complex a) Rational = Complex (DivType a Rational)
  divide (a1 :+ i1) n = (a1/n) :+ (i1/n)

instance
  (CanDiv a Double) => CanDiv (Complex a) Double
  where
  type DivType (Complex a) Double = Complex (DivType a Double)
  divide (a1 :+ i1) n = (a1/n) :+ (i1/n)

instance
  (CanMulAsymmetric t t
  , CanAddSameType (MulType t t)
  , CanSqrt (MulType t t))
  =>
  CanAbs (Complex t)
  where
  type AbsType (Complex t) = SqrtType (MulType t t)
  abs (a :+ i) = sqrt (a*a + i*i)

instance
  (CanExp t
  , CanSinCos t
  , CanMulAsymmetric (ExpType t) (SinCosType t))
  =>
  CanExp (Complex t)
  where
  type ExpType (Complex t) = Complex (MulType (ExpType t) (SinCosType t))
  exp (a :+ i) =
    let ea = exp a in
    (ea * cos i) :+ (ea * sin i)

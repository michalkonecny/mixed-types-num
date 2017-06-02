{-|
    Module      :  Numeric.MixedType.Elementary
    Description :  Bottom-up typed pi, sqrt, cos, etc
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Elementary
(
  -- * Square root
  CanSqrt(..), CanSqrtSameType, CanSqrtCNSameType, specCanSqrtReal
  -- * Exp
  , CanExp(..), CanExpSameType, specCanExpReal
  -- * Log
  , CanLog(..), CanLogSameType, CanLogCNSameType, specCanLogReal
  , powUsingExpLog
  -- * Sine and cosine
  , CanSinCos(..), CanSinCosSameType, specCanSinCosReal
  , approxPi
)
where

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import Numeric.CollectErrors
  (CollectErrors, EnsureCE, CanEnsureCE,
   WithoutCN, EnsureCN, CanEnsureCN, ensureCN)
import qualified Numeric.CollectErrors as CN

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxSqrt
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Ring
import Numeric.MixedTypes.Field

{----  sqrt -----}

{-|
  A replacement for Prelude's `P.sqrt`.  If @Floating t@,
  then one can use the default implementation to mirror Prelude's @sqrt@.
-}
class CanSqrt t where
  type SqrtType t
  type SqrtType t = t -- default
  sqrt :: t -> SqrtType t
  default sqrt :: (SqrtType t ~ t, P.Floating t) => t -> t
  sqrt = P.sqrt

type CanSqrtSameType t = (CanSqrt t, SqrtType t ~ t)
type CanSqrtCNSameType t = (CanSqrt t, SqrtType t ~ EnsureCN t)

type CanSqrtX t =
  (CanSqrt t,
   CanTestPosNeg t,
   HasEqCertainly t (SqrtType t),
   HasOrderCertainly Integer (SqrtType t),
   Show t, Arbitrary t, Show (SqrtType t))

{-|
  HSpec properties that each implementation of CanSqrt should satisfy.
 -}
specCanSqrtReal ::
  (CanSqrtX t,
   CanPowX (SqrtType t) Integer,
   HasEqCertainly t (PowType (SqrtType t) Integer))
  =>
  T t -> Spec
specCanSqrtReal (T typeName :: T t) =
  describe (printf "CanSqrt %s" typeName) $ do
    it "sqrt(x) >= 0" $ do
      property $ \ (x :: t) ->
        isCertainlyNonNegative x ==>
          (sqrt x) ?>=?$ 0
    it "sqrt(x)^2 = x" $ do
      property $ \ (x :: t) ->
        isCertainlyNonNegative x ==>
          (sqrt x)^2 ?==?$ x
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)
  infix 4 ?>=?$
  (?>=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?>=?$) = printArgsIfFails2 "?>=?" (?>=?)

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanSqrt Double -- not exact, will not pass the tests

instance
  (CanSqrt a
  , CanEnsureCE es (SqrtType a)
  , Monoid es)
  =>
  CanSqrt (CollectErrors es a)
  where
  type SqrtType (CollectErrors es a) = EnsureCE es (SqrtType a)
  sqrt = CN.lift1ensureCE sqrt


{----  exp -----}

{-|
  A replacement for Prelude's `P.exp`.  If @Floating t@,
  then one can use the default implementation to mirror Prelude's @exp@.
-}
class CanExp t where
  type ExpType t
  type ExpType t = t -- default
  exp :: t -> ExpType t
  default exp :: (ExpType t ~ t, P.Floating t) => t -> t
  exp = P.exp

type CanExpSameType t = (CanExp t, ExpType t ~ t)

type CanExpX t =
  (CanExp t,
   Ring t,
   Field (ExpType t),
   CanTestPosNeg t,
   CanTestPosNeg (ExpType t),
   HasEqCertainlyCN (ExpType t) (ExpType t),
   HasOrderCertainly Integer t,
   HasOrderCertainly Integer (ExpType t),
   Show t, Arbitrary t, Show (ExpType t),
   Show (WithoutCN t), Show (WithoutCN (ExpType t)))

{-|
  HSpec properties that each implementation of CanExp should satisfy.
 -}
specCanExpReal ::
  (CanExpX t)
  =>
  T t -> Spec
specCanExpReal (T typeName :: T t) =
  describe (printf "CanExp %s" typeName) $ do
    it "exp(x) >= 0" $ do
      property $ \ (x :: t) ->
        ((-100000) !<! x && x !<! 100000) ==>
          exp x ?>=?$ 0
    it "exp(-x) == 1/(exp x)" $ do
      property $ \ (x :: t) ->
        ((-100000) !<! x && x !<! 100000) ==>
          (exp x !>! 0) ==>
            (CN.ensureCN $ exp (-x)) ?==?$ 1/(exp x)
    it "exp(x+y) = exp(x)*exp(y)" $ do
      property $ \ (x :: t)  (y :: t) ->
        ((-100000) !<! x && x !<! 100000 && (-100000) !<! y && y !<! 100000) ==>
          (exp $ x + y) ?==?$ (exp x) * (exp y)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)
  infix 4 ?>=?$
  (?>=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?>=?$) = printArgsIfFails2 "?>=?" (?>=?)

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanExp Double -- not exact, will not pass the tests

instance
  (CanExp a
  , CanEnsureCE es (ExpType a)
  , Monoid es)
  =>
  CanExp (CollectErrors es a)
  where
  type ExpType (CollectErrors es a) = EnsureCE es (ExpType a)
  exp = CN.lift1ensureCE exp

{----  log -----}

{-|
  A replacement for Prelude's `P.log`.  If @Floating t@,
  then one can use the default implementation to mirror Prelude's @log@.
-}
class CanLog t where
  type LogType t
  type LogType t = t -- default
  log :: t -> LogType t
  default log :: (LogType t ~ t, P.Floating t) => t -> t
  log = P.log

type CanLogSameType t = (CanLog t, LogType t ~ t)
type CanLogCNSameType t = (CanLog t, LogType t ~ EnsureCN t)

type CanLogX t =
  (CanLog t,
   Field t,
   Ring (LogType t),
   HasOrderCertainly t Integer,
   HasOrderCertainlyCN t Integer,
   HasEqCertainly (LogType t) (LogType t),
   Show t, Arbitrary t, Show (LogType t))

{-|
  HSpec properties that each implementation of CanLog should satisfy.
 -}
specCanLogReal ::
  (CanLogX t,
   CanLogX (DivType Integer t),
   CanExp t, CanLogX (ExpType t),
   HasEqCertainly (LogType t) (EnsureCN (LogType (WithoutCN t))),
   HasEqCertainlyCN t (LogType (ExpType t)))
  =>
  T t -> Spec
specCanLogReal (T typeName :: T t) =
  describe (printf "CanLog %s" typeName) $ do
    it "log(1/x) == -(log x)" $ do
      property $ \ (x :: t) ->
        x !>! 0 && (1/x) !>! 0  ==>
          log (1/x) ?==?$ -(log x)
    it "log(x*y) = log(x)+log(y)" $ do
      property $ \ (x :: t)  (y :: t) ->
        x !>! 0 && y !>! 0 && x*y !>! 0  ==>
          (log $ x * y) ?==?$ (log x) + (log y)
    it "log(exp x) == x" $ do
      property $ \ (x :: t) ->
        ((-100000) !<! x && x !<! 100000) ==>
          log (exp x) ?==?$ x
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanLog Double -- not exact, will not pass the tests

instance
  (CanLog a
  , CanEnsureCE es (LogType a)
  , Monoid es)
  =>
  CanLog (CollectErrors es a)
  where
  type LogType (CollectErrors es a) = EnsureCE es (LogType a)
  log = CN.lift1ensureCE log

instance CanPow Double Double where
  pow = (P.**)
  -- pow = powUsingExpLog
instance CanPow Double Rational where
  type PowType Double Rational = Double
  pow x y = x ^ (double y)
instance CanPow Rational Double where
  type PowType Rational Double = Double
  pow x y = (double x) ^ y
instance CanPow Integer Double where
  type PowType Integer Double = Double
  pow x y = (double x) ^ y
instance CanPow Int Double where
  type PowType Int Double = Double
  pow x y = (double x) ^ y

powUsingExpLog ::
  (CanTestPosNeg t,
   CanEnsureCN t,
   CanEnsureCN (WithoutCN t),
   WithoutCN t ~ WithoutCN (WithoutCN t),
   CanLogCNSameType t,
   CanMulSameType t,
   CanMulSameType (WithoutCN t),
   CanExpSameType (WithoutCN t),
   CanTestInteger t,
   HasIntegers t,
   CanTestZero t,
   CanRecipCNSameType t,
   HasIntegers (WithoutCN t))
  =>
  t -> t -> EnsureCN t
powUsingExpLog x y =
  case certainlyIntegerGetIt y of
    Just n ->
      powUsingMulRecip x n
    Nothing
      | isCertainlyZero x && isCertainlyPositive y -> convertExactly 0
      | isCertainlyPositive x -> exp ((log x) * (ensureCN y))
      | otherwise ->
          CN.noValueNumErrorPotential $
            CN.NumError "powUsingExpLog: illegal power a^b with negative a and non-integer b"

{----  sine and cosine -----}

{-|
  A replacement for Prelude's `P.cos` and `P.sin`.  If @Floating t@,
  then one can use the default implementation to mirror Prelude's @sin@, @cos@.
-}
class CanSinCos t where
  type SinCosType t
  type SinCosType t = t -- default
  cos :: t -> SinCosType t
  default cos :: (SinCosType t ~ t, P.Floating t) => t -> t
  cos = P.cos
  sin :: t -> SinCosType t
  default sin :: (SinCosType t ~ t, P.Floating t) => t -> t
  sin = P.sin

type CanSinCosSameType t = (CanSinCos t, SinCosType t ~ t)

type CanSinCosX t =
  (CanSinCos t,
   OrderedCertainlyField t,
   OrderedCertainlyField (SinCosType t),
   HasOrderCertainlyCN (SinCosType t) t,
   Show t, Arbitrary t, Show (SinCosType t),
   Show (WithoutCN t), Arbitrary t, Show (WithoutCN (SinCosType t)))

{-|
  HSpec properties that each implementation of CanSinCos should satisfy.

  Derived partially from
  http://math.stackexchange.com/questions/1303044/axiomatic-definition-of-sin-and-cos
 -}
specCanSinCosReal ::
  (CanSinCosX t)
  =>
  T t -> Spec
specCanSinCosReal (T typeName :: T t) =
  describe (printf "CanSinCos %s" typeName) $ do
    it "-1 <= sin(x) <= 1" $ do
      property $ \ (x :: t) ->
          (-1) ?<=?$ (sin x) .&&. (sin x) ?<=?$ 1
    it "-1 <= cos(x) <= 1" $ do
      property $ \ (x :: t) ->
          (-1) ?<=?$ (cos x) .&&. (cos x) ?<=?$ 1
    it "cos(x)^2 + sin(x)^2 = 1" $ do
      property $ \ (x :: t) ->
          (sin x)^2 + (cos x)^2 ?==?$ 1
    it "sin(x-y) = sin(x)cos(y) - cos(x)sin(y)" $ do
      property $ \ (x :: t) (y :: t) ->
          (sin $ x - y) ?==?$ (sin x)*(cos y) - (cos x)*(sin y)
    it "cos(x-y) = cos(x)cos(y) + sin(x)sin(y)" $ do
      property $ \ (x :: t) (y :: t) ->
          (cos $ x - y) ?==?$ (cos x)*(cos y) + (sin x)*(sin y)
    it "sin(x) < x < tan(x) for x in [0,pi/2]" $ do
      property $ \ (x :: t) ->
        x !>=! 0 && x !<=! 1.57 && (cos x) !>! 0 ==>
          (sin x) ?<=?$ x .&&. (CN.ensureCN x) ?<=?$ (sin x)/(cos x)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)
  infix 4 ?<=?$
  (?<=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<=?$) = printArgsIfFails2 "?<=?" (?<=?)

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanSinCos Double -- not exact, will not pass the tests

instance
  (CanSinCos a
  , CanEnsureCE es (SinCosType a)
  , Monoid es)
  =>
  CanSinCos (CollectErrors es a)
  where
  type SinCosType (CollectErrors es a) = EnsureCE es (SinCosType a)
  sin = CN.lift1ensureCE sin
  cos = CN.lift1ensureCE cos

{-|
  Approximate pi, synonym for Prelude's `P.pi`.

  We do not define (exect) @pi@ in this package as we have no type
  that can represent it exactly.
-}
approxPi :: (P.Floating t) => t
approxPi = P.pi

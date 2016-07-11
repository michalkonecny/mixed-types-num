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
  CanSqrt(..), CanSqrtSameType, specCanSqrtReal,
  -- * Exp
  CanExp(..), CanExpSameType, specCanExpReal,
  -- * Log
  CanLog(..), CanLogSameType, specCanLogReal,
  powViaExpLog
)
where

import Prelude hiding
  (fromInteger, fromRational,
   negate,not,(&&),(||),and,or,
   (==), (/=), (>), (<), (<=), (>=),
   abs, min, max, minimum, maximum,
   (-), (+), sum,
   (*), (^), (^^), (**), product,
   (/), recip,
   properFraction, round, truncate, ceiling, floor,
   sqrt, exp, log
  )
import qualified Prelude as P
import Text.Printf

-- import qualified Data.List as List

import Test.Hspec
import qualified Test.QuickCheck as QC

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd
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

type CanSqrtX t =
  (CanSqrt t,
   CanTestPosNeg t,
   CanTestPosNeg (SqrtType t),
   HasEq t (SqrtType t),
   Show t, QC.Arbitrary t)

{-|
  HSpec properties that each implementation of CanSqrt should satisfy.
 -}
specCanSqrtReal ::
  (CanSqrtX t,
   CanPow (SqrtType t) Integer,
   HasEq t (PowType (SqrtType t) Integer))
  =>
  T t -> Spec
specCanSqrtReal (T typeName :: T t) =
  describe (printf "CanSqrt %s" typeName) $ do
    it "sqrt(x) >= 0" $ do
      QC.property $ \ (x :: t) ->
        isCertainlyNonNegative x QC.==>
          isCertainlyNonNegative (sqrt x)
    it "sqrt(x)^2 = x" $ do
      QC.property $ \ (x :: t) ->
        isCertainlyNonNegative x QC.==>
          (sqrt x)^2 ?==? x

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanSqrt Double -- not exact, will not pass the tests

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
   HasEq (ExpType t) (ExpType t),
   Show t, QC.Arbitrary t)

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
      QC.property $ \ (x :: t) ->
        not $ isCertainlyNegative (exp x)
    it "exp(-x) == 1/(exp x)" $ do
      QC.property $ \ (x :: t) ->
        exp (-x) ?==? 1/(exp x)
    it "exp(x+y) = exp(x)*exp(y)" $ do
      QC.property $ \ (x :: t)  (y :: t) ->
        (exp $ x + y) ?==? (exp x) * (exp y)

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanExp Double -- not exact, will not pass the tests

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

type CanLogX t =
  (CanLog t,
   Field t,
   Ring (LogType t),
   CanTestPosNeg t,
   HasEq (LogType t) (LogType t),
   Show t, QC.Arbitrary t)

{-|
  HSpec properties that each implementation of CanLog should satisfy.
 -}
specCanLogReal ::
  (CanLogX t,
   CanExp t, CanLog (ExpType t),
   HasEq t (LogType (ExpType t)))
  =>
  T t -> Spec
specCanLogReal (T typeName :: T t) =
  describe (printf "CanLog %s" typeName) $ do
    it "log(1/x) == -(log x)" $ do
      QC.property $ \ (x :: t) ->
        isCertainlyPositive x QC.==>
          log (1/x) ?==? -(log x)
    it "log(x*y) = log(x)+log(y)" $ do
      QC.property $ \ (x :: t)  (y :: t) ->
        isCertainlyPositive x && isCertainlyPositive y  QC.==>
          (log $ x * y) ?==? (log x) + (log y)
    it "log(exp x) == x" $ do
      QC.property $ \ (x :: t) ->
          log (exp x) ?==? x

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanLog Double -- not exact, will not pass the tests

instance CanPow Double Double where
  pow = powViaExpLog
instance CanPow Double Rational where
  pow x y = powViaExpLog x (double y)

powViaExpLog ::
  (CanMulAsymmetric (LogType t1) t2,
   CanLog t1,
   CanExp (MulType (LogType t1) t2))
  =>
  t1 -> t2 -> ExpType (MulType (LogType t1) t2)
powViaExpLog x y = exp (log x * y)

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
  CanSqrt(..), CanSqrtSameType, specCanSqrt
)
where

import Prelude hiding
  (fromInteger,
   negate,not,(&&),(||),and,or,
   (==), (/=), (>), (<), (<=), (>=),
   abs, min, max, minimum, maximum,
   (-), (+), sum,
   (*), (^), (^^), product,
   (/), recip,
   properFraction, round, truncate, ceiling, floor,
   sqrt)
import qualified Prelude as P
import Text.Printf

-- import qualified Data.List as List

import Test.Hspec
import qualified Test.QuickCheck as QC

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd
-- import Numeric.MixedTypes.MinMaxSqrt
-- import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Ring
-- import Numeric.MixedTypes.Field

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

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanSqrt Double

type CanSqrtX t =
  (CanSqrt t,
   CanNegSameType t,
   CanTestPosNeg t,
   CanTestPosNeg (SqrtType t),
   HasEq t (SqrtType t),
   Show t, QC.Arbitrary t)

{-|
  HSpec properties that each implementation of CanSqrt should satisfy.
 -}
specCanSqrt ::
  (CanSqrtX t,
   CanPow (SqrtType t) Integer,
   HasEq t (PowType (SqrtType t) Integer))
  =>
  T t -> Spec
specCanSqrt (T typeName :: T t) =
  describe (printf "CanSqrt %s" typeName) $ do
    it "sqrt(x)^2 = x" $ do
      QC.property $ \ (x :: t) -> (sqrt x)^2 ?==? x

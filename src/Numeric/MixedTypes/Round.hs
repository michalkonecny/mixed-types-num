{-|
    Module      :  Numeric.MixedType.Round
    Description :  Bottom-up typed round, floor, etc.
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Round
(
  -- * Rounding operations
  CanRound(..)
  -- ** Tests
  , specCanRound
)
where

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

-- import qualified Data.List as List

import Test.Hspec
import qualified Test.QuickCheck as QC

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd
-- import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub

{----  rounding -----}

{-|
  A replacement for Prelude's `P.RealFrac` operations, such as round in
  which the result type is fixed to Integer.

  If @RealFrac t@ and @CanTestPosNeg t@,
  then one can use the default implementation to mirror Prelude's @round@, etc.

  In other cases, it is sufficient to define `properFraction`.
-}
class CanRound t where
  properFraction :: t -> (Integer, t)
  default properFraction :: (P.RealFrac t) => t -> (Integer, t)
  properFraction = P.properFraction
  truncate :: t -> Integer
  truncate = fst . properFraction
  round :: t -> Integer
  default round :: (HasOrder t Rational) => t -> Integer
  round x
    | -0.5 !<! r && r !<! 0.5 = n
    | r !<! -0.5 = n - 1
    | r !>! 0.5 = n + 1
    | even n = n
    | r !<! 0.0 = n - 1
    | r !>! 0.0 = n + 1
    | otherwise = error "round default defn: Bad value"
    where
    (n,r) = properFraction x
  ceiling :: t -> Integer
  default ceiling :: (CanTestPosNeg t) => t -> Integer
  ceiling x
    | isCertainlyPositive r = n + 1
    | otherwise = n
    where
    (n,r) = properFraction x
  floor :: t -> Integer
  default floor :: (CanTestPosNeg t) => t -> Integer
  floor x
    | isCertainlyNegative r = n - 1
    | otherwise = n
    where
    (n,r) = properFraction x

instance CanRound Rational
instance CanRound Double where
  round = P.round
  ceiling = P.ceiling
  floor = P.floor

type CanRoundX t =
  (CanRound t,
   CanNegSameType t,
   CanTestPosNeg t,
   HasOrder t Integer,
   CanTestFinite t,
   Show t, QC.Arbitrary t)

{-|
  HSpec properties that each implementation of CanRound should satisfy.
 -}
specCanRound ::
  (CanRoundX t, HasIntegers t)
  =>
  T t -> Spec
specCanRound (T typeName :: T t) =
  describe (printf "CanRound %s" typeName) $ do
    it "holds floor x <= x <= ceiling x" $ do
      QC.property $ \ (x :: t) ->
        isFinite x QC.==>
          (floor x ?<=? x) && (x ?<=? ceiling x)
    it "holds floor x <= round x <= ceiling x" $ do
      QC.property $ \ (x :: t) ->
        isFinite x QC.==>
          (floor x <= round x) && (round x <= ceiling x)
    it "0 <= ceiling x - floor x <= 1" $ do
      QC.property $ \ (x :: t) ->
        isFinite x QC.==>
          (ceiling x - floor x) `elem` [0,1]
    it "holds floor x = round x = ceiling x for integers" $ do
      QC.property $ \ (xi :: Integer) ->
        let x = convertExactly xi :: t in
          (floor x == round x) && (round x == ceiling x)

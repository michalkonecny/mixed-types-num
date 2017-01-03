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
  CanRound(..), HasIntegerBounds(..)
  -- ** Tests
  , specCanRound, specHasIntegerBounds
)
where

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck as QC

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
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
  default round :: (HasOrderCertainly t Rational) => t -> Integer
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
   HasOrderCertainly t Integer,
   CanTestFinite t,
   Show t, Arbitrary t)

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
      property $ \ (x :: t) ->
        isFinite x ==>
          (floor x ?<=?$ x) .&&. (x ?<=?$ ceiling x)
    it "holds floor x <= round x <= ceiling x" $ do
      property $ \ (x :: t) ->
        isFinite x ==>
          (floor x !<=!$ round x) .&&. (round x !<=!$ ceiling x)
    it "0 <= ceiling x - floor x <= 1" $ do
      property $ \ (x :: t) ->
        isFinite x ==>
          (ceiling x - floor x) `elem_PF` [0,1]
    it "holds floor x = round x = ceiling x for integers" $ do
      property $ \ (xi :: Integer) ->
        let x = convertExactly xi :: t in
          (floor x !==!$ round x) .&&. (round x !==!$ ceiling x)
  where
  (?<=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<=?$) = printArgsIfFails2 "?<=?" (?<=?)
  (!<=!$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (!<=!$) = printArgsIfFails2 "!<=!" (!<=!)
  (!==!$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (!==!$) = printArgsIfFails2 "!==!" (!==!)
  elem_PF = printArgsIfFails2 "elem" elem


class HasIntegerBounds t where
  integerBounds :: t -> (Integer, Integer)
  default integerBounds :: (CanRound t) => t -> (Integer, Integer)
  integerBounds x = (floor x, ceiling x)

instance HasIntegerBounds Rational
instance HasIntegerBounds Double

type HasIntegerBoundsX t =
  (HasIntegerBounds t,
  --  CanNegSameType t,
  --  CanTestPosNeg t,
   HasOrderCertainly t Integer,
   CanTestFinite t,
   Show t, Arbitrary t)


{-|
  HSpec properties that each implementation of CanRound should satisfy.
 -}
specHasIntegerBounds ::
  (HasIntegerBoundsX t)
  =>
  T t -> Spec
specHasIntegerBounds (T typeName :: T t) =
  describe (printf "HasIntegerBounds %s" typeName) $ do
    it "holds l <= x <= r" $ do
      property $ \ (x :: t) ->
        isFinite x ==>
          let (l,r) = integerBounds x in
          (l ?<=?$ x) .&&. (x ?<=?$ r)
  where
  (?<=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<=?$) = printArgsIfFails2 "?<=?" (?<=?)

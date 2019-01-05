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
  -- * Rounded division + modulus
  CanDivIMod(..), CanDivIModIntegerSameType, modNoCN, divINoCN, divIModNoCN
  -- * Rounding
  , CanRound(..), HasIntegerBounds(..)
  -- ** Tests
  , specCanDivIMod, specCanRound, specHasIntegerBounds
)
where

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf
import Data.Fixed (divMod')

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck as QC

import Numeric.CollectErrors
-- import Control.CollectErrors

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Ring

{----  rounded division + modulo -----}

class CanDivIMod t1 t2 where
  type DivIType t1 t2
  type DivIType t1 t2 = CN Integer
  type ModType t1 t2
  type ModType t1 t2 = EnsureCN t1
  divIMod :: t1 -> t2 -> (DivIType t1 t2, ModType t1 t2)
  mod :: t1 -> t2 -> ModType t1 t2
  mod a b = snd $ divIMod a b
  divI :: t1 -> t2 -> DivIType t1 t2
  divI a b = fst $ divIMod a b

type CanDivIModIntegerSameType t =
  (CanDivIMod t t, CanEnsureCN t, DivIType t t ~ CN Integer, ModType t t ~ EnsureCN t)

modNoCN :: 
  (CanDivIMod t1 t2
  , ModType t1 t2 ~ EnsureCN t1, CanEnsureCN t1)
  => 
  t1 -> t2 -> t1
modNoCN x m = 
  case deEnsureCN $ x `mod` m of
    Left err -> error $ show err
    Right xm -> xm

divINoCN :: 
  (CanDivIMod t1 t2, DivIType t1 t2 ~ CN Integer)
  => 
  t1 -> t2 -> Integer
divINoCN x m = (~!) $ x `divI` m

divIModNoCN :: 
  (CanDivIMod t1 t2
  , ModType t1 t2 ~ EnsureCN t1, CanEnsureCN t1
  , DivIType t1 t2 ~ CN Integer)
  => 
  t1 -> t2 -> (Integer, t1)
divIModNoCN x m = 
  case deEnsureCN xm of
    Left err -> error $ show err
    Right xm2 -> ((~!) d, xm2)
  where
  (d,xm) = divIMod x m

instance CanDivIMod Integer Integer where
  divIMod x m 
    | m > 0 = (cn d, cn xm)
    | otherwise = (err, err)
    where
    (d,xm) = P.divMod x m
    err = noValueNumErrorCertainECN sample_v $ OutOfRange $ "modulus not positive: " ++ show m
    sample_v = Just x

instance CanDivIMod Rational Rational where
  divIMod x m 
    | m > 0 = (cn d, cn xm)
    | otherwise = (err (d :: Integer), err xm)
    where
    (d,xm) = divMod' x m
    err :: (CanEnsureCN t) => t -> EnsureCN t
    err s = noValueNumErrorCertainECN (Just s) $ OutOfRange $ "modulus not positive: " ++ show m

instance CanDivIMod Rational Integer where
  divIMod x m = divIMod x (rational m)

instance CanDivIMod Double Double where
  divIMod x m 
    | m > 0 = (cn d, cn xm)
    | otherwise = (err (d :: Integer), err xm)
    where
    (d,xm) = divMod' x m
    err :: (CanEnsureCN t) => t -> EnsureCN t
    err s = noValueNumErrorCertainECN (Just s) $ OutOfRange $ "modulus not positive: " ++ show m

instance CanDivIMod Double Integer where
  divIMod x m = divIMod x (double m)

type CanDivIModX t =
  (CanDivIMod t t,
   ModType t t ~ EnsureCN t,
   DivIType t t ~ CN Integer,
   EnsureNoCN t ~ t,
   CanEnsureCN t,
   CanMulBy t Integer,
   CanAddSameType t,
   HasOrderCertainly t Integer,
   HasOrderCertainly t t,
   HasEqCertainly t t,
   CanTestFinite t,
   Show t, Arbitrary t)

{-|
  HSpec properties that each implementation of CanRound should satisfy.
 -}
specCanDivIMod ::
  (CanDivIModX t, HasIntegers t)
  =>
  T t -> Spec
specCanDivIMod (T typeName :: T t) =
  describe (printf "CanDivMod %s %s" typeName typeName) $ do
    it "holds 0 <= x `mod` m < m" $ do
      property $ \ (x :: t)  (m :: t) ->
        isFinite x && m !>! 0 ==>
          let xm = x `modNoCN` m in
          (0 ?<=?$ xm) .&&. (xm ?<?$ m)
    it "holds x == (x `div'` m)*m + (x `mod` m)" $ do
      property $ \ (x :: t)  (m :: t) ->
        isFinite x && m !>! 0 ==>
          let (d,xm) = divIModNoCN x m in
          (x ?==?$ (d*m + xm))
  where
  (?<=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<=?$) = printArgsIfFails2 "?<=?" (?<=?)
  (?<?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<?$) = printArgsIfFails2 "?<?" (?<?)
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

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
instance HasIntegerBounds Integer where
  integerBounds n = (n,n)
instance HasIntegerBounds Int where
  integerBounds n = (n',n') where n' = integer n

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

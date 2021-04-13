{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  Numeric.MixedType.Kleenean
    Description :  Three-valued logic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}
module Numeric.MixedTypes.Kleenean
(
    Kleenean(..), kleenean
)
where

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P

import Numeric.MixedTypes.Literals
    ( ConvertibleExactly(..), convertExactly )
import Numeric.MixedTypes.Bool
    ( (&&),
      not,
      CanAndOrAsymmetric(..),
      CanNeg(negate),
      CanTestCertainly(..),
      and )

data Kleenean = CertainTrue | CertainFalse | TrueOrFalse
  deriving (P.Eq, Show)

type CanBeKleenean t = ConvertibleExactly t Kleenean
kleenean :: (CanBeKleenean t) => t -> Kleenean
kleenean = convertExactly

instance ConvertibleExactly Bool Kleenean where
  safeConvertExactly True = Right CertainTrue
  safeConvertExactly False = Right CertainFalse

instance CanTestCertainly Kleenean where
  isCertainlyTrue = (P.== CertainTrue)
  isCertainlyFalse = (P.== CertainFalse)

instance CanNeg Kleenean where
  negate CertainTrue = CertainFalse
  negate CertainFalse = CertainTrue
  negate TrueOrFalse = TrueOrFalse

_testNeg1 :: Kleenean
_testNeg1 = not CertainTrue

instance CanAndOrAsymmetric Kleenean Kleenean
  where
  type AndOrType Kleenean Kleenean = Kleenean
  and2 CertainTrue CertainTrue = CertainTrue
  and2 CertainFalse _ = CertainFalse
  and2 _ CertainFalse = CertainFalse
  and2 _ _ = TrueOrFalse
  or2 CertainFalse CertainFalse = CertainFalse
  or2 CertainTrue _ = CertainTrue
  or2 _ CertainTrue = CertainTrue
  or2 _ _ = TrueOrFalse

instance CanAndOrAsymmetric Bool Kleenean
  where
  type AndOrType Bool Kleenean = Kleenean
  and2 b = and2 (kleenean b)
  or2 b = or2 (kleenean b)

instance CanAndOrAsymmetric Kleenean Bool
  where
  type AndOrType Kleenean Bool = Kleenean
  and2 k b = and2 k (kleenean b)
  or2 k b = or2 k (kleenean b)

_testAndOr1 :: Kleenean
_testAndOr1 = TrueOrFalse && False

_testAndOr2 :: Kleenean
_testAndOr2 = and [CertainTrue, TrueOrFalse, CertainFalse]


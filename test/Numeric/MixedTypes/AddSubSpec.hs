{-|
    Module      :  Numeric.MixedType.AddSubSpec
    Description :  hspec tests for addition and subtraction
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.AddSubSpec (spec) where

import Numeric.MixedTypes

import Test.Hspec

spec :: Spec
spec = do
  specCanAddNotMixed tInt
  specCanAddNotMixed tInteger
  specCanAddNotMixed tRational
  specCanAdd tInt tInteger tRational
  specCanAdd tInteger tRational tInt
  specCanAddSameType tInteger
  specCanAddSameType tRational
  specCanSubNotMixed tInt
  specCanSubNotMixed tInteger
  specCanSubNotMixed tRational
  specCanSub tInt tInteger
  specCanSub tInt tRational
  specCanSub tInteger tRational

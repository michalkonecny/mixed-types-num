{-|
    Module      :  Numeric.MixedType.MinMaxAbsSpec
    Description :  hspec tests for Boolean operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.MinMaxAbsSpec (spec) where

import Numeric.MixedTypes

import Test.Hspec

spec :: Spec
spec = do
  specCanMinMaxNotMixed tInt
  specCanMinMaxNotMixed tInteger
  specCanMinMaxNotMixed tRational
  specCanMinMax tInt tInteger tRational
  specCanMinMax tInteger tRational tInt

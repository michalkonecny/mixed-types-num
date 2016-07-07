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
  specCanMinMaxNotMixed "Int" (int 0)
  specCanMinMaxNotMixed "Integer" 0
  specCanMinMaxNotMixed "Rational" 0.0
  specCanMinMax "Int" (int 0) "Integer" 0 "Rational" 0.0
  specCanMinMax "Integer" 0 "Rational" 0.0  "Int" (int 0)

{-|
    Module      :  Numeric.MixedType.EqOrdSpec
    Description :  hspec tests for comparison operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.EqOrdSpec (spec) where

import Numeric.MixedTypes

import Test.Hspec

spec :: Spec
spec = do
  specHasEqNotMixed "Int" (int 0)
  specHasEqNotMixed "Integer" 0
  specHasEqNotMixed "Rational" 0.0
  specHasEqNotMixed "Double" (double 0)
  specHasEq "Int" (int 0) "Integer" 0 "Rational" 0.0
  specHasEq "Integer" 0 "Rational" 0.0  "Int" (int 0)
  specHasOrderNotMixed "Int" (int 0)
  specHasOrderNotMixed "Integer" 0
  specHasOrderNotMixed "Rational" 0.0
  specHasOrderNotMixed "Double" (double 0)
  specHasOrder "Int" (int 0) "Integer" 0 "Rational" 0.0
  specHasOrder "Integer" 0 "Rational" 0.0  "Int" (int 0)

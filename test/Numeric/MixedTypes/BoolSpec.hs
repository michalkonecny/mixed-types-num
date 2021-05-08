{-|
    Module      :  Numeric.MixedType.BoolSpec
    Description :  hspec tests for Boolean operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.BoolSpec (spec) where

import MixedTypesNumPrelude

import Test.Hspec

spec :: Spec
spec = do
  specIsBool tBool
  specIsBool tKleenean
  -- mixed-type tests:
  specCanAndOr tBool tKleenean tBool
  -- describe "mixed-type Boolean operation examples" $ do
  --   it "can do True || Nothing" $ do
  --     True || (Nothing :: Maybe Bool) `shouldBe` Just True
  --   it "can do Nothing || True" $ do
  --     (Nothing :: Maybe Bool) || True `shouldBe` Just True
  --   it "can do True || Just Nothing" $ do
  --     True || Just (Nothing :: Maybe Bool) `shouldBe` Just (Just True)
  --   it "can do Just Nothing || True" $ do
  --     Just (Nothing :: Maybe Bool) || True `shouldBe` Just (Just True)

{-|
    Module      :  Numeric.MixedType.BoolSpec
    Description :  hspec tests for generic Boolean operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.BoolSpec (spec) where

import Numeric.MixedTypes
-- import qualified Prelude as P

-- import Text.Printf
-- import Control.Exception (evaluate)

import Test.Hspec
-- import qualified Test.QuickCheck as QC
-- import qualified Test.Hspec.SmallCheck as SC

spec :: Spec
spec = do
  specIsBool "Bool" True
  specIsBool "Maybe Bool" (Just True)
  specIsBool "Maybe (Maybe Bool)" (Just (Just True))
  -- mixed-type tests:
  specCanAndOr "Bool" True "Maybe Bool" (Just True) "Bool" True
  specCanAndOr "Maybe (Maybe Bool)" (Just (Just True)) "Maybe Bool" (Just True) "Bool" True
  describe "mixed-type Boolean operation examples" $ do
    it "can do True || Just False" $ do
      True || Just False `shouldBe` Just True
    it "can do True || Nothing" $ do
      True || (Nothing :: Maybe Bool) `shouldBe` Just True
    it "can do Nothing || True" $ do
      (Nothing :: Maybe Bool) || True `shouldBe` Just True
    it "can do True || Just (Just False)" $ do
      True || Just (Just False) `shouldBe` Just (Just True)
    it "can do True || Just Nothing" $ do
      True || Just (Nothing :: Maybe Bool) `shouldBe` Just (Just True)
    it "can do Just Nothing || True" $ do
      Just (Nothing :: Maybe Bool) || True `shouldBe` Just (Just True)

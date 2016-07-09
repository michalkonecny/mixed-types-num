{-|
    Module      :  Numeric.MixedType.LiteralsSpec
    Description :  hspec tests for Literals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.LiteralsSpec (spec) where

import Numeric.MixedTypes
import qualified Prelude as P

-- import Text.Printf
import Control.Exception (evaluate)

import Test.Hspec
import qualified Test.QuickCheck as QC
-- import qualified Test.Hspec.SmallCheck as SC

spec :: Spec
spec = do
  specCanBeInteger tInt
  specCanBeInteger tInteger
  specConversions

specConversions :: Spec
specConversions =
  do
  describe "numeric conversions" $ do
    it "convert int to integer and back" $ do
      QC.property $ \ (x :: Int) -> (int $ integer x) P.== x
    it "throws exception when converting large integer to int" $ do
      (evaluate $ int (integer (maxBound :: Int) P.+ 1)) `shouldThrow` anyException
    it "convert int to rational and back" $ do
      QC.property $ \ (x :: Int) -> (int $ round $ rational x) P.== x
    it "convert integer to rational and back" $ do
      QC.property $ \ (x :: Integer) -> (round $ rational x) P.== x
    it "convert double to rational and back" $ do
      QC.property $ \ (x :: Double) -> (double $ toRational x) P.== x

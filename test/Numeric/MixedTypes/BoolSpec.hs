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
import qualified Prelude as P

import Text.Printf
-- import Control.Exception (evaluate)

import Test.Hspec
-- import qualified Test.QuickCheck as QC
import qualified Test.Hspec.SmallCheck as SC

spec :: Spec
spec = do
  specBoolOps "Bool" True
  specBoolOps "Maybe Bool" (Just True)
  specBoolOps "Maybe (Maybe Bool)" (Just (Just True))
  describe "mixed type ops" $ do
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
  where
  specBoolOps typeName (_typeSample :: t) =
    describe (printf "ops on %s" typeName) $ do
      it "detects True using isCertainlyTrue" $ do
        isCertainlyTrue (convert True :: t) `shouldBe`  True
      it "does not detect False using isCertainlyTrue" $ do
        isCertainlyTrue (convert False :: t) `shouldBe`  False
      it "detects False using isCertainlyFalse" $ do
        isCertainlyFalse (convert False :: t) `shouldBe`  True
      it "does not detect True using isCertainlyFalse" $ do
        isCertainlyFalse (convert True :: t) `shouldBe`  False
      it "ignores double negation" $ do
        SC.property $ \ (x :: Bool) -> (not (not x)) P.== x
      it "distributes not over ||" $ do
        SC.property $ \ (x :: Bool) (y :: Bool) -> (not (x || y)) P.== ((not x) && (not y))
      it "distributes not over &&" $ do
        SC.property $ \ (x :: Bool) (y :: Bool) -> (not (x && y)) P.== ((not x) || (not y))
      it "has idempotent ||" $ do
        SC.property $ \ (x :: Bool) -> (x || x) P.== x
      it "has idempotent &&" $ do
        SC.property $ \ (x :: Bool) -> (x && x) P.== x
      it "has symmetric ||" $ do
        SC.property $ \ (x :: Bool) (y :: Bool) -> (x || y) P.== (y || x)
      it "has symmetric &&" $ do
        SC.property $ \ (x :: Bool) (y :: Bool) -> (x && y) P.== (y && x)
      it "has associative ||" $ do
        SC.property $ \ (x :: Bool) (y :: Bool) (z :: Bool) ->
                        (x || (y || z)) P.== ((x || y) || z)
      it "has associative &&" $ do
        SC.property $ \ (x :: Bool) (y :: Bool) (z :: Bool) ->
                        (x && (y && z)) P.== ((x && y) && z)
      it "distributes || over &&" $ do
        SC.property $ \ (x :: Bool) (y :: Bool) (z :: Bool) ->
                        (x || (y && z)) P.== ((x || y) && (x || z))
      it "distributes && over ||" $ do
        SC.property $ \ (x :: Bool) (y :: Bool) (z :: Bool) ->
                        (x && (y || z)) P.== ((x && y) || (x && z))

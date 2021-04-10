{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Div
    Description :  Bottom-up typed division
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Div
(
  -- * Division
    CanDiv(..), CanDivBy, CanDivSameType
  , CanRecip, CanRecipSameType
  , (/), recip
  -- ** Tests
  , specCanDiv, specCanDivNotMixed
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import Numeric.CollectErrors ( CN, cn )
import qualified Numeric.CollectErrors as CN

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
-- import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxAbs
-- import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Ring

{---- Division -----}

{-|
  A replacement for Prelude's binary `P./`.  If @t1 = t2@ and @Fractional t1@,
  then one can use the default implementation to mirror Prelude's @/@.
-}
class CanDiv t1 t2 where
  type DivType t1 t2
  divide :: t1 -> t2 -> DivType t1 t2

divideCN ::
  (CanTestZero t2)
  =>
  (t1 -> t2 -> t3) ->
  CN t1 -> CN t2 -> CN t3
divideCN unsafeDivide a b
  | isCertainlyZero b = CN.noValueNumErrorCertain e
  | isCertainlyNonZero b = r
  | otherwise = errPote r
  where
  r = CN.lift2 unsafeDivide a b
  -- errCert :: CN t -> CN t
  -- errCert = CN.prependErrorCertain e
  errPote :: CN t -> CN t
  errPote = CN.prependErrorPotential e
  e :: CN.NumError
  e = CN.DivByZero

infixl 7  /

(/) :: (CanDiv t1 t2) => t1 -> t2 -> DivType t1 t2
(/) = divide

type CanRecip t =
  (CanDiv Integer t)

type CanRecipSameType t =
  (CanDiv Integer t, DivType Integer t ~ t)

recip :: (CanRecip t) => t -> DivType Integer t
recip = divide 1

type CanDivBy t1 t2 =
  (CanDiv t1 t2, DivType t1 t2 ~ t1)
type CanDivSameType t =
  CanDivBy t t

{-|
  HSpec properties that each implementation of CanDiv should satisfy.
 -}
specCanDiv ::
  (Show t1, Show t2, Show (DivType Integer (DivType Integer t1)),
   Show (DivType t1 t2), Show (DivType t1 t1),
   Show (MulType t1 (DivType t1 t2)), Arbitrary t1, Arbitrary t2,
   ConvertibleExactly Integer t1, ConvertibleExactly Integer t2,
   CanTestCertainly
     (EqCompareType (DivType Integer (DivType Integer t1)) t1),
   CanTestCertainly (EqCompareType (DivType t1 t2) t1),
   CanTestCertainly (EqCompareType (DivType t1 t1) t1),
   CanTestCertainly
     (EqCompareType (DivType t1 t2) (MulType t1 (DivType t1 t2))),
   HasEqAsymmetric (DivType Integer (DivType Integer t1)) t1,
   HasEqAsymmetric (DivType t1 t2) t1,
   HasEqAsymmetric (DivType t1 t2) (MulType t1 (DivType t1 t2)),
   HasEqAsymmetric (DivType t1 t1) t1, CanTestZero t1, CanTestZero t2,
   CanTestZero (DivType Integer t1),
   CanMulAsymmetric t1 (DivType t1 t2), CanDiv t1 t1, CanDiv t1 t2,
   CanDiv Integer t1, CanDiv Integer (DivType Integer t1))
  =>
  T t1 -> T t2 -> Spec
specCanDiv (T typeName1 :: T t1) (T typeName2 :: T t2) =
  describe (printf "CanDiv %s %s" typeName1 typeName2) $ do
    it "recip(recip x) = x" $ do
      property $ \ (x :: t1) ->
        (isCertainlyNonZero x && isCertainlyNonZero (recip x)) ==>
          recip (recip x) ?==?$ x
    it "x/1 = x" $ do
      property $ \ (x :: t1) -> let one = (convertExactly 1 :: t2) in (x / one) ?==?$ x
    it "x/x = 1" $ do
      property $ \ (x :: t1) ->
        (isCertainlyNonZero x) ==>
          let one = (convertExactly 1 :: t1) in (x / x) ?==?$ one
    it "x/y = x*(1/y)" $ do
      property $ \ (x :: t1) (y :: t2) ->
        (isCertainlyNonZero y) ==>
          let one = (convertExactly 1 :: t1) in (x / y) ?==?$ x * (one/y)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

{-|
  HSpec properties that each implementation of CanDiv should satisfy.
 -}
specCanDivNotMixed ::
  (Show t, Show (DivType Integer (DivType Integer t)),
   Show (DivType t t), Show (MulType t (DivType t t)), Arbitrary t,
   ConvertibleExactly Integer t,
   CanTestCertainly
     (EqCompareType (DivType Integer (DivType Integer t)) t),
   CanTestCertainly (EqCompareType (DivType t t) t),
   CanTestCertainly
     (EqCompareType (DivType t t) (MulType t (DivType t t))),
   HasEqAsymmetric (DivType Integer (DivType Integer t)) t,
   HasEqAsymmetric (DivType t t) t,
   HasEqAsymmetric (DivType t t) (MulType t (DivType t t)),
   CanTestZero t, CanTestZero (DivType Integer t),
   CanMulAsymmetric t (DivType t t), CanDiv t t, CanDiv Integer t,
   CanDiv Integer (DivType Integer t))
  =>
  T t -> Spec
specCanDivNotMixed (t :: T t) = specCanDiv t t

instance CanDiv Int Int where
  type DivType Int Int = Rational
  divide a b = (P./) (rational a) (rational b)

instance CanDiv Integer Integer where
  type DivType Integer Integer = Rational
  divide a b = (P./) (rational a) (rational b)
instance CanDiv Rational Rational where
  type DivType Rational Rational = Rational
  divide = (P./)

instance CanDiv Int Integer where
  type DivType Int Integer = Rational
  divide a b = (P./) (rational a) (rational b)
instance CanDiv Integer Int where
  type DivType Integer Int = Rational
  divide a b = (P./) (rational a) (rational b)

instance CanDiv Int Rational where
  type DivType Int Rational = Rational
  divide = convertFirst divide
instance CanDiv Rational Int where
  type DivType Rational Int = Rational
  divide = convertSecond divide

instance CanDiv Integer Rational where
  type DivType Integer Rational = Rational
  divide = convertFirst divide
instance CanDiv Rational Integer where
  type DivType Rational Integer = Rational
  divide = convertSecond divide

instance CanDiv Double Double where
  type DivType Double Double = Double
  divide = (P./)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ t -> [d|

    instance CanDiv $t Double where
      type DivType $t Double = Double
      divide n d = divide (double n) d
    instance CanDiv Double $t where
      type DivType Double $t = Double
      divide d n = divide d (double n)
  |]))

instance (CanDiv a b) => CanDiv [a] [b] where
  type DivType [a] [b] = [DivType a b]
  divide (x:xs) (y:ys) = (divide x y) : (divide xs ys)
  divide _ _ = []

instance (CanDiv a b) => CanDiv (Maybe a) (Maybe b) where
  type DivType (Maybe a) (Maybe b) = Maybe (DivType a b)
  divide (Just x) (Just y) = Just (divide x y)
  divide _ _ = Nothing

instance
  (CanDiv a b, CanTestZero b)
  =>
  CanDiv (CN a) (CN  b)
  where
  type DivType (CN a) (CN b) = CN (DivType a b)
  divide  = divideCN divide

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (CanDiv $t b, CanTestZero b)
      =>
      CanDiv $t (CN  b)
      where
      type DivType $t (CN b) = CN (DivType $t b)
      divide a b = divideCN divide (cn a) b

    instance
      (CanDiv a $t)
      =>
      CanDiv (CN a) $t
      where
      type DivType (CN a) $t = CN (DivType a $t)
      divide a b = divideCN divide a (cn b)
  |]))

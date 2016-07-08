{-|
    Module      :  Numeric.MixedType.AddSub
    Description :  Bottom-up typed addition and subtraction
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.AddSub
(
    -- * Addition
    CanAdd, CanAddAsymmetric(..), CanAddThis, CanAddSameType
    , (+), sum
  -- ** Tests
    , specCanAdd, specCanAddNotMixed, specCanAddSameType, CanAddX, CanAddXX,
    -- * Subtraction
    CanSub(..), CanSubThis, CanSubSameType
    , (-)
  -- ** Tests
    , specCanSub, specCanSubNotMixed, CanSubX
)
where

import Prelude hiding
  (fromInteger,
   negate,not,(&&),(||),and,or,
   (==), (/=), (>), (<), (<=), (>=),
   abs, min, max, minimum, maximum,
   (-), (+), sum)
import qualified Prelude as P
import Text.Printf

import qualified Data.List as List

import Test.Hspec
import qualified Test.QuickCheck as QC

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool (CanNeg(..))
import Numeric.MixedTypes.EqOrd
import Numeric.MixedTypes.MinMaxAbs ()

{---- Addition -----}

type CanAdd t1 t2 =
  (CanAddAsymmetric t1 t2, CanAddAsymmetric t2 t1,
   AddType t1 t2 ~ AddType t2 t1)

{-|
  A replacement for Prelude's `P.+`.  If @t1 = t2@ and @Num t1@,
  then one can use the default implementation to mirror Prelude's @+@.
-}
class CanAddAsymmetric t1 t2 where
  type AddType t1 t2
  type AddType t1 t2 = t1 -- default
  add :: t1 -> t2 -> AddType t1 t2
  default add :: (AddType t1 t2 ~ t1, t1~t2, P.Num t1) => t1 -> t1 -> t1
  add = (P.+)

infixl 6  +, -

(+) :: (CanAddAsymmetric t1 t2) => t1 -> t2 -> AddType t1 t2
(+) = add

type CanAddThis t1 t2 =
  (CanAdd t1 t2, AddType t1 t2 ~ t1)
type CanAddSameType t =
  CanAddThis t t

sum :: (CanAddSameType t, Convertible Integer t) => [t] -> t
sum xs = List.foldl' add (convert 0) xs

{-| Compound type constraint useful for test definition. -}
type CanAddX t1 t2 =
  (CanAdd t1 t2,
   Show t1, QC.Arbitrary t1,
   Show t2, QC.Arbitrary t2,
   HasEq t1 (AddType t1 t2),
   HasEq t2 (AddType t1 t2),
   HasEq (AddType t1 t2) (AddType t1 t2),
   HasOrder t1 (AddType t1 t2),
   HasOrder t2 (AddType t1 t2),
   HasOrder (AddType t1 t2) (AddType t1 t2))

{-| Compound type constraint useful for test definition. -}
type CanAddXX t1 t2 =
  (CanAddX t1 t2,
   HasEq (AddType t1 t2) (AddType t2 t1))

{-|
  HSpec properties that each implementation of CanAdd should satisfy.
 -}
specCanAdd ::
  (CanAddXX t1 t1,
   CanAddXX t1 t2,
   CanAddXX t1 t3, CanAddXX t2 t3,
   CanAddXX t1 (AddType t2 t3),
   CanAddXX (AddType t1 t2) t3,
   Convertible Integer t1,
   CanTestPosNeg t1,
   HasEq (AddType t1 (AddType t2 t3)) (AddType (AddType t1 t2) t3))
  =>
  T t1 -> T t2 -> T t3 -> Spec
specCanAdd (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "CanAdd %s %s, CanAdd %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "absorbs 0" $ do
      QC.property $ \ (x :: t1) -> let z = (convert 0 :: t1) in (x + z) //== x
    it "is commutative" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x + y) //== (y + x)
    it "is associative" $ do
      QC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x + (y + z)) //== ((x + y) + z)
    it "increases when positive" $ do
      QC.property $ \ (x :: t1) (y :: t2) ->
        (isCertainlyPositive x) QC.==> (x + y) //> y
    it "decreases when negative" $ do
      QC.property $ \ (x :: t1) (y :: t2) ->
        (isCertainlyNegative x) QC.==> (x + y) //< y

--
{-|
  HSpec properties that each implementation of CanAdd should satisfy.
 -}
specCanAddNotMixed ::
  (CanAddXX t t,
   CanAddXX t (AddType t t),
   Convertible Integer t,
   CanTestPosNeg t,
   HasEq (AddType (AddType t t) t) (AddType t (AddType t t)) )
  =>
  T t -> Spec
specCanAddNotMixed t = specCanAdd t t t

{-|
  HSpec properties that each implementation of CanAddSameType should satisfy.
 -}
specCanAddSameType ::
  (Convertible Integer t,
   HasEqAsymmetric t t, CanAddSameType t)
   =>
   T t -> SpecWith ()
specCanAddSameType (T typeName :: T t) =
  describe (printf "CanAddSameType %s" typeName) $ do
    it "has sum working over integers" $ do
      QC.property $ \ (xsi :: [Integer]) ->
        (sum $ (map convert xsi :: [t])) //== (convert (sum xsi) :: t)
    it "has sum [] = 0" $ do
        (sum ([] :: [t])) //== (convert 0 :: t)


instance CanAddAsymmetric Int Int
instance CanAddAsymmetric Integer Integer
instance CanAddAsymmetric Rational Rational
instance CanAddAsymmetric Double Double

instance CanAddAsymmetric Int Integer where
  type AddType Int Integer = Integer
  add = convertFirst add
instance CanAddAsymmetric Integer Int where
  type AddType Integer Int = Integer
  add = convertSecond add

instance CanAddAsymmetric Int Rational where
  type AddType Int Rational = Rational
  add = convertFirst add
instance CanAddAsymmetric Rational Int where
  type AddType Rational Int = Rational
  add = convertSecond add

instance CanAddAsymmetric Integer Rational where
  type AddType Integer Rational = Rational
  add = convertFirst add
instance CanAddAsymmetric Rational Integer where
  type AddType Rational Integer = Rational
  add = convertSecond add

instance CanAddAsymmetric Int Double where
  type AddType Int Double = Double
  add = convertFirst add
instance CanAddAsymmetric Double Int where
  type AddType Double Int = Double
  add = convertSecond add

instance CanAddAsymmetric Integer Double where
  type AddType Integer Double = Double
  add = convertFirst add
instance CanAddAsymmetric Double Integer where
  type AddType Double Integer = Double
  add = convertSecond add

instance CanAddAsymmetric Rational Double where
  type AddType Rational Double = Double
  add = convertFirst add
instance CanAddAsymmetric Double Rational where
  type AddType Double Rational = Double
  add = convertSecond add

instance (CanAddAsymmetric a b) => CanAddAsymmetric [a] [b] where
  type AddType [a] [b] = [AddType a b]
  add (x:xs) (y:ys) = (add x y) : (add xs ys)
  add _ _ = []

instance (CanAddAsymmetric a b) => CanAddAsymmetric (Maybe a) (Maybe b) where
  type AddType (Maybe a) (Maybe b) = Maybe (AddType a b)
  add (Just x) (Just y) = Just (add x y)
  add _ _ = Nothing

{---- Subtraction -----}

{-|
  A replacement for Prelude's binary `P.-`.  If @t1 = t2@ and @Num t1@,
  then one can use the default implementation to mirror Prelude's @-@.
-}
class CanSub t1 t2 where
  type SubType t1 t2
  type SubType t1 t2 = t1 -- default
  sub :: t1 -> t2 -> SubType t1 t2
  default sub :: (SubType t1 t2 ~ t1, t1~t2, P.Num t1) => t1 -> t1 -> t1
  sub = (P.-)

(-) :: (CanSub t1 t2) => t1 -> t2 -> SubType t1 t2
(-) = sub

type CanSubThis t1 t2 =
  (CanSub t1 t2, SubType t1 t2 ~ t1)
type CanSubSameType t =
  CanSubThis t t

{-| Compound type constraint useful for test definition. -}
type CanSubX t1 t2 =
  (CanSub t1 t2,
   HasEq t1 (SubType t1 t2),
   CanAddXX t1 t2)

{-|
  HSpec properties that each implementation of CanSub should satisfy.
 -}
specCanSub ::
  (CanSubX t1 t1,
   CanSubX t1 t2,
   CanNeg t2,
   CanAdd t1 (NegType t2),
   HasEq (SubType t1 t2) (AddType t1 (NegType t2)),
   Convertible Integer t1)
  =>
  T t1 -> T t2 -> Spec
specCanSub (T typeName1 :: T t1) (T typeName2 :: T t2) =
  describe (printf "CanSub %s %s" typeName1 typeName2) $ do
    it "x-0 = x" $ do
      QC.property $ \ (x :: t1) -> let z = (convert 0 :: t1) in (x - z) //== x
    it "x-x = 0" $ do
      QC.property $ \ (x :: t1) -> let z = (convert 0 :: t1) in (x - x) //== z
    it "x-y = x+(-y)" $ do
      QC.property $ \ (x :: t1) (y :: t2) ->
        (x - y) //== (x + (negate y))

--
{-|
  HSpec properties that each implementation of CanSub should satisfy.
 -}
specCanSubNotMixed ::
  (CanSubX t t,
   CanSubX t (SubType t t),
   CanNeg t,
   CanAdd t (NegType t),
   HasEq (SubType t t) (AddType t (NegType t)),
   Convertible Integer t)
  =>
  T t -> Spec
specCanSubNotMixed t = specCanSub t t

instance CanSub Int Int
instance CanSub Integer Integer
instance CanSub Rational Rational
instance CanSub Double Double

instance CanSub Int Integer where
  type SubType Int Integer = Integer
  sub = convertFirst sub
instance CanSub Integer Int where
  type SubType Integer Int = Integer
  sub = convertSecond sub

instance CanSub Int Rational where
  type SubType Int Rational = Rational
  sub = convertFirst sub
instance CanSub Rational Int where
  type SubType Rational Int = Rational
  sub = convertSecond sub

instance CanSub Integer Rational where
  type SubType Integer Rational = Rational
  sub = convertFirst sub
instance CanSub Rational Integer where
  type SubType Rational Integer = Rational
  sub = convertSecond sub

instance CanSub Int Double where
  type SubType Int Double = Double
  sub = convertFirst sub
instance CanSub Double Int where
  type SubType Double Int = Double
  sub = convertSecond sub

instance CanSub Integer Double where
  type SubType Integer Double = Double
  sub = convertFirst sub
instance CanSub Double Integer where
  type SubType Double Integer = Double
  sub = convertSecond sub

instance CanSub Rational Double where
  type SubType Rational Double = Double
  sub = convertFirst sub
instance CanSub Double Rational where
  type SubType Double Rational = Double
  sub = convertSecond sub

instance (CanSub a b) => CanSub [a] [b] where
  type SubType [a] [b] = [SubType a b]
  sub (x:xs) (y:ys) = (sub x y) : (sub xs ys)
  sub _ _ = []

instance (CanSub a b) => CanSub (Maybe a) (Maybe b) where
  type SubType (Maybe a) (Maybe b) = Maybe (SubType a b)
  sub (Just x) (Just y) = Just (sub x y)
  sub _ _ = Nothing

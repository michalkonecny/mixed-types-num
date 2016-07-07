{-|
    Module      :  Numeric.MixedType.Bool
    Description :  Mixed-type generic Boolean operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Bool
(
  IsBool, specIsBool
  , stronglyImplies, stronglyEquivalentTo
  , weaklyImplies, weaklyEquivalentTo
  -- * Conversion to/from Bool
  , HasBools(..), HasBoolsX, specHasBools
  -- * Negation
  , CanNeg(..), not, CanNegSameType, CanNegX, specCanNeg
  -- * And and or
  , CanAndOr(..), (&&), (||), CanAndOrWith, CanAndOrSameType, and, or,
    CanAndOrX, specCanAndOr, specCanAndOrNotMixed
)
where

import Prelude hiding (negate,not,(&&),(||),and,or)
import qualified Prelude as P
import Text.Printf

import qualified Data.List as List

import Numeric.MixedTypes.Literals (Convertible(..), convert)

import Test.Hspec
-- import qualified Test.QuickCheck as QC
import qualified Test.Hspec.SmallCheck as HSC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Series as SCS

{-|
  Tests for truth or falsity.  Beware, when @isCertainlyTrue@ returns @False@,
  it does not mean that the proposition is false.  It usually means that
  we failed to prove the proposition.
-}
class (Convertible Bool t) => HasBools t
  where
    isCertainlyTrue :: t -> Bool
    isCertainlyFalse :: t -> Bool

{-|
  HasBools extended with other type constraints that make
  the type suitable for testing.
-}
type HasBoolsX t = (HasBools t, Show t, SCS.Serial IO t)

{-|
  If l is certainly True, then r is also certainly True.
-}
stronglyImplies :: (HasBools t1, HasBools t2) => t1 -> t2 -> Bool
stronglyImplies l r =
  (P.not (isCertainlyTrue l) P.|| isCertainlyTrue r)

{-|
  If l is certainly True, then r is not certainly False.
-}
weaklyImplies :: (HasBools t1, HasBools t2) => t1 -> t2 -> Bool
weaklyImplies l r =
  (P.not $ isCertainlyTrue l) P.|| (P.not $ isCertainlyFalse r)

stronglyEquivalentTo :: (HasBools t1, HasBools t2) => t1 -> t2 -> Bool
stronglyEquivalentTo l r =
  stronglyImplies l r P.&& stronglyImplies r l

weaklyEquivalentTo :: (HasBools t1, HasBools t2) => t1 -> t2 -> Bool
weaklyEquivalentTo l r =
  weaklyImplies l r P.&& weaklyImplies r l

{-|
  HSpec properties that each implementation of HasBools should satisfy.
 -}
specHasBools :: (HasBools t) => String -> t -> Spec
specHasBools typeName (_typeSample :: t) =
  describe (printf "HasBools %s" typeName) $ do
    it "detects True using isCertainlyTrue" $ do
      isCertainlyTrue (convert True :: t) `shouldBe`  True
    it "does not detect False using isCertainlyTrue" $ do
      isCertainlyTrue (convert False :: t) `shouldBe`  False
    it "detects False using isCertainlyFalse" $ do
      isCertainlyFalse (convert False :: t) `shouldBe`  True
    it "does not detect True using isCertainlyFalse" $ do
      isCertainlyFalse (convert True :: t) `shouldBe`  False

instance Convertible Bool Bool where
  safeConvert b = Right b

instance HasBools Bool where
  isCertainlyTrue = id
  isCertainlyFalse = not

instance (Convertible Bool t) => Convertible Bool (Maybe t) where
  safeConvert b =
    case (safeConvert b) of
      Left _ -> Right Nothing
      Right r -> Right (Just r)

instance (HasBools t) => HasBools (Maybe t) where
  isCertainlyTrue (Just b) = isCertainlyTrue b
  isCertainlyTrue _ = False
  isCertainlyFalse (Just b) = isCertainlyFalse b
  isCertainlyFalse _ = False

{---- Negation ----}

{-|
  This is negation is both the numeric negation as well as the Boolean negation.
  Example of non-standard Boolean negation:

  @
  negate (Just True) = Just False
  @
 -}
class CanNeg t where
  type NegType t
  negate :: t -> NegType t

{-| A synonym of 'negate'. -}
not :: (CanNeg t) => t -> NegType t
not = negate

type CanNegSameType t = (CanNeg t, NegType t ~ t)

type CanNegX t =
  (CanNeg t, HasBoolsX t, HasBoolsX (NegType t))

{-|
  HSpec properties that each implementation of CanNegSameType should satisfy.
 -}
specCanNeg ::
  (CanNegX t, CanNegX (NegType t))
  =>
  String -> t -> Spec
specCanNeg typeName (_typeSample :: t) =
  describe (printf "CanNegSameType %s" typeName) $ do
    it "ignores double negation" $ do
      HSC.property $ \ (x :: t) -> (not (not x)) `scEquals` x
    it "negates True to False" $ do
      HSC.property $ \ (x :: t) ->
        (isCertainlyTrue x) SC.==> (isCertainlyFalse (not x))
    it "negates False to True" $ do
      HSC.property $ \ (x :: t) ->
        (isCertainlyFalse x) SC.==> (isCertainlyTrue (not x))


instance CanNeg Bool where
  type NegType Bool = Bool
  negate b = P.not b

instance CanNeg t => CanNeg (Maybe t) where
  type NegType (Maybe t) = Maybe (NegType t)
  negate = fmap negate

_testNeg1 :: Maybe Bool
_testNeg1 = not (Just True)

{---- And/Or ----}

{-|
  Binary logical `and' and `or' for generalised Booleans.  For example:

  @
  (Just True) && False = Just False
  (Just (Just True)) || False = (Just (Just True))
  @
 -}
class CanAndOr t1 t2 where
  type AndOrType t1 t2
  and2 :: t1 -> t2 -> AndOrType t1 t2
  or2 :: t1 -> t2 -> AndOrType t1 t2

type CanAndOrWith t1 t2 = (CanAndOr t1 t2, AndOrType t1 t2 ~ t1)
type CanAndOrSameType t = (CanAndOrWith t t)

infixr 3  &&
infixr 2  ||

{-| A synonym of 'and2'. -}
(&&) :: (CanAndOr a b) => a -> b -> AndOrType a b
(&&) = and2
{-| A synonym of 'or2'. -}
(||) :: (CanAndOr a b) => a -> b -> AndOrType a b
(||) = or2

and :: (CanAndOrSameType t, HasBools t) => [t] -> t
and = List.foldl' (&&) (convert True)

or :: (CanAndOrSameType t, HasBools t) => [t] -> t
or = List.foldl' (||) (convert True)

type CanAndOrX t1 t2 =
  (CanAndOr t1 t2,
   CanNeg t1,
   CanNeg t2,
   CanAndOr (NegType t1) t2,
   CanAndOr t1 (NegType t2),
   CanAndOr (NegType t1) (NegType t2),
   HasBoolsX t1,
   HasBoolsX t2,
   HasBoolsX (AndOrType t1 t2),
   HasBoolsX (NegType (AndOrType t1 t2)),
   HasBoolsX (AndOrType (NegType t1) t2),
   HasBoolsX (AndOrType t1 (NegType t2)),
   HasBoolsX (AndOrType (NegType t1) (NegType t2))
   )

{-|
  HSpec properties that each implementation of CanAndOr should satisfy.
 -}
specCanAndOr ::
  (CanAndOrX t1 t1,
   CanAndOrX t1 t2, CanAndOrX t2 t1,
   CanAndOrX t1 t3, CanAndOrX t2 t3,
   CanAndOrX (AndOrType t1 t2) t3, CanAndOrX t1 (AndOrType t2 t3),
   CanAndOrX (AndOrType t1 t2) (AndOrType t1 t3))
  =>
  String -> t1 ->
  String -> t2 ->
  String -> t3 ->
  Spec
specCanAndOr typeName1 (_typeSample1 :: t1) typeName2 (_typeSample2 :: t2) typeName3 (_typeSample3 :: t3) =
  describe (printf "CanAndOr %s %s, CanAndOr %s %s)" typeName1 typeName2 typeName2 typeName3) $ do
    it "has idempotent ||" $ do
      HSC.property $ \ (x :: t1) -> (x || x) `scEquals` x
    it "has idempotent &&" $ do
      HSC.property $ \ (x :: t1) -> (x && x) `scEquals` x
    it "has commutative ||" $ do
      HSC.property $ \ (x :: t1) (y :: t2) -> (x || y) `scEquals` (y || x)
    it "has commutative &&" $ do
      HSC.property $ \ (x :: t1) (y :: t2) -> (x && y) `scEquals` (y && x)
    it "has associative ||" $ do
      HSC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x || (y || z)) `scEquals` ((x || y) || z)
    it "has associative &&" $ do
      HSC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x && (y && z)) `scEquals` ((x && y) && z)
    it "distributes || over &&" $ do
      HSC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x || (y && z)) `scEquals` ((x || y) && (x || z))
    it "distributes && over ||" $ do
      HSC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x && (y || z)) `scEquals` ((x && y) || (x && z))
    it "distributes not over ||" $ do
      HSC.property $ \ (x :: t1) (y :: t2) -> (not (x || y)) `scEquals` ((not x) && (not y))
    it "distributes not over &&" $ do
      HSC.property $ \ (x :: t1) (y :: t2) -> (not (x && y)) `scEquals` ((not x) || (not y))

{-|
  HSpec properties that each implementation of CanAndOr should satisfy.
 -}
specCanAndOrNotMixed ::
  (CanAndOrX t t,
   CanAndOrX (AndOrType t t) t, CanAndOrX t (AndOrType t t),
   CanAndOrX (AndOrType t t) (AndOrType t t))
  =>
  String -> t -> Spec
specCanAndOrNotMixed typeName typeSample =
  specCanAndOr typeName typeSample typeName typeSample typeName typeSample

instance CanAndOr Bool Bool where
  type AndOrType Bool Bool = Bool
  and2 = (P.&&)
  or2 = (P.||)

instance (CanAndOr t1 t2, HasBools t1, HasBools t2, HasBools (AndOrType t1 t2)) =>
  CanAndOr (Maybe t1) (Maybe t2)
  where
  type AndOrType (Maybe t1) (Maybe t2) = Maybe (AndOrType t1 t2)
  and2 (Just b1) _ | isCertainlyFalse b1 = Just (convert False)
  and2 _ (Just b2) | isCertainlyFalse b2 = Just (convert False)
  and2 (Just b1) (Just b2) = Just (b1 && b2)
  and2 _ _ = Nothing
  or2 (Just b1) _ | isCertainlyTrue b1 = Just (convert True)
  or2 _ (Just b2) | isCertainlyTrue b2 = Just (convert True)
  or2 (Just b1) (Just b2) = Just (b1 || b2)
  or2 _ _ = Nothing

instance (CanAndOr Bool t2, HasBools t2, HasBools (AndOrType Bool t2)) =>
  CanAndOr Bool (Maybe t2)
  where
  type AndOrType Bool (Maybe t2) = Maybe (AndOrType Bool t2)
  and2 False _ = Just (convert False)
  and2 _ (Just b2) | isCertainlyFalse b2 = Just (convert False)
  and2 b1 (Just b2) = Just (b1 && b2)
  and2 _ _ = Nothing
  or2 True _ = Just (convert True)
  or2 _ (Just b2) | isCertainlyTrue b2 = Just (convert True)
  or2 b1 (Just b2) = Just (b1 || b2)
  or2 _ _ = Nothing

instance (CanAndOr t1 Bool, HasBools t1, HasBools (AndOrType t1 Bool)) =>
  CanAndOr (Maybe t1) Bool
  where
  type AndOrType (Maybe t1) Bool = Maybe (AndOrType t1 Bool)
  and2 _ False = Just (convert False)
  and2 (Just b1) _ | isCertainlyFalse b1 = Just (convert False)
  and2 (Just b1) b2 = Just (b1 && b2)
  and2 _ _ = Nothing
  or2 _ True = Just (convert True)
  or2 (Just b1) _ | isCertainlyTrue b1 = Just (convert True)
  or2 (Just b1) b2 = Just (b1 || b2)
  or2 _ _ = Nothing

_testAndOr1 :: Maybe Bool
_testAndOr1 = (Just True) && False

_testAndOr2 :: Maybe (Maybe Bool)
_testAndOr2 = (Just (Just True)) || False

_testAndOr3 :: Maybe Bool
_testAndOr3 = and [Just True, Nothing, Just False]

{-|
  A type constraint synonym that stipulates that the type behaves very
  much like Bool, except it does not necessarily satisfy the law of excluded middle,
  which means that the type can contain a "do-not-know" value.

  Examples: @Bool@, @Maybe Bool@, @Maybe (Maybe Bool)@
-}
type IsBool t = (HasBools t, CanNegSameType t, CanAndOrSameType t)

{-|
  HSpec properties that each implementation of IsBool should satisfy.
 -}
specIsBool :: (IsBool t, Show t, SCS.Serial IO t) => String -> t -> Spec
specIsBool typeName (typeSample :: t) =
  describe (printf "IsBool %s" typeName) $ do
    specHasBools typeName typeSample
    specCanNeg typeName typeSample
    specCanAndOrNotMixed typeName typeSample

scEquals ::
  (Show t1, HasBools t1, Show t2, HasBools t2) =>
  t1 -> t2 -> Either String String
scEquals l r
  | l `stronglyEquivalentTo` r = Right "OK"
  | otherwise = Left $ printf "(%s) /= (%s)" (show l) (show r)

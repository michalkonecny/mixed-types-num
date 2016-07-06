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
  IsBool
  -- * Conversion to/from Bool
  , HasBools(..)
  -- * Negation
  , CanNeg(..), not, CanNegSameType
  -- * And and or
  , CanAndOr(..), (&&), (||), CanAndOrWith, CanAndOrSameType, and, or
)
where

import Prelude hiding (negate,not,(&&),(||),and,or)
import qualified Prelude as P

import qualified Data.List as List

import Numeric.MixedTypes.Literals (Convertible(..), convert)

{-|
  A type constraint synonym that stipulates that the type behaves very
  much like Bool, except it does not necessarily satisfy the law of excluded middle,
  which means that the type can contain a "do-not-know" value.

  Examples: @Bool@, @Maybe Bool@, @Maybe (Maybe Bool)@
-}
type IsBool t = (HasBools t, CanNegSameType t, CanAndOrSameType t)

{-|
  Tests for truth or falsity.  Beware, when @isCertainlyTrue@ returns @False@,
  it does not mean that the proposition is false.  It usually means that
  we failed to prove the proposition.
-}
class (Convertible Bool t) => HasBools t
  where
    isCertainlyTrue :: t -> Bool
    isCertainlyFalse :: t -> Bool

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

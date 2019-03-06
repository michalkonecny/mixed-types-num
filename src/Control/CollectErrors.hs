{-# LANGUAGE TemplateHaskell #-}
module Control.CollectErrors
(
-- * Monad for collecting errors in expressions
  CollectErrors(..), SuitableForCE
, CanTestErrorsCertain(..), hasCertainErrorCE
, noValueCE, prependErrorsCE
, filterValuesWithoutErrorCE, getValueIfNoErrorCE
, ce2ConvertResult
-- * Tools for avoiding @CollectErrors(CollectErrors t)@ and putting CE inside containers
, CanEnsureCE(..)
, getValueOrThrowErrorsNCE
, lift1CE, lift2CE, lift2TCE, lift2TLCE
-- ** Tools for pulling errors out of structures
, CanExtractCE(..)
)
where

import Prelude
  (Functor(..), Applicative(..), Monad(..), (<$>), ($), (.)
  , error, const, flip, not, fst, snd, foldMap, (++)
  , Int, Integer, Rational, Double, Bool, Char
  , Maybe(..), Either(..)
  , Show(..), Eq(..)
  , Traversable(..))
import Text.Printf

import Control.Monad (join)

import Data.Monoid
import Data.Maybe (fromJust)

import Data.Convertible
import Data.Typeable

-- import Language.Haskell.TH

import Test.QuickCheck

{-|
  A wrapper around values which can accommodate a list of
  (potential) errors that have (maybe) occurred during the computation
  of a value.  A value may be missing, leaving only the error(s).

  Such error collection allows one to write expressions with partial
  functions (ie functions that fail for some inputs) instead of
  branching after each application of such function.
  Dealing with the errors can be moved outside the expression.
  If the error data contain enough information, their list can be used
  to trace the source of the errors.
-}
data CollectErrors es v =
  CollectErrors
    { getMaybeValueCE :: Maybe v
    , getErrorsCE :: es }

class CanTestErrorsCertain es where
  hasCertainError :: es -> Bool

hasCertainErrorCE :: (CanTestErrorsCertain es) => (CollectErrors es v) -> Bool
hasCertainErrorCE (CollectErrors _ es) = hasCertainError es

type SuitableForCE es = (Monoid es, Eq es, Show es, CanTestErrorsCertain es)

instance (Show v, SuitableForCE es) => (Show (CollectErrors es v)) where
  show (CollectErrors mv es) =
    case mv of
      Just v | es == mempty -> show v
      Just v -> printf "%s{%s}" (show v) (show es)
      Nothing -> printf "{%s}" (show es)

noValueCE :: es -> CollectErrors es v
noValueCE es = CollectErrors Nothing es

prependErrorsCE :: (Monoid es) => es -> CollectErrors es v -> CollectErrors es v
prependErrorsCE es1 (CollectErrors mv es2) = CollectErrors mv (es1 <> es2)

ce2ConvertResult ::
  (Typeable t, Show t, SuitableForCE es)
  =>
  CollectErrors es t -> Either ConvertError t
ce2ConvertResult (CollectErrors mv es) =
  case mv of
    Just v | es == mempty -> Right v
    _ -> convError (show es) mv

{-| A safe way to get a value out of the CollectErrors wrapper. -}
getValueIfNoErrorCE ::
  (SuitableForCE es)
  =>
  CollectErrors es v -> (v -> t) -> (es -> t) -> t
getValueIfNoErrorCE (CollectErrors mv es) withValue withErrors =
  case mv of
    Just v | es == mempty -> withValue v
    _ -> withErrors es

filterValuesWithoutErrorCE ::
  (SuitableForCE es)
  =>
  [CollectErrors es v] -> [v]
filterValuesWithoutErrorCE [] = []
filterValuesWithoutErrorCE (vCE : rest) =
  getValueIfNoErrorCE vCE (: restDone) (const restDone)
  where
  restDone = filterValuesWithoutErrorCE rest

-- functor instances:

instance Functor (CollectErrors es) where
  fmap f (CollectErrors mv es) =
    CollectErrors (fmap f mv) es

instance (Monoid es) => Applicative (CollectErrors es) where
  pure v = CollectErrors (Just v) mempty
  (CollectErrors (Just a) ae) <*> (CollectErrors (Just b) be) =
    CollectErrors (Just (a b)) (ae <> be)
  (CollectErrors _ ae) <*> (CollectErrors _ be) =
    CollectErrors Nothing (ae <> be)

instance (Monoid es) => Monad (CollectErrors es) where
  ae >>= f =
    case ae of
      CollectErrors (Just a) es1 ->
        let (CollectErrors mv es2) = f a in
          CollectErrors mv (es1 <> es2)
      CollectErrors _ es ->
        CollectErrors Nothing es

instance (Arbitrary t, Monoid es) => Arbitrary (CollectErrors es t) where
  arbitrary = (\v -> CollectErrors (Just v) mempty) <$> arbitrary

{-|
  A mechanism for adding and removing CollectErrors
  to a type in a manner that depends on
  the shape of the type, especially whether
  it already has CollectErrors.
-}
class
  (Monoid es
  , EnsureCE es (EnsureCE es a) ~ EnsureCE es a
  , EnsureCE es (EnsureNoCE es a) ~ EnsureCE es a
  , EnsureNoCE es (EnsureCE es a) ~ EnsureNoCE es a
  , EnsureNoCE es (EnsureNoCE es a) ~ EnsureNoCE es a)
  =>
  CanEnsureCE es a where
  {-|
    Add CollectErrors to a type except when the type already
    has CollectErrors in it.
  -}
  type EnsureCE es a
  type EnsureCE es a = CollectErrors es a -- default
  type EnsureNoCE es a
  type EnsureNoCE es a = a -- default

  {-|
    Translate a value of a type @a@
    to a value of a type @EnsureCE es a@.
  -}
  ensureCE ::
    Maybe es {-^ sample only -} ->
    a -> EnsureCE es a

  default ensureCE ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe es {-^ sample only -} ->
    a -> EnsureCE es a
  ensureCE _ = pure

  deEnsureCE ::
    Maybe es {-^ sample only -} ->
    EnsureCE es a -> Either es a

  default deEnsureCE ::
    (EnsureCE es a ~ CollectErrors es a, Eq es) =>
    Maybe es {-^ sample only -} ->
    EnsureCE es a -> Either es a
  deEnsureCE _ (CollectErrors mv es) =
    case mv of
      Just v | es == mempty -> Right v
      _ -> Left es

  ensureNoCE ::
    Maybe es {-^ sample only -} ->
    a -> (Maybe (EnsureNoCE es a), es)

  default ensureNoCE ::
    (EnsureNoCE es a ~ a, Eq es, Monoid es) =>
    Maybe es {-^ sample only -} ->
    a -> (Maybe (EnsureNoCE es a), es)
  ensureNoCE _ a = (Just a, mempty)

  {-|  Make CollectErrors record with no value, only errors. -}
  noValueECE ::
    Maybe a {-^ sample only -} ->
    es -> EnsureCE es a

  default noValueECE ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe a ->
    es -> EnsureCE es a
  noValueECE _ = noValueCE

  prependErrorsECE ::
    Maybe a ->
    es -> EnsureCE es a -> EnsureCE es a
  default prependErrorsECE ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe a ->
    es -> EnsureCE es a -> EnsureCE es a
  prependErrorsECE _ = prependErrorsCE

-- instance for CollectErrors a:

instance
  (SuitableForCE es, CanEnsureCE es a)
  =>
  CanEnsureCE es (CollectErrors es a)
  where
  type EnsureCE es (CollectErrors es a) = EnsureCE es a
  type EnsureNoCE es (CollectErrors es a) = EnsureNoCE es a

  ensureCE sample_es (CollectErrors mv es) =
    case mv of
      Just v -> prependErrorsECE (Just v) es $ ensureCE sample_es v
      _ -> noValueECE mv es
  deEnsureCE sample_es vCE =
    case deEnsureCE sample_es vCE of
      Right v -> Right $ CollectErrors (Just v) mempty
      Left es -> Left es
  ensureNoCE sample_es (CollectErrors mv es) =
    case fmap (ensureNoCE sample_es) mv of
      Just (Just v, es2) -> (Just v, es2 <> es)
      Just (_, es2) -> (Nothing, es2 <> es)
      _ -> (Nothing, mempty)

  noValueECE sample_vCE es =
    noValueECE (join $ fmap getMaybeValueCE sample_vCE) es
  prependErrorsECE sample_vCE =
    prependErrorsECE (join $ fmap getMaybeValueCE sample_vCE)

-- instances for ground types, using the default implementations:

instance (SuitableForCE es) => CanEnsureCE es Int
instance (SuitableForCE es) => CanEnsureCE es Integer
instance (SuitableForCE es) => CanEnsureCE es Rational
instance (SuitableForCE es) => CanEnsureCE es Double
instance (SuitableForCE es) => CanEnsureCE es Bool
instance (SuitableForCE es) => CanEnsureCE es Char
instance (SuitableForCE es) => CanEnsureCE es ()

-- instance for Maybe a:

instance
  (SuitableForCE es, CanEnsureCE es a)
  =>
  CanEnsureCE es (Maybe a)
  where
  type EnsureCE es (Maybe a) = Maybe (EnsureCE es a)
  type EnsureNoCE es (Maybe a) = Maybe (EnsureNoCE es a)

  ensureCE sample_es = fmap (ensureCE sample_es)
  deEnsureCE sample_es (Just vCE) = fmap Just (deEnsureCE sample_es vCE)
  deEnsureCE _sample_es Nothing = Right Nothing
  ensureNoCE sample_es (Just vCE) =
    case ensureNoCE sample_es vCE of
      (Just v, es) -> (Just (Just v), es)
      (_, es) -> (Nothing, es)
  ensureNoCE _sample_es Nothing = (Nothing, mempty)

  noValueECE sample_vCE es = Just (noValueECE (fromJust sample_vCE) es)

  prependErrorsECE sample_vCE es (Just vCE) =
    Just $ prependErrorsECE (fromJust sample_vCE) es vCE
  prependErrorsECE _sample_vCE _es Nothing = Nothing

instance
  (SuitableForCE es, CanEnsureCE es a)
  =>
  CanEnsureCE es (b -> a)
  where
  type EnsureCE es (b -> a) = b -> (EnsureCE es a)
  type EnsureNoCE es (b -> a) = b ->  (EnsureNoCE es a)

  ensureCE sample_es = ((ensureCE sample_es) .)
  deEnsureCE sample_es f =
    Right $ \ a ->
      case deEnsureCE sample_es (f a) of
        Right v -> v
        Left es -> error $ "deEnsureCE for function: " ++ show es
  ensureNoCE sample_es f = (Just f', mempty)
    where
    f' a =
      case ensureNoCE sample_es (f a) of
        (Just v, _) -> v
        (_, es) -> error $ "ensureNoCE for function: " ++ show es

  noValueECE (_fvCE :: Maybe (b -> a)) es =
    const (noValueECE (Nothing :: Maybe a) es)

  prependErrorsECE (_fvCE :: Maybe (b -> a)) es =
    ((prependErrorsECE (Nothing :: Maybe a) es) .)

-- instance (Monoid es) => CanEnsureCE es [a] where
-- instance (Monoid es) => CanEnsureCE es (Either e a) where

{-| An unsafe way to get a value out of an CollectErrors wrapper. -}
getValueOrThrowErrorsNCE ::
  (SuitableForCE es, CanEnsureCE es v, Show v)
  =>
  Maybe es {-^ sample only -} ->
  v -> (EnsureNoCE es v)
getValueOrThrowErrorsNCE sample_es v =
  case ensureNoCE sample_es v of
    (Just vNCE, es) | not (hasCertainError es) -> vNCE
    _ -> error (show v)

{-|
  Add error collection support to an unary function whose
  result may already have collected errors.
-}
lift1CE ::
  (SuitableForCE es
  , CanEnsureCE es a, CanEnsureCE es c)
  =>
  (a -> c) ->
  (CollectErrors es a) -> (EnsureCE es c)
lift1CE fn aCE =
  case ma of
    Just a ->
      prependErrorsECE sample_c a_es $ ensureCE sample_es $ fn a
    _ ->
      noValueECE sample_c a_es
  where
  CollectErrors ma a_es = aCE
  sample_es = Just a_es
  sample_c = fn <$> ma

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
-}
lift2CE ::
  (SuitableForCE es
  , CanEnsureCE es a, CanEnsureCE es b, CanEnsureCE es c)
  =>
  (a -> b -> c) ->
  (CollectErrors es a) -> (CollectErrors es b) -> (EnsureCE es c)
lift2CE fn aCE bCE =
  case (ma, mb) of
    (Just a, Just b) ->
      prependErrorsECE sample_c ab_es $ ensureCE sample_es $ fn a b
    _ ->
      noValueECE sample_c ab_es
  where
  CollectErrors ma a_es = aCE
  CollectErrors mb b_es = bCE
  ab_es = a_es <> b_es
  sample_es = Just a_es
  sample_c = fn <$> ma <*> mb

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
  A version where the second operand is not lifted, only the first one.
-}
lift2TCE ::
  (SuitableForCE es
  , CanEnsureCE es a, CanEnsureCE es c)
  =>
  (a -> b -> c) ->
  (CollectErrors es a) -> b -> (EnsureCE es c)
lift2TCE fn aCE b =
  case ma of
    (Just a) ->
      prependErrorsECE sample_c a_es $ ensureCE sample_es $ fn a b
    _ ->
      noValueECE sample_c a_es
  where
  CollectErrors ma a_es = aCE
  sample_es = Just a_es
  sample_c = fn <$> ma <*> (Just b)

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
  A version where the first operand is not lifted, only the second one.
-}
lift2TLCE ::
  (SuitableForCE es
  , CanEnsureCE es b, CanEnsureCE es c)
  =>
  (a -> b -> c) ->
  a -> (CollectErrors es b) -> (EnsureCE es c)
lift2TLCE f = flip $ lift2TCE (flip f)


{-|
  Ability to lift collected (potential) errors from inside some structure/collection.

  This is useful mostly for structures that use the default implementation of
  'CanEnsureCE es'.
-}
class (SuitableForCE es) => CanExtractCE es f where
  extractCE ::
    (CanEnsureCE es c) =>
    Maybe es ->
    f c -> CollectErrors es (f (EnsureNoCE es c))
  default extractCE ::
    (CanEnsureCE es c, Traversable f) =>
    Maybe es ->
    f c -> CollectErrors es (f (EnsureNoCE es c))
  extractCE sample_es fc =
    case mapM fst fcNoCE of
      Just fec -> pure fec
      _ -> noValueCE $ foldMap snd fcNoCE
    where
    fcNoCE = fmap (ensureNoCE sample_es) fc

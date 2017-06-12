{-# LANGUAGE TemplateHaskell #-}
module Control.CollectErrors
(
-- * Monad for collecting errors in expressions
  CollectErrors(..), SuitableForCE
, noValueCE, prependErrorsCE
, filterValuesWithoutErrorCE, getValueIfNoErrorCE, getValueOrThrowErrorsCE
, ce2ConvertResult
-- * Tools for avoiding @CollectErrors(CollectErrors t)@ and putting CE inside containers
, CanEnsureCE(..)
, filterValuesWithoutErrorECE, getValueIfNoErrorECE, getValueOrThrowErrorsECE
, lift1CE, lift2CE, lift2TCE, lift2TLCE
)
where

import Prelude
  (Functor(..), Applicative(..), Monad(..), (<$>), ($)
  , id, error, const, otherwise, flip
  , Int, Integer, Rational, Double, Bool, Char
  , Maybe(..), Either(..)
  , Show(..), Eq(..), (.))
import Text.Printf
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

type SuitableForCE es = (Monoid es, Eq es, Show es)

instance (Show v, SuitableForCE es) => (Show (CollectErrors es v)) where
  show (CollectErrors mv es) =
    case mv of
      Just v | es == mempty -> show v
      _ -> printf "{%s}" (show es)

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

{-| An unsafe way to get a value out of the CollectErrors wrapper. -}
getValueOrThrowErrorsCE ::
  (SuitableForCE es)
  =>
  (CollectErrors es v) -> v
getValueOrThrowErrorsCE vCE =
  getValueIfNoErrorCE vCE id (error . show)

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
class (Monoid es) => CanEnsureCE es a where
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
    EnsureCE es a -> Maybe a

  default deEnsureCE ::
    (EnsureCE es a ~ CollectErrors es a, Eq es) =>
    Maybe es {-^ sample only -} ->
    EnsureCE es a -> Maybe a
  deEnsureCE _ (CollectErrors mv es)
    | es == mempty = mv
    | otherwise = Nothing

  ensureNoCE ::
    Maybe es {-^ sample only -} ->
    a -> Maybe (EnsureNoCE es a)

  default ensureNoCE ::
    (EnsureNoCE es a ~ a, Eq es) =>
    Maybe es {-^ sample only -} ->
    a -> Maybe (EnsureNoCE es a)
  ensureNoCE _ = Just

  {-|  Make CollectErrors record with no value, only errors. -}
  noValueECE ::
    Maybe a {-^ sample only -} ->
    es -> EnsureCE es a

  default noValueECE ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe a ->
    es -> CollectErrors es a
  noValueECE _ = noValueCE

  getMaybeValueECE ::
    Maybe es {-^ sample only -} ->
    EnsureCE es a -> Maybe a

  default getMaybeValueECE ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe es ->
    EnsureCE es a -> Maybe a
  getMaybeValueECE _ = getMaybeValueCE

  getErrorsECE ::
    Maybe a {-^ sample only -} ->
    EnsureCE es a -> es

  default getErrorsECE ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe a ->
    EnsureCE es a -> es
  getErrorsECE _ = getErrorsCE

  {-|  Add further errors into an EnsureCE value. -}
  prependErrorsECE ::
    Maybe a {-^ sample only -} ->
    es -> EnsureCE es a -> EnsureCE es a
  default prependErrorsECE ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe a ->
    es -> EnsureCE es a -> EnsureCE es a
  prependErrorsECE _ = prependErrorsCE

-- instance for CollectErrors a:

instance
  (SuitableForCE es)
  =>
  CanEnsureCE es (CollectErrors es a)
  where
  type EnsureCE es (CollectErrors es a) = CollectErrors es a
  type EnsureNoCE es (CollectErrors es a) = a

  ensureCE _sample_es = id
  deEnsureCE _sample_es = Just
  ensureNoCE _sample_es (CollectErrors mv es)
    | es == mempty = mv
    | otherwise = Nothing

  noValueECE _sample_vCE es = CollectErrors Nothing es

  getMaybeValueECE _sample_se vCE = Just vCE
  getErrorsECE _sample_vCE (CollectErrors _ es) = es
  prependErrorsECE _sample_vCE es1 (CollectErrors mv es2) = CollectErrors mv $ es1 <> es2


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
  deEnsureCE _sample_es Nothing = Just Nothing
  ensureNoCE sample_es = fmap (ensureNoCE sample_es)

  noValueECE sample_vCE es = Just (noValueECE (fromJust sample_vCE) es)

  getMaybeValueECE sample_es = fmap (getMaybeValueECE sample_es)
  getErrorsECE sample_mv (Just v) = getErrorsECE (fromJust sample_mv) v
  getErrorsECE _sample_mv Nothing = mempty
  prependErrorsECE sample_vCE es1 = fmap (prependErrorsECE (fromJust sample_vCE) es1)

-- instance (Monoid es) => CanEnsureCE es [a] where
-- instance (Monoid es) => CanEnsureCE es (Either e a) where

{-| An unsafe way to get a value out of the EnsureEC wrapper. -}
getValueOrThrowErrorsECE ::
  (SuitableForCE es, CanEnsureCE es v)
  =>
  Maybe es {-^ sample only -} ->
  (EnsureCE es v) -> v
getValueOrThrowErrorsECE sample_es vCE =
  getValueIfNoErrorECE sample_es vCE id (error . show)

{-| A safe way to get a value out of the EnsureEC wrapper. -}
getValueIfNoErrorECE ::
  (SuitableForCE es, CanEnsureCE es v)
  =>
  Maybe es {-^ sample only -} ->
  EnsureCE es v -> (v -> t) -> (es -> t) -> t
getValueIfNoErrorECE sample_es vCE withValue withErrors =
  let mv = deEnsureCE sample_es vCE in
  case mv of
    Just v -> withValue v
    _ -> withErrors (getErrorsECE mv vCE)

filterValuesWithoutErrorECE ::
  (SuitableForCE es, CanEnsureCE es v)
  =>
  Maybe es {-^ sample only -} ->
  [EnsureCE es v] -> [v]
filterValuesWithoutErrorECE _ [] = []
filterValuesWithoutErrorECE sample_es (vCE : rest) =
  getValueIfNoErrorECE sample_es vCE (: restDone) (const restDone)
  where
  restDone = filterValuesWithoutErrorECE sample_es rest

{-|
  Add error collection support to an unary function whose
  result may already have collected errors.
-}
lift1CE ::
  (SuitableForCE es
  , CanEnsureCE es c)
  =>
  (a -> c) ->
  (CollectErrors es a) -> (EnsureCE es c)
lift1CE (fn :: a -> c) aCE =
  case (ensureNoCE sample_es aCE) of
    (Just a) -> ensureCE sample_es $ fn a
    _ -> noValueECE sample_c a_es
  where
  sample_es = Just a_es
  CollectErrors ma a_es = aCE
  sample_c = fn <$> ma

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
-}
lift2CE ::
  (SuitableForCE es
  , CanEnsureCE es c)
  =>
  (a -> b -> c) ->
  (CollectErrors es a) -> (CollectErrors es b) -> (EnsureCE es c)
lift2CE (fn :: a -> b -> c) aCE bCE =
  case (ensureNoCE sample_es aCE, ensureNoCE sample_es bCE) of
    (Just a, Just b) -> ensureCE sample_es $ fn a b
    _ -> noValueECE sample_c (a_es <> b_es)
  where
  sample_es = Just a_es
  CollectErrors ma a_es = aCE
  CollectErrors mb b_es = bCE
  sample_c = fn <$> ma <*> mb

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
  A version where the second operand is not lifted, only the first one.
-}
lift2TCE ::
  (SuitableForCE es
  , CanEnsureCE es c)
  =>
  (a -> b -> c) ->
  (CollectErrors es a) -> b -> (EnsureCE es c)
lift2TCE (fn :: a -> b -> c) aCE b =
  case (ensureNoCE sample_es aCE) of
    (Just a) -> ensureCE sample_es $ fn a b
    _ -> noValueECE sample_c a_es
  where
  sample_es = Just a_es
  CollectErrors ma a_es = aCE
  sample_c = fn <$> ma <*> (Just b)

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
  A version where the first operand is not lifted, only the second one.
-}
lift2TLCE ::
  (SuitableForCE es
  , CanEnsureCE es c)
  =>
  (a -> b -> c) ->
  a -> (CollectErrors es b) -> (EnsureCE es c)
lift2TLCE f = flip $ lift2TCE (flip f)

-- Templates for instances propagating CollectErrors through operations
--
-- makeInstanceForCollectErrors2 :: Name -> Q Type -> [Q Exp] -> Q Dec
-- makeInstanceForCollectErrors2 opClass resultType operations =
--   -- instanceD (return []) (conT opClass `appT` (conT ''CollectErrors `appT` (varT (mkName "es")))) []
--   [d|
--     instance
--       (Monoid es) =>
--       $(conT opClass `appT` (conT ''CollectErrors `appT` (varT (mkName "es"))))
--       where
--
--   |]
--
-- instance
--   (CanMinMaxAsymmetric a b
--   , CanEnsureCE es (MinMaxType a b)
--   , Monoid es)
--   =>
--   CanMinMaxAsymmetric (CollectErrors es a) (CollectErrors es  b)
--   where
--   type MinMaxType (CollectErrors es a) (CollectErrors es b) =
--     EnsureCE es (MinMaxType a b)
--   min = CN.lift2ensureCE min
--   max = CN.lift2ensureCE max

{-# LANGUAGE TemplateHaskell #-}
module Control.CollectErrors
(
-- * Monad for collecting errors in expressions
  CollectErrors(..)
  , SuitableForCE
, getConvertResult
-- * Tools for avoiding @CollectErrors(CollectErrors t)@ and putting CE inside containers
, CanEnsureCE(..)
, filterValuesWithoutError, getValueIfNoError, getValueOrThrowErrors
, unlift2first, unlift2second
, lift1, lift2
)
where

import Prelude
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
  CollectErrors (Maybe v) es
  deriving (Show)

type SuitableForCE es = (Monoid es, Eq es, Show es)

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

getConvertResult ::
  (Typeable t, Show t, SuitableForCE es)
  =>
  CollectErrors es t -> Either ConvertError t
getConvertResult (CollectErrors mv es) =
  case mv of
    Just v | es == mempty -> Right v
    _ -> convError (show es) mv


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
  noValue ::
    Maybe a {-^ sample only -} ->
    es -> EnsureCE es a

  default noValue ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe a ->
    es -> CollectErrors es a
  noValue _sample_v es = CollectErrors Nothing es

  getMaybeValue ::
    Maybe es {-^ sample only -} ->
    EnsureCE es a -> Maybe a

  default getMaybeValue ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe es ->
    EnsureCE es a -> Maybe a
  getMaybeValue _sample_es (CollectErrors mv _) = mv

  getErrors ::
    Maybe a {-^ sample only -} ->
    EnsureCE es a -> es

  default getErrors ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe a ->
    EnsureCE es a -> es
  getErrors _sample_v (CollectErrors _ es) = es

  {-|  Add further errors into an EnsureCE value. -}
  prependErrors ::
    Maybe a {-^ sample only -} ->
    es -> EnsureCE es a -> EnsureCE es a
  default prependErrors ::
    (EnsureCE es a ~ CollectErrors es a)
    =>
    Maybe a ->
    es -> EnsureCE es a -> EnsureCE es a
  prependErrors _sample_v es1 (CollectErrors mv es2) = CollectErrors mv (es1 <> es2)

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

  noValue _sample_vCE es = CollectErrors Nothing es

  getMaybeValue _sample_se vCE = Just vCE
  getErrors _sample_vCE (CollectErrors _ es) = es
  prependErrors _sample_vCE es1 (CollectErrors mv es2) = CollectErrors mv $ es1 <> es2


-- instances for ground types, using the default implementations:

instance (SuitableForCE es) => CanEnsureCE es Int
instance (SuitableForCE es) => CanEnsureCE es Integer
instance (SuitableForCE es) => CanEnsureCE es Rational
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

  noValue sample_vCE es = Just (noValue (fromJust sample_vCE) es)

  getMaybeValue sample_es = fmap (getMaybeValue sample_es)
  getErrors sample_mv (Just v) = getErrors (fromJust sample_mv) v
  getErrors _sample_mv Nothing = mempty
  prependErrors sample_vCE es1 = fmap (prependErrors (fromJust sample_vCE) es1)

-- instance (Monoid es) => CanEnsureCE es [a] where
--   ensureTypeOp = noErrors
-- instance (Monoid es) => CanEnsureCE es (Either e a) where
--   ensureTypeOp = noErrors

{-| An unsafe way to get a value out of the CollectErrors wrapper. -}
getValueOrThrowErrors ::
  (SuitableForCE es, CanEnsureCE es v)
  =>
  Maybe es {-^ sample only -} ->
  (EnsureCE es v) -> v
getValueOrThrowErrors sample_es vCE =
  getValueIfNoError sample_es vCE id (error . show)

{-| A safe way to get a value out of the CollectErrors wrapper. -}
getValueIfNoError ::
  (SuitableForCE es, CanEnsureCE es v)
  =>
  Maybe es {-^ sample only -} ->
  EnsureCE es v -> (v -> t) -> (es -> t) -> t
getValueIfNoError sample_es vCE withValue withErrors =
  let mv = deEnsureCE sample_es vCE in
  case mv of
    Just v -> withValue v
    _ -> withErrors (getErrors mv vCE)

filterValuesWithoutError ::
  (SuitableForCE es, CanEnsureCE es v)
  =>
  Maybe es {-^ sample only -} ->
  [EnsureCE es v] -> [v]
filterValuesWithoutError _ [] = []
filterValuesWithoutError sample_es (vCE : rest) =
  getValueIfNoError sample_es vCE (: restDone) (const restDone)
  where
  restDone = filterValuesWithoutError sample_es rest

{-|
  A utility function for easily defining a binary function with
  only the second parameter collecting errors, if we have an analogous
  function that requires both parameters to collect errors.
-}
unlift2first ::
  (Monoid es, CanEnsureCE es a)
  =>
  Maybe es {-^ sample only -} ->
  (EnsureCE es a -> b -> t) ->
  (a -> b -> t)
unlift2first sample_es op a = op (ensureCE sample_es a)

{-|
  An utility function for easily defining a binary function with
  only the first parameter collecting errors, if we have an analogous
  function that requires both parameters to collect errors.
-}
unlift2second ::
  (Monoid es, CanEnsureCE es b)
  =>
  Maybe es {-^ sample only -} ->
  (a -> EnsureCE es b -> t) ->
  (a -> b -> t)
unlift2second sample_es op a b = op a (ensureCE sample_es b)

{-|
  Add error collection support to an unary function whose
  result may already have collected errors.
-}
lift1 ::
  (SuitableForCE es
  , CanEnsureCE es a
  , CanEnsureCE es c)
  =>
  Maybe es {-^ sample only -} ->
  (a -> c) ->
  (CollectErrors es a) -> (EnsureCE es c)
lift1 (sample_es :: Maybe es) (fn :: a -> c) aCE =
  case (ensureNoCE sample_es aCE) of
    (Just a) -> ensureCE sample_es $ fn a
    _ -> noValue sample_c a_es
  where
  CollectErrors ma a_es = aCE
  sample_c = fn <$> ma

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
-}
lift2 ::
  (SuitableForCE es
  , CanEnsureCE es a
  , CanEnsureCE es b
  , CanEnsureCE es c)
  =>
  Maybe es {-^ sample only -} ->
  (a -> b -> c) ->
  (CollectErrors es a) -> (CollectErrors es b) -> (EnsureCE es c)
lift2 (sample_es :: Maybe es) (fn :: a -> b -> c) aCE bCE =
  case (ensureNoCE sample_es aCE, ensureNoCE sample_es bCE) of
    (Just a, Just b) -> ensureCE sample_es $ fn a b
    _ -> noValue sample_c (a_es <> b_es)
  where
  CollectErrors ma a_es = aCE
  CollectErrors mb b_es = bCE
  sample_c = fn <$> ma <*> mb

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

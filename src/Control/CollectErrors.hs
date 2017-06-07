{-# LANGUAGE TemplateHaskell #-}
module Control.CollectErrors
(
-- * Monad for collecting errors in expressions
  CollectErrors(..)
, noErrors, noValue, prependErrors
, filterValuesWithoutError, getValueIfNoError, getValueOrThrowErrors
, getConvertResult
, lift1, lift2
, unlift2first, unlift2second
, SuitableForCE
-- * Tools for avoiding @CollectErrors(CollectErrors t)@
, ensureCE, deEnsureCE, defaultDeEnsureTypeOp
, CanEnsureCE, EnsureCE, WithoutCE
, lift1ensureCE, lift2ensureCE
)
where

import Prelude
import Data.Monoid

import Data.Convertible
import Data.Typeable

-- import Language.Haskell.TH

import Test.QuickCheck

import Control.EnsureTypeOp

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
  {
    getMaybeValue :: (Maybe v)
  , getErrors :: es
  }
  deriving (Show)

-- utilities:

{-|  Wrap a pure value in a CollectErrors record -}
noErrors :: (Monoid es) => v -> CollectErrors es v
noErrors v = CollectErrors (Just v) mempty

{-|  Make a CollectErrors record with no value, only errors. -}
noValue :: es -> CollectErrors es v
noValue es = CollectErrors Nothing es

{-|  Add further errors into a CollectErrors record. -}
prependErrors :: (Monoid es) => es -> CollectErrors es v -> CollectErrors es v
prependErrors es1 (CollectErrors mv es2) = CollectErrors mv (es1 <> es2)

filterValuesWithoutError :: (Monoid es, Eq es) => [CollectErrors es v] -> [v]
filterValuesWithoutError [] = []
filterValuesWithoutError (vCN : rest) =
  getValueIfNoError vCN (: restDone) (const restDone)
  where
  restDone = filterValuesWithoutError rest

{-| A safe way to get a value out of the CollectErrors wrapper. -}
getValueIfNoError :: (Monoid es, Eq es) => CollectErrors es v -> (v -> t) -> (es -> t) -> t
getValueIfNoError ce withValue withErrors =
  case (getMaybeValue ce, getErrors ce) of
    (Just v, es) | es == mempty -> withValue v
    (_, es) -> withErrors es

{-| An unsafe way to get a value out of the CollectErrors wrapper. -}
getValueOrThrowErrors :: (Show es, Monoid es, Eq es) => (CollectErrors es v) -> v
getValueOrThrowErrors ce =
  getValueIfNoError ce id (error . show)

-- caseErrors :: [((CollectErrors es v) -> Bool, (CollectErrors es v) -> t)] -> t -> (CollectErrors es v) -> t
-- caseErrors cases defaultT ce = aux cases
--   where
--   aux [] = defaultT
--   aux ((cond, comp) : rest)
--     | cond ce = comp ce
--     | otherwise = aux rest

getConvertResult ::
  (Typeable t, Show t, Show es, Monoid es, Eq es)
  =>
  CollectErrors es t -> Either ConvertError t
getConvertResult vCN =
  getValueIfNoError vCN Right (\ es -> convError (show es) (getMaybeValue vCN))

{-|
  Add error collection support to an unary operation.
-}
lift1 ::
  (a -> b) ->
  (CollectErrors es a) -> (CollectErrors es b)
lift1 = fmap

{-|
  Add error collection support to a binary operation.
-}
lift2 ::
  (Monoid es) =>
  (a -> b -> c) ->
  (CollectErrors es a) -> (CollectErrors es b) -> (CollectErrors es c)
lift2 fn
    (CollectErrors (Just a) ae) (CollectErrors (Just b) be) =
        CollectErrors (Just (fn a b)) (ae <> be)
lift2 _
    (CollectErrors _ ae) (CollectErrors _ be) =
        CollectErrors Nothing (ae <> be)

{-|
  An utility function for easily defining a binary function with
  only the second parameter collecting errors, if we have an analogous
  function that requires both parameters to collect errors.
-}
unlift2first ::
  (Monoid es)
  =>
  ((CollectErrors es a) -> (CollectErrors es b) -> t)
  ->
  (a -> (CollectErrors es b) -> t)
unlift2first op (a :: a) (be :: CollectErrors es b) =
  op (noErrors a :: CollectErrors es a) be

{-|
  An utility function for easily defining a binary function with
  only the first parameter collecting errors, if we have an analogous
  function that requires both parameters to collect errors.
-}
unlift2second ::
  (Monoid es)
  =>
  ((CollectErrors es a) -> (CollectErrors es b) -> t)
  ->
  ((CollectErrors es a) -> b -> t)
unlift2second op (ae :: CollectErrors es a) (b :: b)  =
  op ae (noErrors b :: CollectErrors es b)

-- functor instances:

instance Functor (CollectErrors es) where
  fmap f (CollectErrors mv es) =
    CollectErrors (fmap f mv) es

instance (Monoid es) => Applicative (CollectErrors es) where
  pure = noErrors
  (<*>) = lift2 ($)

instance (Monoid es) => Monad (CollectErrors es) where
  ae >>= f =
    case ae of
      CollectErrors (Just a) es ->
        prependErrors es (f a)
      CollectErrors _ es ->
        CollectErrors Nothing es

instance (Arbitrary t, Monoid es) => Arbitrary (CollectErrors es t) where
  arbitrary = noErrors <$> arbitrary

type EnsureCE es v = EnsureTypeOp (CollectErrors es) v
{-|
Apply CollectErrors to a type except when the type already
is a CollectErrors type.
-}

{-|
  Remove CollectErrors wrapper from a type if it is there.
-}
type WithoutCE es v = RemoveTypeOp (CollectErrors es) v

{-|
  Translate a value of a type @a@
  to a value of a type @CollectErrors es a@ except when @a@
  already is a @CollectErrors@ type, in which case the value is left as is.
-}
ensureCE :: (CanEnsureCE es v) => v -> EnsureCE es v
ensureCE = ensureTypeOp

{-|
  Translate a value of a type @EnsureCE es a@ to @a@,
  throwing an exception if there was an error.
  If @a@ is a @CollectErrors@ type, then this is just an identity.
-}
deEnsureCE :: (CanEnsureCE es v) => EnsureCE es v -> v
deEnsureCE = deEnsureTypeOp

type CanEnsureCE es v = CanEnsureTypeOp (CollectErrors es) v

type SuitableForCE es = (Monoid es, Show es, Eq es)

defaultDeEnsureTypeOp :: (SuitableForCE es) => CollectErrors es t -> t
defaultDeEnsureTypeOp vCE = getValueIfNoError vCE id (error . show)

instance (SuitableForCE es) => CanEnsureTypeOp (CollectErrors es) Int where
  ensureTypeOp = noErrors
  deEnsureTypeOp = defaultDeEnsureTypeOp
instance (SuitableForCE es) => CanEnsureTypeOp (CollectErrors es) Integer where
  ensureTypeOp = noErrors
  deEnsureTypeOp = defaultDeEnsureTypeOp
instance (SuitableForCE es) => CanEnsureTypeOp (CollectErrors es) Rational where
  ensureTypeOp = noErrors
  deEnsureTypeOp = defaultDeEnsureTypeOp
instance (SuitableForCE es) => CanEnsureTypeOp (CollectErrors es) Char where
  ensureTypeOp = noErrors
  deEnsureTypeOp = defaultDeEnsureTypeOp
instance (SuitableForCE es) => CanEnsureTypeOp (CollectErrors es) Bool where
  ensureTypeOp = noErrors
  deEnsureTypeOp = defaultDeEnsureTypeOp
instance (SuitableForCE es) => CanEnsureTypeOp (CollectErrors es) [a] where
  ensureTypeOp = noErrors
  deEnsureTypeOp = defaultDeEnsureTypeOp
instance (SuitableForCE es) => CanEnsureTypeOp (CollectErrors es) (Maybe a) where
  ensureTypeOp = noErrors
  deEnsureTypeOp = defaultDeEnsureTypeOp
instance (SuitableForCE es) => CanEnsureTypeOp (CollectErrors es) (Either e a) where
  ensureTypeOp = noErrors
  deEnsureTypeOp = defaultDeEnsureTypeOp

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
-}
lift1ensureCE ::
  (Monoid es, CanEnsureCE es b) =>
  (a -> b) ->
  (CollectErrors es a) -> (EnsureCE es b)
lift1ensureCE fn
    (CollectErrors (Just a) ae) =
        prependErrors ae (ensureCE $ fn a)
lift1ensureCE _
    (CollectErrors _ ae) =
        CollectErrors Nothing ae

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
-}
lift2ensureCE ::
  (Monoid es, CanEnsureCE es c) =>
  (a -> b -> c) ->
  (CollectErrors es a) -> (CollectErrors es b) -> (EnsureCE es c)
lift2ensureCE fn
    (CollectErrors (Just a) ae) (CollectErrors (Just b) be) =
        prependErrors (ae <> be) (ensureCE $ fn a b)
lift2ensureCE _
    (CollectErrors _ ae) (CollectErrors _ be) =
        CollectErrors Nothing (ae <> be)

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

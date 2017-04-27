module Control.CollectErrors
(
  CollectErrors(..)
, noErrors, noValue, prependErrors, ifError
, lift1, lift2
, unlift2first, unlift2second
, ensureCollectErrors
, CanEnsureCollectErrors, EnsureCollectErrors
, lift1ensureCE, lift2ensureCE
)
where

import Prelude
import Data.Monoid

import Control.EnsureTypeOp

{-|
  A wrapper around values which can accommodate a list of
  (potential) errors that have (maybe) occurred during the computation
  of a value.  A value may be missing, leaving only the error(s).

  Such error collectoin allows one to write expressions with partial
  functions (ie functions that fail for some inputs) instead of
  branching after each application of such function.
  Dealing with the errors can be moved outside the expression.
  If the error data contain enough information, their list can be used
  to trace the source of the errors.
-}
data CollectErrors es v =
  CollectErrors (Maybe v) es
  deriving (Show)

-- utilities:

{-|  Wrap a pure value in a CollectErrors record -}
noErrors :: (Monoid es) => v -> CollectErrors es v
noErrors v = CollectErrors (Just v) mempty

{-|  Make CollectErrors record with no value, only errors. -}
noValue :: es -> CollectErrors es v
noValue es = CollectErrors Nothing es

{-|  Add further errors into a CollectErrors record. -}
prependErrors :: (Monoid es) => es -> CollectErrors es v -> CollectErrors es v
prependErrors es1 (CollectErrors mv es2) = CollectErrors mv (es1 <> es2)

ifError :: (Monoid es, Eq es) => (CollectErrors es v) -> (CollectErrors es v -> t) -> (v -> t) -> t
ifError ce@(CollectErrors mv es) onError onValue =
  case mv of
    Just v | mempty == es -> onValue v
    _ -> onError ce

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


{-|
  Apply CollectErrors to a type except when the type already
  is a CollectErrors type.
-}
type EnsureCollectErrors es v = EnsureTypeOp (CollectErrors es) v

{-|
  Translate a value of a type @a@
  to a value of a type @CollectErrors es a@ except when @a@
  already is a @CollectErrors@ type, in which case the value is left as is.
-}
ensureCollectErrors :: (CanEnsureCollectErrors es v) => v -> EnsureCollectErrors es v
ensureCollectErrors = ensureTypeOp

type CanEnsureCollectErrors es v = CanEnsureTypeOp (CollectErrors es) v

instance (Monoid es) => CanEnsureTypeOp (CollectErrors es) Int where
  ensureTypeOp = noErrors
instance (Monoid es) => CanEnsureTypeOp (CollectErrors es) Integer where
  ensureTypeOp = noErrors
instance (Monoid es) => CanEnsureTypeOp (CollectErrors es) Rational where
  ensureTypeOp = noErrors
instance (Monoid es) => CanEnsureTypeOp (CollectErrors es) Char where
  ensureTypeOp = noErrors
instance (Monoid es) => CanEnsureTypeOp (CollectErrors es) Bool where
  ensureTypeOp = noErrors
instance (Monoid es) => CanEnsureTypeOp (CollectErrors es) [a] where
  ensureTypeOp = noErrors
instance (Monoid es) => CanEnsureTypeOp (CollectErrors es) (Maybe a) where
  ensureTypeOp = noErrors
instance (Monoid es) => CanEnsureTypeOp (CollectErrors es) (Either e a) where
  ensureTypeOp = noErrors

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
-}
lift1ensureCE ::
  (Monoid es, CanEnsureCollectErrors es b) =>
  (a -> b) ->
  (CollectErrors es a) -> (EnsureCollectErrors es b)
lift1ensureCE fn
    (CollectErrors (Just a) ae) =
        prependErrors ae (ensureCollectErrors $ fn a)
lift1ensureCE _
    (CollectErrors _ ae) =
        CollectErrors Nothing ae

{-|
  Add error collection support to a binary function whose
  result may already have collected errors.
-}
lift2ensureCE ::
  (Monoid es, CanEnsureCollectErrors es c) =>
  (a -> b -> c) ->
  (CollectErrors es a) -> (CollectErrors es b) -> (EnsureCollectErrors es c)
lift2ensureCE fn
    (CollectErrors (Just a) ae) (CollectErrors (Just b) be) =
        prependErrors (ae <> be) (ensureCollectErrors $ fn a b)
lift2ensureCE _
    (CollectErrors _ ae) (CollectErrors _ be) =
        CollectErrors Nothing (ae <> be)

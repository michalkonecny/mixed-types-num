module Control.CollectErrors where

import Prelude
import Data.Monoid

import Control.EnsureTypeOp

data CollectErrors es v =
  CollectErrors (Maybe v) es
  deriving (Show)

-- utilities:

noErrors :: (Monoid es) => v -> CollectErrors es v
noErrors v = CollectErrors (Just v) mempty

prependErrors :: (Monoid es) => es -> CollectErrors es v -> CollectErrors es v
prependErrors es1 (CollectErrors mv es2) = CollectErrors mv (es1 <> es2)

firstNoErrors ::
  (Monoid es)
  =>
  ((CollectErrors es a) -> (CollectErrors es b) -> t)
  ->
  (a -> (CollectErrors es b) -> t)
firstNoErrors op (a :: a) (be :: CollectErrors es b) =
  op (noErrors a :: CollectErrors es a) be

secondNoErrors ::
  (Monoid es)
  =>
  ((CollectErrors es a) -> (CollectErrors es b) -> t)
  ->
  ((CollectErrors es a) -> b -> t)
secondNoErrors op (ae :: CollectErrors es a) (b :: b)  =
  op ae (noErrors b :: CollectErrors es b)

-- functor instances:

instance Functor (CollectErrors es) where
  fmap f (CollectErrors mv es) =
    CollectErrors (fmap f mv) es

instance (Monoid es) => Applicative (CollectErrors es) where
  pure = noErrors
  (<*>) = zipCollectErrorsWith ($)

zipCollectErrorsWith ::
  (Monoid es) =>
  (a -> b -> c) ->
  (CollectErrors es a) -> (CollectErrors es b) -> (CollectErrors es c)
zipCollectErrorsWith fn
    (CollectErrors (Just a) ae) (CollectErrors (Just b) be) =
        CollectErrors (Just (fn a b)) (ae <> be)
zipCollectErrorsWith _
    (CollectErrors _ ae) (CollectErrors _ be) =
        CollectErrors Nothing (ae <> be)

instance (Monoid es) => Monad (CollectErrors es) where
  ae >>= f =
    case ae of
      CollectErrors (Just a) es ->
        prependErrors es (f a)
      CollectErrors _ es ->
        CollectErrors Nothing es

type CanEnsureCollectErrors es v = CanEnsureTypeOp (CollectErrors es) v
type EnsureCollectErrors es v = EnsureTypeOp (CollectErrors es) v

ensureCollectErrors :: (CanEnsureCollectErrors es v) => v -> EnsureCollectErrors es v
ensureCollectErrors = ensureTypeOp

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

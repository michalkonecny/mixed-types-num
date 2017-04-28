module Control.EnsureTypeOp where

import Prelude

{-|
  A mechanism for applying a type operator
  to a type except when the type already
  has that operator.
-}
type EnsureTypeOp f a = f (RemoveTypeOp f a)

type family RemoveTypeOp f a where
  RemoveTypeOp f (f a) = a
  RemoveTypeOp f a = a

{-|
  A mechanism for translating a value of a type @a@
  to a value of a type @f a@ except when @a = f b@
  for some @b@, in which case the value is left as is.
-}
class
  CanEnsureTypeOp f a where
  ensureTypeOp :: a -> EnsureTypeOp f a

instance CanEnsureTypeOp f (f a) where
  ensureTypeOp = id

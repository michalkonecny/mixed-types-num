-- {-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Control.WithExceptions where

import Numeric.MixedTypes

{- Exception-bearing machinery -}

type Exceptions = [String]

data WithExceptions v =
  WithExceptions (Maybe v) Exceptions
  deriving (Show)

class HasExceptions ve where
  type HasExceptionsValue ve
  getExceptions :: ve -> Exceptions
  getMaybeValue :: ve -> Maybe (HasExceptionsValue ve)
  pureValue :: (HasExceptionsValue ve) -> ve
  noValue :: Exceptions -> ve
  addExceptions :: Exceptions -> ve -> ve

instance HasExceptions (WithExceptions v) where
  type HasExceptionsValue (WithExceptions v) = v
  getExceptions (WithExceptions _ es) = es
  getMaybeValue (WithExceptions mv _) = mv
  pureValue v = WithExceptions (Just v) []
  noValue es = WithExceptions Nothing es
  addExceptions es2 (WithExceptions mv es1) = WithExceptions mv (es1 ++ es2)

class (HasExceptions (EnsureHasExceptions v)) => CanEnsureExceptions v where
  type EnsureHasExceptions v
  ensureHasExceptions :: v -> EnsureHasExceptions v

instance CanEnsureExceptions (WithExceptions v) where
  type EnsureHasExceptions (WithExceptions v) = WithExceptions v
  ensureHasExceptions we = we

instance CanEnsureExceptions Rational where
  type EnsureHasExceptions Rational = WithExceptions Rational
  ensureHasExceptions r = WithExceptions (Just r) []

-- class CanMyMul a b where
--   type MyMulType a b
--   myMul :: a -> b -> MyMulType a b
--
-- (*!) :: (CanMyMul a b) => a -> b -> MyMulType a b
-- a *! b = myMul a b
--
-- instance CanMyMul Rational Rational where
--   type MyMulType Rational Rational = Rational
--   myMul = (*)

class CanMyDiv a b where
  type MyDivType a b
  myDiv :: a -> b -> MyDivType a b

instance CanMyDiv Rational Rational where
  type MyDivType Rational Rational = WithExceptions Rational
  myDiv a b
    | b == 0 = WithExceptions Nothing ["division by zero"]
    | otherwise = WithExceptions (Just (a/b)) []

(/!) :: (CanMyDiv a b) => a -> b -> MyDivType a b
a /! b = myDiv a b

instance
  (CanMyDiv a b,
  CanEnsureExceptions (MyDivType a b))
  =>
  CanMyDiv (WithExceptions a) (WithExceptions b)
  where
  type MyDivType (WithExceptions a) (WithExceptions b) = EnsureHasExceptions (MyDivType a b)
  myDiv (WithExceptions ma ae) (WithExceptions mb be) =
    case (ma, mb) of
      (Just a, Just b) -> addExceptions (ae ++ be) $ ensureHasExceptions (myDiv a b)
      _ -> noValue (ae ++ be)

instance
  (CanMyDiv Rational b,
  CanEnsureExceptions (MyDivType Rational b))
  =>
  CanMyDiv Rational (WithExceptions b)
  where
  type MyDivType Rational (WithExceptions b) = EnsureHasExceptions (MyDivType Rational b)
  myDiv a (WithExceptions mb be) =
    case (mb) of
      (Just b) -> addExceptions (be) $ ensureHasExceptions (myDiv a b)
      _ -> noValue (be)

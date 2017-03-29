module Numeric.MixedTypes.Concept.WithExceptions where

import Numeric.MixedTypes

import Data.Monoid

{- Exception-bearing machinery -}

data WithExceptions e v =
  WithExceptions (Maybe v) e
  deriving (Show)

-- utilities:

noExceptions :: (Monoid e) => v -> WithExceptions e v
noExceptions v = WithExceptions (Just v) mempty

prependExceptions :: (Monoid e) => e -> WithExceptions e v -> WithExceptions e v
prependExceptions es1 (WithExceptions mv es2) = WithExceptions mv (es1 <> es2)

firstNoExceptions ::
  (Monoid e)
  =>
  ((WithExceptions e a) -> (WithExceptions e b) -> t)
  ->
  (a -> (WithExceptions e b) -> t)
firstNoExceptions op (a :: a) (be :: WithExceptions e b) =
  op (noExceptions a :: WithExceptions e a) be

secondNoExceptions ::
  (Monoid e)
  =>
  ((WithExceptions e a) -> (WithExceptions e b) -> t)
  ->
  ((WithExceptions e a) -> b -> t)
secondNoExceptions op (ae :: WithExceptions e a) (b :: b)  =
  op ae (noExceptions b :: WithExceptions e b)

-- functor instances:

instance Functor (WithExceptions e) where
  fmap f (WithExceptions mv es) =
    WithExceptions (fmap f mv) es

instance (Monoid e) => Applicative (WithExceptions e) where
  pure = noExceptions
  (<*>) = zipExceptionsWith ($)

zipExceptionsWith ::
  (Monoid e) =>
  (a -> b -> c) ->
  (WithExceptions e a) -> (WithExceptions e b) -> (WithExceptions e c)
zipExceptionsWith fn
    (WithExceptions (Just a) ae) (WithExceptions (Just b) be) =
        WithExceptions (Just (fn a b)) (ae <> be)
zipExceptionsWith _
    (WithExceptions _ ae) (WithExceptions _ be) =
        WithExceptions Nothing (ae <> be)

instance (Monoid e) => Monad (WithExceptions e) where
  ae >>= f =
    case ae of
      WithExceptions (Just a) es ->
        prependExceptions es (f a)
      WithExceptions _ es ->
        WithExceptions Nothing es

-- idempotent functor:

class IdempotentFunctor f a where
  ensureFunctor :: a -> f (FunctorValue f a)

type family FunctorValue f a where
  FunctorValue f (f a) = a
  FunctorValue f a = a

instance IdempotentFunctor (WithExceptions e) (WithExceptions e a) where
  ensureFunctor = id

instance (Monoid e) => IdempotentFunctor (WithExceptions e) Rational where
  ensureFunctor = noExceptions

type CanEnsureExceptions e v = IdempotentFunctor (WithExceptions e) v
type WithExceptionsValue e v = FunctorValue (WithExceptions e) v
type EnsureWithExceptions e v = WithExceptions e (WithExceptionsValue e v)

ensureWithExceptions :: (CanEnsureExceptions e v) => v -> EnsureWithExceptions e v
ensureWithExceptions = ensureFunctor

{- Numeric exceptions -}

type NumExceptions = [(ExceptionCertaintyLevel, NumException)]

data ExceptionCertaintyLevel =
  ExceptionCertain | ExceptionPotential
    deriving (Show)

data NumException =
    DivByZero | OutOfRange String | NumericalException String
    deriving (Show)

type WithNumExceptions v = WithExceptions NumExceptions v
type CanEnsureNumExceptions v = CanEnsureExceptions NumExceptions v

noNumExceptions :: v -> WithNumExceptions v
noNumExceptions = noExceptions

{- division with exception handling -}

class CanMyDiv a b where
  type MyDivType a b
  myDiv :: a -> b -> MyDivType a b

instance CanMyDiv Rational Rational where
  type MyDivType Rational Rational = WithNumExceptions Rational
  myDiv a b
    | b == 0 = WithExceptions Nothing [(ExceptionCertain, DivByZero)]
    | otherwise = noNumExceptions (a/b)

(/!) :: (CanMyDiv a b) => a -> b -> MyDivType a b
a /! b = myDiv a b

instance
  (CanMyDiv a b,
   Monoid e,
   CanEnsureExceptions e (MyDivType a b))
  =>
  CanMyDiv (WithExceptions e a) (WithExceptions e b)
  where
  type MyDivType (WithExceptions e a) (WithExceptions e b) = EnsureWithExceptions e (MyDivType a b)
  myDiv (WithExceptions ma ae) (WithExceptions mb be) =
    case (ma, mb) of
      (Just a, Just b) -> prependExceptions (ae <> be) (ensureWithExceptions (myDiv a b))
      _ -> WithExceptions Nothing (ae <> be)

instance
  (CanMyDiv Rational b,
   Monoid e,
   CanEnsureExceptions e (MyDivType Rational b))
  =>
  CanMyDiv Rational (WithExceptions e b)
  where
  type MyDivType Rational (WithExceptions e b) = EnsureWithExceptions e (MyDivType Rational b)
  myDiv = firstNoExceptions myDiv

instance
  (CanMyDiv a Rational,
   Monoid e,
   CanEnsureExceptions e (MyDivType a Rational))
  =>
  CanMyDiv (WithExceptions e a) Rational
  where
  type MyDivType (WithExceptions e a) Rational = EnsureWithExceptions e (MyDivType a Rational)
  myDiv = secondNoExceptions myDiv

withExceptionsExample1 :: WithNumExceptions Rational
withExceptionsExample1 = 1.0 /! 1.0

withExceptionsExample2 :: WithNumExceptions Rational
withExceptionsExample2 = 1.0 /! (1.0 /! 1.0)

withExceptionsExample3 :: WithNumExceptions Rational
withExceptionsExample3 = (1.0 /! 1.0) /! (1.0 /! 1.0)

{- multiplication with exception propagation -}

class CanMyMul a b where
  type MyMulType a b
  myMul :: a -> b -> MyMulType a b

(*!) :: (CanMyMul a b) => a -> b -> MyMulType a b
a *! b = myMul a b

instance CanMyMul Rational Rational where
  type MyMulType Rational Rational = Rational
  myMul = (*)

instance
  (CanMyMul a b,
   Monoid e,
   CanEnsureExceptions e (MyMulType a b))
  =>
  CanMyMul (WithExceptions e a) (WithExceptions e b)
  where
  type MyMulType (WithExceptions e a) (WithExceptions e b) = EnsureWithExceptions e (MyMulType a b)
  myMul (WithExceptions ma ae) (WithExceptions mb be) =
    case (ma, mb) of
      (Just a, Just b) -> prependExceptions (ae <> be) (ensureWithExceptions (myMul a b))
      _ -> WithExceptions Nothing (ae <> be)

instance
  (CanMyMul Rational b,
   Monoid e,
   CanEnsureExceptions e (MyMulType Rational b))
  =>
  CanMyMul Rational (WithExceptions e b)
  where
  type MyMulType Rational (WithExceptions e b) = EnsureWithExceptions e (MyMulType Rational b)
  myMul = firstNoExceptions myMul

instance
  (CanMyMul a Rational,
   Monoid e,
   CanEnsureExceptions e (MyMulType a Rational))
  =>
  CanMyMul (WithExceptions e a) Rational
  where
  type MyMulType (WithExceptions e a) Rational = EnsureWithExceptions e (MyMulType a Rational)
  myMul = secondNoExceptions myMul

withExceptionsExample4 :: WithNumExceptions Rational
withExceptionsExample4 = ((1.0 *! 1.0) /! 1.0) *! (1.0 *! (1.0 /! 1.0))

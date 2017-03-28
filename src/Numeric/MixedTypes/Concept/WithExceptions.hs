module Numeric.MixedTypes.Concept.WithExceptions where

import Numeric.MixedTypes

import Data.Monoid

{- Exception-bearing machinery -}

data WithExceptions v e =
  WithExceptions (Maybe v) e
  deriving (Show)

noExceptions :: (Monoid e) => v -> WithExceptions v e
noExceptions v = WithExceptions (Just v) mempty

prependExceptions :: (Monoid e) => e -> WithExceptions v e -> WithExceptions v e
prependExceptions es1 (WithExceptions mv es2) = WithExceptions mv (es1 <> es2)

firstNoExceptions ::
  (Monoid e)
  =>
  ((WithExceptions a e) -> (WithExceptions b e) -> t)
  ->
  (a -> (WithExceptions b e) -> t)
firstNoExceptions op (a :: a) (be :: WithExceptions b e) =
  op (noExceptions a :: WithExceptions a e) be

secondNoExceptions ::
  (Monoid e)
  =>
  ((WithExceptions a e) -> (WithExceptions b e) -> t)
  ->
  ((WithExceptions a e) -> b -> t)
secondNoExceptions op (ae :: WithExceptions a e) (b :: b)  =
  op ae (noExceptions b :: WithExceptions b e)

class CanEnsureExceptions v e where
  type Value v
  ensureWithExceptions :: v -> WithExceptions (Value v) e

type EnsureWithExceptions v e = WithExceptions (Value v) e

instance CanEnsureExceptions (WithExceptions v e) e where
  type Value (WithExceptions v e) = v
  ensureWithExceptions we = we

instance (Monoid e) => CanEnsureExceptions Rational e where
  type Value Rational = Rational
  ensureWithExceptions r = noExceptions r

{- Numeric exceptions -}

type NumExceptions = [(ExceptionCertaintyLevel, NumException)]

data ExceptionCertaintyLevel =
  ExceptionCertain | ExceptionPotential
    deriving (Show)

data NumException =
    DivByZero | OutOfRange String | NumericalException String
    deriving (Show)

{- division with exception handling -}

class CanMyDiv a b where
  type MyDivType a b
  myDiv :: a -> b -> MyDivType a b

instance CanMyDiv Rational Rational where
  type MyDivType Rational Rational = WithExceptions Rational NumExceptions
  myDiv a b
    | b == 0 = WithExceptions Nothing [(ExceptionCertain, DivByZero)]
    | otherwise = noExceptions (a/b)

(/!) :: (CanMyDiv a b) => a -> b -> MyDivType a b
a /! b = myDiv a b

instance
  (CanMyDiv a b,
   Monoid e,
   CanEnsureExceptions (MyDivType a b) e)
  =>
  CanMyDiv (WithExceptions a e) (WithExceptions b e)
  where
  type MyDivType (WithExceptions a e) (WithExceptions b e) = EnsureWithExceptions (MyDivType a b) e
  myDiv (WithExceptions ma ae) (WithExceptions mb be) =
    case (ma, mb) of
      (Just a, Just b) -> prependExceptions (ae <> be) (ensureWithExceptions (myDiv a b))
      _ -> WithExceptions Nothing (ae <> be)

instance
  (CanMyDiv Rational b,
   Monoid e,
   CanEnsureExceptions (MyDivType Rational b) e)
  =>
  CanMyDiv Rational (WithExceptions b e)
  where
  type MyDivType Rational (WithExceptions b e) = EnsureWithExceptions (MyDivType Rational b) e
  myDiv = firstNoExceptions myDiv

instance
  (CanMyDiv a Rational,
   Monoid e,
   CanEnsureExceptions (MyDivType a Rational) e)
  =>
  CanMyDiv (WithExceptions a e) Rational
  where
  type MyDivType (WithExceptions a e) Rational = EnsureWithExceptions (MyDivType a Rational) e
  myDiv = secondNoExceptions myDiv

withExceptionsExample1 :: WithExceptions Rational NumExceptions
withExceptionsExample1 = 1.0 /! 1.0

withExceptionsExample2 :: WithExceptions Rational NumExceptions
withExceptionsExample2 = 1.0 /! (1.0 /! 1.0)

withExceptionsExample3 :: WithExceptions Rational NumExceptions
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
   CanEnsureExceptions (MyMulType a b) e)
  =>
  CanMyMul (WithExceptions a e) (WithExceptions b e)
  where
  type MyMulType (WithExceptions a e) (WithExceptions b e) = EnsureWithExceptions (MyMulType a b) e
  myMul (WithExceptions ma ae) (WithExceptions mb be) =
    case (ma, mb) of
      (Just a, Just b) -> prependExceptions (ae <> be) (ensureWithExceptions (myMul a b))
      _ -> WithExceptions Nothing (ae <> be)

instance
  (CanMyMul Rational b,
   Monoid e,
   CanEnsureExceptions (MyMulType Rational b) e)
  =>
  CanMyMul Rational (WithExceptions b e)
  where
  type MyMulType Rational (WithExceptions b e) = EnsureWithExceptions (MyMulType Rational b) e
  myMul = firstNoExceptions myMul

instance
  (CanMyMul a Rational,
   Monoid e,
   CanEnsureExceptions (MyMulType a Rational) e)
  =>
  CanMyMul (WithExceptions a e) Rational
  where
  type MyMulType (WithExceptions a e) Rational = EnsureWithExceptions (MyMulType a Rational) e
  myMul = secondNoExceptions myMul

withExceptionsExample4 :: WithExceptions Rational NumExceptions
withExceptionsExample4 = ((1.0 *! 1.0) /! 1.0) *! (1.0 *! (1.0 /! 1.0))

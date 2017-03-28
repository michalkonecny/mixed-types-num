-- {-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Control.WithExceptions where

import Numeric.MixedTypes

{- Exception-bearing machinery -}

type Exceptions = [String]

data WithExceptions v =
  WithExceptions (Maybe v) Exceptions
  deriving (Show)

noExceptions :: v -> WithExceptions v
noExceptions v = WithExceptions (Just v) []

prependExceptions :: Exceptions -> WithExceptions v -> WithExceptions v
prependExceptions es1 (WithExceptions mv es2) = WithExceptions mv (es1 ++ es2)

class CanEnsureExceptions v where
  type Value v
  ensureWithExceptions :: v -> WithExceptions (Value v)

type EnsureWithExceptions v = WithExceptions (Value v)

instance CanEnsureExceptions (WithExceptions v) where
  type Value (WithExceptions v) = v
  ensureWithExceptions we = we

instance CanEnsureExceptions Rational where
  type Value Rational = Rational
  ensureWithExceptions r = WithExceptions (Just r) []

{- division with exception handling -}

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
  type MyDivType (WithExceptions a) (WithExceptions b) = EnsureWithExceptions (MyDivType a b)
  myDiv (WithExceptions ma ae) (WithExceptions mb be) =
    case (ma, mb) of
      (Just a, Just b) -> prependExceptions (ae ++ be) (ensureWithExceptions (myDiv a b))
      _ -> WithExceptions Nothing (ae ++ be)

instance
  (CanMyDiv Rational b,
  CanEnsureExceptions (MyDivType Rational b))
  =>
  CanMyDiv Rational (WithExceptions b)
  where
  type MyDivType Rational (WithExceptions b) = EnsureWithExceptions (MyDivType Rational b)
  myDiv r b = myDiv (noExceptions r) b

instance
  (CanMyDiv a Rational,
  CanEnsureExceptions (MyDivType a Rational))
  =>
  CanMyDiv (WithExceptions a) Rational
  where
  type MyDivType (WithExceptions a) Rational = EnsureWithExceptions (MyDivType a Rational)
  myDiv a r = myDiv a (noExceptions r)

withExceptionsExample1 :: WithExceptions Rational
withExceptionsExample1 = 1.0 /! 1.0

withExceptionsExample2 :: WithExceptions Rational
withExceptionsExample2 = 1.0 /! (1.0 /! 1.0)

withExceptionsExample3 :: WithExceptions Rational
withExceptionsExample3 = (1.0 /! 1.0) /! (1.0 /! 1.0)

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

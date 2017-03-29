module Numeric.MixedTypes.Concept.CollectErrors where

import Numeric.MixedTypes

import Data.Monoid

import Control.CollectErrors

{- Numeric exceptions -}

type NumErrors = [(ErrorCertaintyLevel, NumError)]

data ErrorCertaintyLevel =
  ErrorCertain | ErrorPotential
    deriving (Show)

data NumError =
    DivByZero | OutOfRange String | NumError String
    deriving (Show)

type CollectNumErrors v = CollectErrors NumErrors v
type CanEnsureCollectNumErrors v = CanEnsureCollectErrors NumErrors v

noNumErrors :: v -> CollectNumErrors v
noNumErrors = noErrors

{- division with exception handling -}

class CanMyDiv a b where
  type MyDivType a b
  myDiv :: a -> b -> MyDivType a b

instance CanMyDiv Rational Rational where
  type MyDivType Rational Rational = CollectNumErrors Rational
  myDiv a b
    | b == 0 = CollectErrors Nothing [(ErrorCertain, DivByZero)]
    | otherwise = noNumErrors (a/b)

(/!) :: (CanMyDiv a b) => a -> b -> MyDivType a b
a /! b = myDiv a b

instance
  (CanMyDiv a b,
   Monoid es,
   CanEnsureCollectErrors es (MyDivType a b))
  =>
  CanMyDiv (CollectErrors es a) (CollectErrors es b)
  where
  type MyDivType (CollectErrors es a) (CollectErrors es b) = EnsureCollectErrors es (MyDivType a b)
  myDiv (CollectErrors ma ae) (CollectErrors mb be) =
    case (ma, mb) of
      (Just a, Just b) -> prependErrors (ae <> be) (ensureCollectErrors (myDiv a b))
      _ -> CollectErrors Nothing (ae <> be)

instance
  (CanMyDiv Rational b,
   Monoid es,
   CanEnsureCollectErrors es (MyDivType Rational b))
  =>
  CanMyDiv Rational (CollectErrors es b)
  where
  type MyDivType Rational (CollectErrors es b) = EnsureCollectErrors es (MyDivType Rational b)
  myDiv = firstNoErrors myDiv

instance
  (CanMyDiv a Rational,
   Monoid es,
   CanEnsureCollectErrors es (MyDivType a Rational))
  =>
  CanMyDiv (CollectErrors es a) Rational
  where
  type MyDivType (CollectErrors es a) Rational = EnsureCollectErrors es (MyDivType a Rational)
  myDiv = secondNoErrors myDiv

withErrorsExample1 :: CollectNumErrors Rational
withErrorsExample1 = 1.0 /! 1.0

withErrorsExample2 :: CollectNumErrors Rational
withErrorsExample2 = 1.0 /! (1.0 /! 1.0)

withErrorsExample3 :: CollectNumErrors Rational
withErrorsExample3 = (1.0 /! 1.0) /! (1.0 /! 1.0)

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
   Monoid es,
   CanEnsureCollectErrors es (MyMulType a b))
  =>
  CanMyMul (CollectErrors es a) (CollectErrors es b)
  where
  type MyMulType (CollectErrors es a) (CollectErrors es b) = EnsureCollectErrors es (MyMulType a b)
  myMul (CollectErrors ma ae) (CollectErrors mb be) =
    case (ma, mb) of
      (Just a, Just b) -> prependErrors (ae <> be) (ensureCollectErrors (myMul a b))
      _ -> CollectErrors Nothing (ae <> be)

instance
  (CanMyMul Rational b,
   Monoid es,
   CanEnsureCollectErrors es (MyMulType Rational b))
  =>
  CanMyMul Rational (CollectErrors es b)
  where
  type MyMulType Rational (CollectErrors es b) = EnsureCollectErrors es (MyMulType Rational b)
  myMul = firstNoErrors myMul

instance
  (CanMyMul a Rational,
   Monoid es,
   CanEnsureCollectErrors es (MyMulType a Rational))
  =>
  CanMyMul (CollectErrors es a) Rational
  where
  type MyMulType (CollectErrors es a) Rational = EnsureCollectErrors es (MyMulType a Rational)
  myMul = secondNoErrors myMul

withErrorsExample4 :: CollectNumErrors Rational
withErrorsExample4 = ((1.0 *! 1.0) /! 1.0) *! (1.0 *! (1.0 /! 1.0))

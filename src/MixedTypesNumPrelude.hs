{-|
    Module      :  MixedTypesNumPrelude
    Description :  Bottom-up typed numeric expressions
    Copyright   :  (c) Michal Konecny, Pieter Collins
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    = Main purpose

    This package provides a version of Prelude where
    unary and binary operations such as @not@, @+@, @==@
    have their result type derived from the parameter type(s),
    allowing, /e.g./:

      * dividing an integer by an integer, giving a rational, wrapped in the CN (ie Collecting NumErrors) monad:

      @let n = 1 :: Integer in n/(n+1) :: CN Rational@

      @1/2 :: CN Rational@

      (The type @CN Rational@ would be derived automatically because
      integer literals are always of type @Integer@, not @Num t => t@.)

      * adding an integer and a rational, giving a rational:

      @(length [x])+1/3 :: CN Rational@

      The @CN@ monad is required because integer division can, in general, fail as it is a partial operation:

      @> 1/0 :: CN Rational@

      @{[(ErrorCertain,DivByZero)]} @

      When one is certain the division is well defined, one can remove @CN@ using the @~!@ operator:

      @((1/2)~!) :: Rational@

      @((1/n)~!) :: Rational@

      The last example will throw an exception when evaluated with @n=0@.

      * taking natural, integer and fractional power using the same operator:

      @2^2 :: CN Integer@

      @2.0^(-2) :: CN Rational@

      @(double 2)^(1/2) :: CN Double@

      The following examples require package <https://github.com/michalkonecny/aern2/aern2-real aern2-real>:

      @2^(1/2) :: CauchyRealCN@

      @pi :: CauchyReal@

      @sqrt 2 :: CauchyRealCN@

      * comparing an integer with an (exact) real number, giving a seqeunce of @Maybe Bool@:

      @if x < 0 then -x else x :: CauchyReal@

      In the last example, @if@ is overloaded so that it works for conditions
      of other types than @Bool@.  Here the condition has the type @Sequence (Maybe Bool)@.
      The whole expression is the sequence of balls in which those balls for which the condition
      is inconclusive are the union of the balls computed by both branches.

    = Type classes

    Arithmetic operations are provided via multi-parameter type classes
    and the result type is given by associated
    type families. For example:

    @(+) :: (CanAddAsymmetric t1 t2) => t1 -> t2 -> AddType t1 t2@

    The type constraint @CanAdd t1 t2@ implies both
    @CanAddAsymmetric t1 t2@ and @CanAddAsymmetric t2 t1@.

    For convenience there are other aggregate type constraints such as
    @CanAddThis t1 t2@, which implies that the result is of type @t1@,
    and @CanAddSameType t@, which is a shortcut for @CanAddThis t t@.

    == Testable specification

    The arithmetic type classes are accompanied by generic hspec test suites,
    which are specialised to concrete instance types for their testing.
    These test suites include the expected algebraic properties of operations,
    such as commutativity and associativity of addition.

    = Limitations

    * Not all numerical operations are supported yet.
      Eg @tan@, @atan@ are missing at the moment.

    * Inferred types can be very large. Eg for @f a b c = sqrt (a + b * c + 1)@ the inferred type is:

      @
      f: (CanMulAsymmetric t1 t2, CanAddAsymmetric t4 (MulType t1 t2),
          CanAddAsymmetric (AddType t4 (MulType t1 t2)) Integer,
          CanSqrt (AddType (AddType t4 (MulType t1 t2)) Integer)) =>
         t4
         -> t1
         -> t2
         -> SqrtType (AddType (AddType t4 (MulType t1 t2)) Integer)
      @

    * Due to limitations of some versions of ghc, type inferrence sometimes fails.
      Eg @add1 = (+ 1)@ fails (eg with ghc 8.0.2) unless we explicitly declare the type
      @add1 :: (CanAdd Integer t) => t -> AddType t Integer@
      or use an explicit parameter, eg @add1 x = x + 1@.

    = Origin

    The idea of having numeric expressions in Haskell with types
    derived bottom-up was initially suggested and implemented by Pieter Collins.
    This version is a fresh rewrite by Michal Konečný.

    = More details

    This module facilitates a single-line import for the package
    mixed-types-num.  See the re-exported modules for further details.
-}
module MixedTypesNumPrelude
(
  -- * Re-exporting Prelude, hiding the operators we are changing
  module Numeric.MixedTypes.PreludeHiding,
  -- * Modules with Prelude alternatives
  module Numeric.MixedTypes.Literals,
  module Numeric.MixedTypes.Bool,
  module Numeric.MixedTypes.Eq,
  module Numeric.MixedTypes.Ord,
  module Numeric.MixedTypes.MinMaxAbs,
  module Numeric.MixedTypes.AddSub,
  module Numeric.MixedTypes.Round,
  module Numeric.MixedTypes.Ring,
  module Numeric.MixedTypes.Field,
  (%),
  module Numeric.MixedTypes.Elementary,
  module Numeric.CollectErrors,
  module Utils.TH.DeclForTypes
)
where

import Data.Ratio ((%))
import Utils.TH.DeclForTypes
import Numeric.CollectErrors
import Numeric.MixedTypes.PreludeHiding
import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Round
import Numeric.MixedTypes.Ring
import Numeric.MixedTypes.Field
import Numeric.MixedTypes.Elementary
import Numeric.MixedTypes.Complex ()

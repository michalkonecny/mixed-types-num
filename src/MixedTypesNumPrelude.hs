{-|
    Module      :  MixedTypesNumPrelude
    Description :  Bottom-up typed numeric expressions
    Copyright   :  (c) Michal Konecny, Pieter Collins
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    @MixedTypesNumPrelude@ provides a version of @Prelude@ where
    unary and binary operations such as @not@, @+@, @==@
    have their result type derived from the parameter type(s).

    This module facilitates a single-line import for the package
    mixed-types-num.  See the re-exported modules for further details.
-}


module MixedTypesNumPrelude
(
  -- * Feature highlights
  -- ** Basics
  -- $basics

  -- ** Type classes
  -- $classes

  -- ** Testable specifications
  -- $specs

  -- * Limitations
  -- $limitations

  -- * Origin
  -- $origin

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
  module Numeric.MixedTypes.Elementary,
  module Numeric.MixedTypes.Complex,
  module Numeric.CollectErrors,
  module Utils.TH.DeclForTypes,
  -- * Re-export for convenient Rational literals
  (%)
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
import Numeric.MixedTypes.Complex

{- $basics

=== Literals have a fixed type

>>> :t 1
... Integer

>>> :t 1.0
... Rational

>>> 1 :: Rational
... Couldn't match type ‘Integer’ with ‘GHC.Real.Ratio Integer’ ...

=== Mixed-type operations

>>> :t 1.5 + 1
... :: Rational

>>> :t 1.5 * (length [[]])
... :: Rational

=== Dividing integers, dealing with potential error

>>> :t let n = 1 in n/(n+1)
... :: CollectErrors [(ErrorCertaintyLevel, NumError)] Rational

A shorter synonym of this type is @CN Rational@.
We use the shorter form below for better readability of this documentation
although ghci usually prints the longer version:

>>> :t let n = 1 in n/(n+1)
... :: CN Rational

The @CN@ wrapper here indicates that integer division can fail for some values:

>>> 1/0
{[(ERROR,division by 0)]}

Note that when evaluating @1/0@, it evaluates to the error value printed above.
This is not an exception, but a special value.

When one is certain the division is well defined, one can remove @CN@ in two ways:

>>> :t (1/!2)
... :: Rational

>>> :t (~!) (1/2)
... :: Rational

The operator @(/!)@ stands for division which throws an exception is the
denominator is 0.  It "propagates" any potential errors
from the sub-expressions.  For example:

>>> :t 1/!(1 - 1/n)
... :: CN Rational

The above expression will throw an error exception when evaluated with @n=1@
but when @n=0@, it will not throw an excetion but return an error value.

The @(~!)@ operator removes CN from any type, throwing an exception if some errors have certainly occurred:

>>> :t (~!) (1/(1 - 1/n))
... :: Rational

Potential errors are ignored by @(~!)@:

(These examples require also package <https://github.com/michalkonecny/aern2 aern2-real>.)

>>> (~!) sqrt (pi-pi)
[7.395570986446986e-32 ± <2^(-103)]

>>> sqrt (pi-pi)
[7.395570986446986e-32 ± <2^(-103)]{[(POTENTIAL ERROR,out of range: sqrt: argument must be >= 0: [0 ± <2^(-204)])]}


=== Natural, integer and fractional powers

>>> :t 2^2
...CN Integer

>>> :t 2.0^(-2)
...CN Rational

>>> :t (double 2)^(1/!2)
...Double

The following examples require package <https://github.com/michalkonecny/aern2 aern2-real>:

>>> :t 2^(1/2)
...CauchyRealCN

>>> :t pi
...CauchyReal

>>> :t sqrt 2
...CauchyRealCN

=== Comparing an integer with an (exact) real number

>>> let abs2 x = if x < 0 then -x else x in (abs2 (pi - pi)) ? (bitsS 100)
[0 ± <2^(-103)]{[(POTENTIAL ERROR,numeric error: union of enclosures: not enclosing the same value)]}

>>> let abs2 x = (~!) (if x < 0 then -x else x) in (abs2 (pi - pi)) ? (bitsS 100)
[0 ± <2^(-103)]

In these examples, @if@ is overloaded so that it works for conditions
of other types than @Bool@.  Here the condition has the type @Sequence (Maybe Bool)@.
The whole expression is the sequence of balls in which those balls for which the condition
is inconclusive are the union of the balls computed by both branches.

-}

{- $classes
    Arithmetic operations are provided via multi-parameter type classes
    and the result type is given by associated
    type families. For example:

    > (+) :: (CanAddAsymmetric t1 t2) => t1 -> t2 -> AddType t1 t2

    The type constraint @CanAdd t1 t2@ implies both
    @CanAddAsymmetric t1 t2@ and @CanAddAsymmetric t2 t1@.

    For convenience there are other aggregate type constraints such as
    @CanAddThis t1 t2@, which implies that the result is of type @t1@,
    and @CanAddSameType t@, which is a shortcut for @CanAddThis t t@.

    Notably, there are convenience classes `Ring` and `Field`.

    For types that instantiate Prelude classes such as `Num`, one can
    define instances of the new classes using the default implementation, eg:


    > newtype II = II Integer deriving (Eq, Ord, Num) -- assuming -XGeneralizedNewtypeDeriving
    > instance CanAddAsymmetric II II

    Conversely, if one defines instances for classes such as `CanAddAsymmetric`,
    one can then trivially define also instances of `Num` etc:

    > instance Prelude.Num T where
    >   (+) = (+)
    >   ...

-}

{- $specs
    The arithmetic type classes are accompanied by generic hspec test suites,
    which are specialised to concrete instance types for their testing.
    These test suites include the expected algebraic properties of operations,
    such as commutativity and associativity of addition.
-}

{- $limitations
    * Not all numerical operations are supported yet.
      Eg @tan@, @atan@ are missing at the moment.

    * Not all Prelude numerical types are supported yet.
      Eg @Natural@ and @Float@ are not supported at present,
      but @Double@ is supported.

    * Inferred types can be very large. Eg for @f a b c = sqrt (a + b * c + 1)@ the inferred type is:

    >  f: (CanMulAsymmetric t1 t2, CanAddAsymmetric t4 (MulType t1 t2),
    >      CanAddAsymmetric (AddType t4 (MulType t1 t2)) Integer,
    >      CanSqrt (AddType (AddType t4 (MulType t1 t2)) Integer)) =>
    >     t4
    >     -> t1
    >     -> t2
    >     -> SqrtType (AddType (AddType t4 (MulType t1 t2)) Integer)

    * Due to limitations of some versions of ghc, type inferrence sometimes fails.
      Eg @add1 = (+ 1)@ fails (eg with ghc 8.0.2) unless we explicitly declare the type
      @add1 :: (CanAdd Integer t) => t -> AddType t Integer@
      or use an explicit parameter, eg @add1 x = x + 1@.

-}

{- $origin

    The idea of having numeric expressions in Haskell with types
    derived bottom-up was initially suggested and implemented by Pieter Collins.
    This version is a fresh rewrite by Michal Konečný.

-}

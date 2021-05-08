# mixed-types-num

This package provides a version of Prelude where
unary and binary operations such as `not`, `+`, `==`
have their result type derived from the parameter type(s)
and thus supports mixed-type arithmetic and comparisons.

Partial operations such as division, sqrt and power
do not throw exceptions even when errors such as division by zero
occur.  Instead, these errors are propagated bottom-up in
a bespoke error-accumulating functor from package collect-errors.

This library (as well as collect-errors) arose while developing the
[AERN2](https://github.com/michalkonecny/aern2) library for interval and exact real computation.
Certain aspects are specifically tailored for interval or exact real arithmetics,
including three-valued numerical comparisons and distinguishing potential and certain errors.

## Generated documentation

See the [Hackage page](https://hackage.haskell.org/package/mixed-types-num).

## Feature highlights

To replicate the examples included below, start ghci as follows:

    $ stack ghci mixed-types-num:lib
    ...> :add MixedTypesNumPrelude

### Main idea

Literals have a fixed type:

    ...> :t 1
    ... Integer

    ...> :t 1.0
    ... Rational
    
    ...> 1 :: Rational
    ... Couldn't match type ‘Integer’ with ‘GHC.Real.Ratio Integer’ ...

Operations permit operands of mixed types, types inferred bottom-up:

    ...> :t 1.5 + 1
    ... :: Rational

    ...> :t 1.5 * (length [[]])
    ... :: Rational

### Dealing with numerical errors

To avoid runtime exceptions, it is recommended to use the CN error-collecting wrapper from package collect-errors:

    ...> :t let n = cn 1 in n/(n-1)
    ... :: CN Rational

`CN` is a synonym for `CollectErrors NumErrors Rational` as defined in package [collect-errors](https://hackage.haskell.org/package/collect-errors) module [Numeric.CollectErrors](https://hackage.haskell.org/package/collect-errors).
The `CN` wrapper indicates that integer division can fail for some values:

    ...> let n = cn 1 in n/(n-1)
    {{ERROR: division by 0}}

Note that the error printed above is not an exception, but a special value.

All arithmetic operations have been extended so that it is possible to have expressions that operate exclusively on CN-wrapped types:

    ...> f (n :: CN Integer) = 1/(1/(n-1) + 1/n) :: CN Rational
    ...> f (cn 0)
    {{ERROR: division by 0}}
    ...> f (cn 1)
    {{ERROR: division by 0}}
    ...> f (cn 2)
    2 % 3

The function `hasError` from module [Numeric.CollectErrors](https://hackage.haskell.org/package/collect-errors) can be used to check whether any error occurred:

    ...> import qualified Numeric.CollectErrors as CN
    ... CN> CN.hasError (cn 1/0)
    True

    ...> CN.hasError (cn 1/1)
    False

To extract a value from the `CN` wrapper, one can use function `withErrorOrValue`:

    ...> CN.withErrorOrValue (const 0.0) id (cn 1/2)
    1 % 2

The following examples require also package [aern2-real](https://github.com/michalkonecny/aern2).
To get access to this via stack, you can start ghci eg as follows:

    $ stack ghci aern2-real:lib
    ...> :add AERN2.Real 
    AERN2.Real> import MixedTypesNumPrelude

    ...> :t pi
    ...  :: CReal

    ...> :t sqrt 2
    ...  :: CReal

Harmless potential errors can be ignored using `unCN`:

    ...> sqrt (pi-pi) ? (prec 100)
    [0.0000000000000000006133173666733496325755399890... ± ~6.1332e-19 ~2^(-60)]{{POTENTIAL ERROR: out of domain: negative sqrt argument}}

    ...> unCN $ sqrt (pi-pi) ? (prec 100)
    [0.0000000000000000006133173666733496325755399890... ± ~6.1332e-19 ~2^(-60)]

If used unsafely, `unCN` will cause an exception:

    ...> unCN $ sqrt (-1) ? (prec 100)
    *** Exception: CollectErrors: {ERROR: out of domain: negative sqrt argument}

When an error is present (which can be checked using `hasError`), the function `CN.hasCertainError` can be used to further distinguish cases where the error is certain or potential:

    ...> import qualified Numeric.CollectErrors as CN
    ...> CN.hasCertainError (sqrt (-1) ? (prec 100))
    True

    ...> CN.hasCertainError (sqrt (pi-pi) ? (prec 100))
    False

### The generalised power operator

    ...> :t 2^(-2)
    ... :: Rational

    ...> :t 2^2
    ... :: Rational

    ...> :t round (2^2)
    ... :: Integer

    ...> :t (double 2)^(1/2)
    ... :: Double

The following example require also package [aern2-real](https://github.com/michalkonecny/aern2).

    ...> :t 2^(1/2)
    ... :: CReal

### Undecided comparisons

Comparisons involving intervals are undecided when the intervals overlap:

    > pi10 = pi ? (bits 10)
    > pi10
    [3.1416015625 ± ~9.7656e-4 ~2^(-10)]

    > pi10 > 0
    CertainTrue

    > pi10 == pi10
    TrueOrFalse

The above equality cannot be decided since `pi10` is not a single number but a set of numbers spanning the interval and the comparison operator cannot tell if the two operands sets represent the same number or a different number.

Comparison involving real numbers are semi-decidable.  The result of such a comparison is a lazy Kleenean, ie an infinite sequence of Kleeneans.
Please see package [aern2-real](https://github.com/michalkonecny/aern2) for further details.

### Fuzzy if-then-else

This package generalises the Haskell if-then-else statement so that it admits Kleenean and lazy Kleenean conditions:

    ...> abs1 x = max 0 (if x < 0 then -x else x)
    ...> abs1 (pi10 - pi10)
    [0.0009765625 ± ~9.7656e-4 ~2^(-10)]

Although the condition `x < 0` cannot be decided for the interval
`pi10-pi10 = [0 ± ~1.9531e-3 ~2^(-9)]`, the if-then-else statement is resolved by computing both branches and unifying the resulting intervals.  This makes sense only if both branches compute the same number whenever the condition cannot be decided, ie when `x = 0` in this case, making the function continuous.

If we try to define a discontinuous function this way, we get an error as soon as it is detected:

    ...> bad1 x = if x < 0 then 1-x else x
    ...> bad1 (pi10 - pi10)
    [0.5 ± ~0.5020 ~2^(-1)]{{ERROR: numeric error: union of enclosures: not enclosing the same value}}

The generalised if-then-else works also for real numbers with lazy Kleenean comparisons:

    ...> abs1 (pi - pi)
    {?(prec 36): [0.000000000014551915228366851806640625 ± ~1.4552e-11 ~2^(-36)]}

## Type classes

Mixed-type arithmetic operations are provided via multi-parameter type classes
and the result type is given by associated
type families. For example:

```Haskell
(+) :: (CanAddAsymmetric t1 t2) => t1 -> t2 -> AddType t1 t2
```

The constraint `CanAdd t1 t2` is a shortcut for both
`CanAddAsymmetric t1 t2` and `CanAddAsymmetric t2 t1`.

For convenience there are other aggregate type constraints such as
`CanAddThis t1 t2`, which implies that the result is of type `t1`,
and `CanAddSameType t`, which is a shortcut for `CanAddThis t t`.

Notably, there are convenience classes `Ring` and `Field` as well as `OrderedRing` and `OrderedField`.

For types that instantiate Prelude classes such as `Num`, one can
define instances of the new classes using the default implementation, eg:

```Haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import MixedTypesPrelude
import qualified Prelude as P

newtype II = II Integer deriving (P.Eq, P.Ord, P.Num) 
instance CanAddAsymmetric II II
```

Conversely, if one defines instances such as `CanAddAsymmetric T T`,
one can then trivially define also instances `Num T` etc:

```Haskell
instance P.Num T where
  (+) = (+)
  ...
```


## Testable specifications

The arithmetic type classes are accompanied by generic hspec test suites,
which are specialised to concrete instance types for their testing.
These test suites include the expected algebraic properties of operations,
such as commutativity and associativity of addition.

## Limitations

* Not all numerical operations are supported yet.
  Eg `tan`, `atan` are missing at the moment.

* Not all Prelude numerical types are supported yet.
  Eg `Natural` and `Float` are not supported at present,
  but `Double` is supported.

* Many common operations such as `fromEnum`, `threadDelay` give or require
  an `Int` value, which means we sometimes need to convert:

      threadDelay (int 1000000)
      integer (fromEnum True)

  Prelude functions such as `take`, `!!` and `length` that use `Int` in Prelude
  are shadowed in MixedTypesNumPrelude with more compatible/flexible versions.
  Beware that `Data.List.length` clashes with `length` in MixedTypesNumPrelude.

* Inferred types can be very large. Eg for `f a b c = sqrt (a + b * c + 1)` the inferred type is:

```Haskell
f :: (CanSqrt (AddType (AddType t2 (MulType t3 t4)) Integer),
      CanAddAsymmetric (AddType t2 (MulType t3 t4)) Integer,
      CanAddAsymmetric t2 (MulType t3 t4), CanMulAsymmetric t3 t4) =>
     t2
     -> t3
     -> t4
     -> SqrtType (AddType (AddType t2 (MulType t3 t4)) Integer)
```

<!-- * Due to limitations of some versions of ghc, type inference sometimes fails.
  Eg `add1 = (+ 1)` fails (eg with ghc 8.0.2) unless we explicitly declare the type
  @add1 :: (CanAdd Integer t) => t -> AddType t Integer@
  or use an explicit parameter, eg @add1 x = x + 1@. -->

## Credits

The idea of having numeric expressions in Haskell with types
derived bottom-up was initially suggested and implemented by Pieter Collins.
This version is a fresh rewrite by Michal Konečný.

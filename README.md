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

See module [MixedTypesNumPrelude](https://hackage.haskell.org/package/mixed-types-num/docs/MixedTypesNumPrelude.html) for further documentation.

[Hackage page including Haddock](https://hackage.haskell.org/package/mixed-types-num)

# mixed-types-num change log

* v 0.5.5 2021-05-26
  * powUsingMulRecip etc with custom multiply and recip operations
* v 0.5.4 2021-05-21
  * remove Kleeneans (move them to aern2-mp)
* v 0.5.3 2021-05-15
  * export clearPotentialErrors (from collect-errors)
* v 0.5.2 2021-05-14
  * add OrdGenericBool
* v 0.5.1 2021-05-12
  * if-then-else for CN-wrapped (see collect-errors) condition
  * Documentation now in README
* v 0.5.0 2021-04-13
  * use package collect-errors with a much simpler CN wrapper
  * replace Maybe Bool by Kleenean (a new type)
  * remove very long type constraints in specifications using PartialTypeSignatures
* v 0.4.1 2021-01-21
  * add hasErrorCE and hasErrorCN for testing if CE/CN values contain errors
* v 0.4.0.2 2020-08-02
  * remove smallcheck version upper bound
  * update to cabal-version >= 1.10
* v 0.4.0.1 2019-04-11
  * fix infinite loop in some conversions
* v 0.4.0 2019-04-10
  * eliminated dependency on convertible, improving ghcjs compatibility
* v 0.3.2 2019-01-08
  * added divI and mod
  * added enforceRange
  * used enforceRange in exp tests
* v 0.3.1.5 2018-11-14
  * improved documentation
* v 0.3.1.4 2017-12-06
  * removed upper bounds for dependencies
* v 0.3.1.3 2017-08-22
  * fixed bound on base in test suite
* v 0.3.1.2 2017-08-15
  * provided compatible versions of take, drop, length, replicate, splitAt
  * added missing mixed-type subtraction combination to Ring etc.

* v 0.3.0.1 2017-08-01
  * improve package documentation in module MixedTypesNumPrelude

* v 0.3 2017-08-01
  * renamed the main module to MixedTypesNumPrelude
  * much faster compilation
  * Ring and Field are now classes, not synonyms for large sets of constraints
  * many fixes in collect-error framework and its use in division and power
  * Overloaded if-then-else via -XRebindableSyntax
  * compiling with ghc 8.2.1

* v 0.2.0.1
  * fix compilation bug in test suite
  * minor doc improvements
  * fix Complex instances of error-throwing division (/!)

* v 0.2
  * new CollectErrors wrapper
  * CN, specialisation of CollectErrors to NumErrors
  * numerical partial operators (eg division) return a CN type
  * instances for Data.Complex

* v 0.1
  * first release

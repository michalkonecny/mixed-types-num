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

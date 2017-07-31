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

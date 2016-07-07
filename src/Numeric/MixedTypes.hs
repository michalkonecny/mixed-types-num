{-|
    Module      :  Numeric.MixedType
    Description :  Mixed-type numeric expressions
    Copyright   :  (c) Michal Konecny, Pieter Collins
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A single-import module for the package
    mixed-types-num.  Please see the package description.
-}

module Numeric.MixedTypes
(
  module Prelude,
  module Numeric.MixedTypes.Literals,
  module Numeric.MixedTypes.Bool,
  module Numeric.MixedTypes.EqOrd,
  module Numeric.MixedTypes.MinMaxAbs
)
where

import Prelude hiding
  (
    fromInteger, fromRational, (!!),
    negate, not, (&&), (||), and, or,
    (==),(/=),(<),(>),(<=),(>=),
    abs, min, max, minimum, maximum
  )

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd
import Numeric.MixedTypes.MinMaxAbs

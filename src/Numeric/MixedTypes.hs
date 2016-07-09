{-|
    Module      :  Numeric.MixedType
    Description :  Bottom-up typed numeric expressions
    Copyright   :  (c) Michal Konecny, Pieter Collins
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A single-import module for the package
    mixed-types-num.  Please see the package description (under Contents).
-}

module Numeric.MixedTypes
(
  -- ** Re-exporting Prelude, hiding the operators we are changing
  module Prelude,
  -- ** Modules with Prelude alternatives
  module Numeric.MixedTypes.Literals,
  module Numeric.MixedTypes.Bool,
  module Numeric.MixedTypes.EqOrd,
  module Numeric.MixedTypes.MinMaxAbs,
  module Numeric.MixedTypes.AddSub,
  module Numeric.MixedTypes.Round,
  module Numeric.MixedTypes.Ring,
  module Numeric.MixedTypes.Field,
  module Numeric.MixedTypes.Elementary
)
where

import Prelude hiding
  (
    fromInteger, fromRational, (!!),
    negate, not, (&&), (||), and, or,
    (==),(/=),(<),(>),(<=),(>=),
    abs, min, max, minimum, maximum,
     (-), (+), sum,
     (*), (^), (^^), product,
     (/), recip,
     properFraction, round, truncate, ceiling, floor,
     sqrt
  )

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd
import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Round
import Numeric.MixedTypes.Ring
import Numeric.MixedTypes.Field
import Numeric.MixedTypes.Elementary

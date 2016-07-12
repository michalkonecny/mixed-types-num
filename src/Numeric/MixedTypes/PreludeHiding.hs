{-|
    Module      :  Numeric.MixedType.PreludeHiding
    Description :  Prelude without operations that clash with MixedTypes
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Prelude without operations that clash with MixedTypes
-}
module Numeric.MixedTypes.PreludeHiding
(
  module Prelude
)
where

import Prelude hiding
  (
    fromInteger, fromRational, (!!),
    negate, not, (&&), (||), and, or,
    (==),(/=),(<),(>),(<=),(>=),
    abs, min, max, minimum, maximum,
     (-), (+), sum,
     (*), (^), (^^), (**), product,
     (/), recip,
     properFraction, round, truncate, ceiling, floor,
     sqrt, exp, log, sin, cos, pi
  )

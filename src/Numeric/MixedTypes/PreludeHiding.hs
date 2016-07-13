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
    fromInteger, fromRational, (!!)
    , Eq(..), Ord(..), Num(..), Fractional(..), RealFrac(..), Floating(..)
    , not, (&&), (||), and, or
    , (^), (^^)
    , minimum, maximum, sum, product
  )

{-|
    Module      :  Numeric.CollectErrors
    Description :  A type of numeric errors to be collected
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A type of numeric errors to be collected.
-}
module Numeric.CollectErrors
(
  -- * Describing numeric errors
  ErrorCertaintyLevel(..), NumError(..), NumErrors
  -- * The general error collection mechanism
, module Control.CollectErrors
  -- * Specialisation to numeric errors
, CollectNumErrors, noNumErrors
, WithoutCN, CanEnsureCN
, EnsureCN, ensureCN
  -- ** More compact synonyms
, CN, cn, unCN
)
where

import Prelude

import Control.CollectErrors

data NumError =
    DivByZero | OutOfRange String | NumError String
    deriving (Show, Eq)

data ErrorCertaintyLevel =
  ErrorCertain | ErrorPotential
    deriving (Show, Eq)

type NumErrors = [(ErrorCertaintyLevel, NumError)]

type CollectNumErrors v = CollectErrors NumErrors v

{-| Wrap a value in the 'CollectNumErrors' wrapper. -}
noNumErrors :: v -> CollectNumErrors v
noNumErrors = pure

type CanEnsureCN v = CanEnsureCE NumErrors v
type EnsureCN v = EnsureCE NumErrors v
type WithoutCN v = WithoutCE NumErrors v

ensureCN :: (CanEnsureCN v) => v -> EnsureCN v
ensureCN = ensureCE

-- more compact synonyms:

type CN v = CollectNumErrors v

{-| Wrap a value in the 'CollectNumErrors' wrapper. -}
cn :: v -> CollectNumErrors v
cn = noNumErrors

{-| An unsafe way to get a value out of the CollectNumErrors wrapper. -}
unCN :: CollectNumErrors v -> v
unCN = getValueOrThrowErrors

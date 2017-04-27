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
, CanEnsureCollectNumErrors, EnsureCollectNumErrors
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

noNumErrors :: v -> CollectNumErrors v
noNumErrors = pure

type CanEnsureCollectNumErrors v = CanEnsureCollectErrors NumErrors v
type EnsureCollectNumErrors v = EnsureCollectErrors NumErrors v

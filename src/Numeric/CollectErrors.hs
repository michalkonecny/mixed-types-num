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
, noValueNumErrorCertain, noValueNumErrorPotential
, WithoutCN, CanEnsureCN
, EnsureCN, ensureCN, deEnsureCN
  -- ** More compact synonyms
, CN, cn, unCN, (⚡), (~!)
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

{-| Construct an empty wrapper indicating that given error has certainly occurred. -}
noValueNumErrorCertain :: NumError -> CollectNumErrors v
noValueNumErrorCertain e = noValue [(ErrorCertain, e)]

{-| Construct an empty wrapper indicating that given error may have occurred. -}
noValueNumErrorPotential :: NumError -> CollectNumErrors v
noValueNumErrorPotential e = noValue [(ErrorPotential, e)]

type CanEnsureCN v = CanEnsureCE NumErrors v
type EnsureCN v = EnsureCE NumErrors v
type WithoutCN v = WithoutCE NumErrors v

{-|
  Translate a value of a type @a@
  to a value of a type @CollectNumErrors a@ except when @a@
  already is a @CollectNumErrors@ type, in which case the value is left as is.
-}
ensureCN :: (CanEnsureCN v) => v -> EnsureCN v
ensureCN = ensureCE

{-|
  Translate a value of a type @EnsureCN es a@ to @a@,
  throwing an exception if there was an error.
  If @a@ is a @CollectNumErrors@ type, then this is just an identity.
-}
deEnsureCN :: (CanEnsureCN v) => EnsureCN v -> Maybe v
deEnsureCN = deEnsureCE

-- more compact synonyms:

type CN v = CollectNumErrors v

{-| Wrap a value in the 'CollectNumErrors' wrapper. -}
cn :: v -> CollectNumErrors v
cn = noNumErrors

{-| An unsafe way to get a value out of the CollectNumErrors wrapper. -}
unCN :: CollectNumErrors v -> v
unCN = getValueOrThrowErrors

{-| An unsafe way to get a value out of the CollectNumErrors wrapper. -}
(⚡) :: CollectNumErrors v -> v
(⚡) = getValueOrThrowErrors

{-| An unsafe way to get a value out of the CollectNumErrors wrapper. -}
(~!) :: CollectNumErrors v -> v
(~!) = getValueOrThrowErrors

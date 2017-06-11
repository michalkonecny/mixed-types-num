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
  ErrorCertaintyLevel(..), NumError(..), NumErrors, sample_NumErrors
  -- * Specialisation to numeric errors
, CN, CanEnsureCN, EnsureCN, EnsureNoCN
, ensureCN, deEnsureCN, ensureNoCN
, noValueECN
, noValueNumErrorCertainECN, noValueNumErrorPotentialECN
, getMaybeValueECN, getErrorsECN, prependErrorsECN
  -- ** More compact synonyms
, cn, deCN, (⚡), (~!)
)
where

import Prelude (Show(..), Eq(..), String, Maybe(..))

import Control.CollectErrors

data NumError =
    DivByZero | OutOfRange String | NumError String
    deriving (Show, Eq)

data ErrorCertaintyLevel =
  ErrorCertain | ErrorPotential
    deriving (Show, Eq)

type NumErrors = [(ErrorCertaintyLevel, NumError)]

sample_NumErrors :: Maybe [(ErrorCertaintyLevel, NumError)]
sample_NumErrors = Nothing

type CN = CollectErrors NumErrors
type CanEnsureCN = CanEnsureCE NumErrors
type EnsureCN a = EnsureCE NumErrors a
type EnsureNoCN a = EnsureNoCE NumErrors a

{-|
  Translate a value of a type @a@
  to a value of a type @CollectNumErrors a@ except when @a@
  already is a @CollectNumErrors@ type, in which case the value is left as is.
-}
ensureCN :: (CanEnsureCN v) => v -> EnsureCN v
ensureCN = ensureCE sample_NumErrors

{-|
  Translate a value of a type @EnsureCN es a@ to @a@,
  throwing an exception if there was an error.
  If @a@ is a @CollectNumErrors@ type, then this is just an identity.
-}
deEnsureCN :: (CanEnsureCN v) => EnsureCN v -> Maybe v
deEnsureCN = deEnsureCE sample_NumErrors

{-|
  Translate a value of a type @a@
  to a value of a type @CollectNumErrors a@ except when @a@
  already is a @CollectNumErrors@ type, in which case the value is left as is.
-}
ensureNoCN :: (CanEnsureCN v) => v -> Maybe (EnsureNoCN v)
ensureNoCN = ensureNoCE sample_NumErrors

getErrorsECN ::
  (CanEnsureCN v) =>
  Maybe v {-^ sample only -} ->
  EnsureCN v -> NumErrors
getErrorsECN = getErrorsECE

getMaybeValueECN ::
  (CanEnsureCN v) =>
  EnsureCN v -> Maybe v
getMaybeValueECN = getMaybeValueECE sample_NumErrors

noValueECN :: (CanEnsureCN v) => Maybe v -> NumErrors -> EnsureCN v
noValueECN = noValueECE

{-| Construct an empty wrapper indicating that given error has certainly occurred. -}
noValueNumErrorCertainECN :: (CanEnsureCN v) => Maybe v -> NumError -> EnsureCN v
noValueNumErrorCertainECN sample_v e = noValueECE sample_v [(ErrorCertain, e)]

{-| Construct an empty wrapper indicating that given error may have occurred. -}
noValueNumErrorPotentialECN :: (CanEnsureCN v) => Maybe v -> NumError -> EnsureCN v
noValueNumErrorPotentialECN sample_v e = noValueECE sample_v [(ErrorPotential, e)]

prependErrorsECN ::
  (CanEnsureCN v) =>
  Maybe v {-^ sample only -} ->
  NumErrors -> EnsureCN v -> EnsureCN v
prependErrorsECN = prependErrorsECE


-- more compact synonyms:

{-| Wrap a value in the 'CollectNumErrors' wrapper. -}
cn :: (CanEnsureCN v) => v -> EnsureCN v
cn = ensureCN

{-| An unsafe way to get a value out of the CollectNumErrors wrapper. -}
deCN :: (CanEnsureCN v) => EnsureCN v -> Maybe v
deCN = deEnsureCN

{-| An unsafe way to get a value out of the CollectNumErrors wrapper. -}
(⚡) :: CN v -> v
(⚡) = getValueOrThrowErrorsCE

{-| An unsafe way to get a value out of the CollectNumErrors wrapper. -}
(~!) :: CN v -> v
(~!) = getValueOrThrowErrorsCE

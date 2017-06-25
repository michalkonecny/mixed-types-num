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
, noValueCN
, noValueNumErrorCertainCN, noValueNumErrorPotentialCN
, getMaybeValueCN, getErrorsCN, prependErrorsCN
, noValueECN
, noValueNumErrorCertainECN, noValueNumErrorPotentialECN
  -- ** More compact synonyms
, cn, deCN, (~!)
)
where

import Prelude
  (Show(..), Eq(..), String, Maybe(..), Either(..), (++), (.), or, map, fst, ($))

import Control.CollectErrors

data NumError =
    DivByZero | OutOfRange String | NumError String
    deriving (Eq)

instance Show NumError where
  show DivByZero = "division by 0"
  show (OutOfRange s) = "out of range: " ++ s
  show (NumError s) = "numeric error: " ++ s

data ErrorCertaintyLevel =
  ErrorCertain | ErrorPotential
  deriving (Eq)

instance Show ErrorCertaintyLevel where
  show ErrorCertain = "ERROR"
  show ErrorPotential = "POTENTIAL ERROR"

type NumErrors = [(ErrorCertaintyLevel, NumError)]

instance CanTestErrorsCertain NumErrors where
  hasCertainError es =
    or $ map ((== ErrorCertain) . fst) es

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
deEnsureCN :: (CanEnsureCN v) => EnsureCN v -> Either NumErrors v
deEnsureCN = deEnsureCE sample_NumErrors

{-|
  Translate a value of a type @a@
  to a value of a type @CollectNumErrors a@ except when @a@
  already is a @CollectNumErrors@ type, in which case the value is left as is.
-}
ensureNoCN :: (CanEnsureCN v) => v -> Either NumErrors (EnsureNoCN v)
ensureNoCN = ensureNoCE sample_NumErrors

noValueECN :: (CanEnsureCN v) => Maybe v -> NumErrors -> EnsureCN v
noValueECN = noValueECE

{-| Construct an empty wrapper indicating that given error has certainly occurred. -}
noValueNumErrorCertainECN :: (CanEnsureCN v) => Maybe v -> NumError -> EnsureCN v
noValueNumErrorCertainECN sample_v e = noValueECE sample_v [(ErrorCertain, e)]

{-| Construct an empty wrapper indicating that given error may have occurred. -}
noValueNumErrorPotentialECN :: (CanEnsureCN v) => Maybe v -> NumError -> EnsureCN v
noValueNumErrorPotentialECN sample_v e = noValueECE sample_v [(ErrorPotential, e)]

getErrorsCN :: CN v -> NumErrors
getErrorsCN = getErrorsCE

getMaybeValueCN :: CN v -> Maybe v
getMaybeValueCN = getMaybeValueCE

noValueCN :: NumErrors -> CN v
noValueCN = noValueCE

{-| Construct an empty wrapper indicating that given error has certainly occurred. -}
noValueNumErrorCertainCN :: NumError -> CN v
noValueNumErrorCertainCN e = noValueCN [(ErrorCertain, e)]

{-| Construct an empty wrapper indicating that given error may have occurred. -}
noValueNumErrorPotentialCN :: NumError -> CN v
noValueNumErrorPotentialCN e = noValueCN [(ErrorPotential, e)]

prependErrorsCN :: NumErrors -> CN v -> CN v
prependErrorsCN = prependErrorsCE

-- more compact synonyms:

{-| Wrap a value in the 'CollectNumErrors' wrapper. -}
cn :: (CanEnsureCN v) => v -> EnsureCN v
cn = ensureCN

{-| An unsafe way to get a value out of the CollectNumErrors wrapper. -}
deCN :: (CanEnsureCN v) => EnsureCN v -> Either NumErrors v
deCN = deEnsureCN

{-| An unsafe way to get a value out of the CollectNumErrors wrapper. -}
(~!) :: (CanEnsureCN v, Show v) => v -> EnsureNoCN v
(~!) = getValueOrThrowErrorsNCE sample_NumErrors

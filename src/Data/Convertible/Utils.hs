{- 
Borrowed from package convertible-1.1.1.0.  

We cannot use convertible directly as some of its dependencies do not compile on ghcjs.
-}

{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

{- |
   Module     : Data.Convertible.Utils
   Copyright  : Copyright (C) 2009-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

-}

module Data.Convertible.Utils(boundedConversion,
                             convertVia
                             )
where
import Prelude
import Data.Convertible.Base
import Data.Typeable

{- | Utility function to perform bounds checking as part of a conversion.

Does this be examining the bounds of the destination type, converting to the type of
the source via 'safeConvert', comparing to the source value.  Results in an error
if the conversion is out of bounds. -}
boundedConversion :: (Ord a, Bounded b, Show a, Show b, Convertible a Integer,
                      Convertible b Integer,
                      Typeable a, Typeable b) => 
                     (a -> ConvertResult b) -- ^ Function to do the conversion
                  -> a                      -- ^ Input data
                  -> ConvertResult b        -- ^ Result
boundedConversion (func :: (a -> ConvertResult b)) inp =
    do result <- func inp
       let smallest = asTypeOf minBound result
       let biggest = asTypeOf maxBound result
       let smallest' = (convert smallest)::Integer
       let biggest' = (convert biggest)::Integer
       let inp' = (convert inp)::Integer
       if inp' < smallest' || inp' > biggest'
          then convError ("Input value outside of bounds: " ++ show (smallest, biggest))
               inp :: ConvertResult b
          else return result :: ConvertResult b

ifThenElse :: Bool -> t -> t -> t
ifThenElse b c d 
   | b = c
   | otherwise = d

{- | Useful for defining conversions that are implemented in terms of other
conversions via an intermediary type. Instead of:

>instance Convertible CalendarTime POSIXTime where
>    safeConvert a = do r <- safeConvert a
>                       safeConvert (r :: ClockTime)

we can now write:

>instance Convertible CalendarTime POSIXTime where
>    safeConvert = convertVia (undefined::ClockTime)

which does the same thing -- converts a CalendarTime to a ClockTime, then a
ClockTime to a POSIXTime, both using existing 'Convertible' instances.
 -}
convertVia :: (Convertible a b, Convertible b c) =>
              b                 -- ^ Dummy data to establish intermediate type - can be undefined
           -> a                 -- ^ Input value
           -> ConvertResult c   -- ^ Result
convertVia dummy inp =
    do r1 <- safeConvert inp
       safeConvert (asTypeOf r1 dummy)


{- |
Module       : Mittens.Time
Description  : Some trivial missing functionality from the time package
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Mittens.Time where

import Data.Ord (comparing)
import Data.Time

-- Just for reference
-- data ZonedTime = ZonedTime { zonedTimeToLocalTime :: LocalTime
--                            , zonedTimeZone :: TimeZone
--                            }
-- 
-- zonedTimeToUTC :: ZonedTime -> UTCTime

instance Eq ZonedTime where
  (==) t1 t2 = (zonedTimeToLocalTime t1) == (zonedTimeToLocalTime t2) && (zonedTimeZone t1) == (zonedTimeZone t2)

instance Ord ZonedTime where
  compare = comparing zonedTimeToUTC

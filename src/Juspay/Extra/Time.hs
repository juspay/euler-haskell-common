module Juspay.Extra.Time
  ( readToLocalTime
  , convertLocalToUTC
  ) where

import           Data.Time (LocalTime,
                            UTCTime, localTimeToUTC, utc,
                            utcToLocalTime)

readToLocalTime :: Maybe UTCTime -> Maybe LocalTime
readToLocalTime = fmap (utcToLocalTime utc)

convertLocalToUTC :: LocalTime -> UTCTime
convertLocalToUTC = localTimeToUTC utc


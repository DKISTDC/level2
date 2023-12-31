module App.View.Common where

import Data.Time.Format (defaultTimeLocale, formatTime)
import NSO.Prelude

showDate :: UTCTime -> Text
showDate = cs . formatTime defaultTimeLocale "%F"

showTimestamp :: UTCTime -> Text
showTimestamp = cs . formatTime defaultTimeLocale "%F %T"

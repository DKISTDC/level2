module App.View.Common where

import Data.Time.Format (defaultTimeLocale, formatTime)
import NSO.Prelude
import Web.View


showDate :: UTCTime -> Text
showDate = cs . formatTime defaultTimeLocale "%F"


showTimestamp :: UTCTime -> Text
showTimestamp = cs . formatTime defaultTimeLocale "%F %T"


code :: Text -> View c ()
code = pre (fontSize 14)


hr :: Mod -> View c ()
hr f = tag "hr" f none

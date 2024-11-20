module App.View.Common where

import App.Colors
import Data.Time.Format (defaultTimeLocale, formatTime)
import NSO.Prelude
import Web.View
import Web.View.Style


showDate :: UTCTime -> Text
showDate = cs . formatTime defaultTimeLocale "%F"


showTimestamp :: UTCTime -> Text
showTimestamp = cs . formatTime defaultTimeLocale "%F %T"


code :: Text -> View c ()
code = pre (fontSize 14)


systemError :: Text -> View c ()
systemError = tag "code" (fontSize 14 . wrap) . text
 where
  wrap = addClass $ cls "wrap" & prop @Text "word-wrap" "break-word"


hr :: Mod -> View c ()
hr f = tag "hr" f none


progress :: Float -> View c ()
progress p = do
  row (bg Gray . height 20) $ do
    el (width (Pct p) . bg (light Info)) $ do
      space

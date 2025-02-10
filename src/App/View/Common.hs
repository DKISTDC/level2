module App.View.Common where

import App.Colors
import App.Style qualified as Style
import App.View.Icons qualified as Icons
import Data.Time.Format (defaultTimeLocale, formatTime)
import NSO.Prelude
import Web.Hyperbole hiding (input, label)
import Web.View qualified as View
import Web.View.Style


showDate :: UTCTime -> Text
showDate = cs . formatTime defaultTimeLocale "%F"


showTimestamp :: UTCTime -> Text
showTimestamp = cs . formatTime defaultTimeLocale "%F %T"


systemError :: Text -> View c ()
systemError e = do
  col (gap 5 . grow . borderColor Danger . border 1 . bg (HexColor "#fdd9d7") . rounded 5) $ do
    el (bold . color White . bg Danger . pad (XY 8 5)) "Error!"
    tag "code" (fontSize 14 . wrap . pad 10) . text $ e
 where
  wrap = addClass $ cls "wrap" & prop @Text "word-wrap" "break-word"


hr :: Mod c -> View c ()
hr f = tag "hr" f none


progress :: Float -> View c ()
progress p = do
  row (bg Gray . height 20) $ do
    el (width (Pct p) . bg (light Info)) $ do
      space


iconButton :: (ViewAction (Action id)) => Action id -> Mod id -> View id () -> Text -> View id ()
iconButton action f icon txt =
  button action f $ do
    row (gap 10) $ do
      el (width 24) icon
      text txt


toggleBtn :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> Mod id -> View id () -> View id ()
toggleBtn toAction sel f =
  button (toAction $ not sel) (f . Style.btn (if sel then on else off))
 where
  on = Primary
  off = Gray


checkBtn :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> Text -> View id ()
checkBtn toAction sel lbl = do
  View.button (gap 10 . onClick (toAction $ not sel) . flexRow) $ do
    checkCircle sel id
    text lbl


checkCircle :: Bool -> Mod c -> View c ()
checkCircle sel f =
  el (f . rounded 100 . border 1 . width 20 . height 20 . Style.alignMiddle) $ do
    content sel
 where
  content True = el (pad 2) Icons.check
  content False = ""

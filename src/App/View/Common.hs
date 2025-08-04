module App.View.Common where

import App.Colors
import App.Style qualified as Style
import App.View.Icons qualified as Icons
import Data.Time.Format (defaultTimeLocale, formatTime)
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole hiding (input, label)


showDate :: UTCTime -> Text
showDate = cs . formatTime defaultTimeLocale "%F"


showTimestamp :: UTCTime -> Text
showTimestamp = cs . formatTime defaultTimeLocale "%F %T"


systemError :: Text -> View c ()
systemError e = do
  col ~ gap 5 . grow . borderColor Danger . border 1 . bg (HexColor "#fdd9d7") . rounded 5 $ do
    el ~ bold . color White . bg Danger . pad (XY 8 5) $ "Error!"
    tag "code" ~ fontSize 14 . wrap . pad 10 $ text e
 where
  wrap = utility "wrap-word" ["word-wrap" :. "break-word"]


hr :: View c ()
hr = tag "hr" $ pure ()


progress :: Float -> View c ()
progress p = do
  row ~ bg Gray . height 20 $ do
    el ~ width (Pct p) . bg (light Info) $ do
      space


iconButton :: (ViewAction (Action id)) => Action id -> View id () -> Text -> View id ()
iconButton action icon txt =
  button action $ do
    row ~ gap 10 $ do
      el ~ width 24 $ icon
      text txt


toggleBtn :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> View id () -> View id ()
toggleBtn toAction sel =
  button (toAction $ not sel) ~ Style.btn (if sel then on else off)
 where
  on = Primary
  off = Gray


checkBtn :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> View id ()
checkBtn toAction sel = do
  button (toAction $ not sel) $ do
    checkCircle sel


checkCircle :: Bool -> View c ()
checkCircle sel =
  el ~ rounded 100 . border 1 . width 20 . height 20 . Style.alignMiddle $ do
    content sel
 where
  content True = el ~ pad 2 $ Icons.check
  content False = ""

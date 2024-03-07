module App.View.Inversions where

import App.Colors
import App.Globus (Task (..), taskPercentComplete)
import App.Style as Style
import NSO.Prelude
import Numeric
import Web.Hyperbole
import Web.View qualified as WebView


viewTransferProgress :: Task -> View c ()
viewTransferProgress t = do
  row id $ do
    el_ $ text $ "Transferring... (" <> cs rate <> " Mb/s)"
    space
    activityLink t
  progress (taskPercentComplete t)
 where
  rate :: String
  rate = showFFloat (Just 2) (fromIntegral t.effective_bytes_per_second / (1000 * 1000) :: Float) ""


viewTransferFailed :: Task -> View c ()
viewTransferFailed t = do
  row id $ do
    el_ "Transfer Failed"
    space
    activityLink t


activityLink :: Task -> View c ()
activityLink t =
  WebView.link activityUrl Style.link "View Transfer on Globus"
 where
  activityUrl = Url "https://" "app.globus.org" ["activity", t.task_id.unTagged] []


progress :: Float -> View c ()
progress p = do
  row (bg Gray . height 20) $ do
    el (width (Pct p) . bg (light Info)) $ do
      space

module App.Page.Inversions.InvForm where

import App.Colors
import App.Effect.Auth
import App.Globus as Globus
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons as Icons
import Effectful
import NSO.Prelude
import NSO.Types.Common (Id (..))
import Numeric (showFFloat)
import Web.Hyperbole
import Web.View qualified as WebView


-----------------------------------------------------
-- Inversion Transfer
-----------------------------------------------------

data TransferAction
  = CheckTransfer
  | TaskFailed
  | TaskSucceeded
  deriving (Show, Read, ViewAction)


-- I want it to reload itself and call these when necessary
checkTransfer :: (HyperView id, Action id ~ TransferAction, Hyperbole :> es, Globus :> es, Auth :> es) => Id Task -> Eff es (View id ())
checkTransfer it = do
  task <- requireLogin $ Globus.transferStatus it
  pure $ viewTransfer it task


viewLoadTransfer :: (HyperView id, Action id ~ TransferAction) => View id ()
viewLoadTransfer = do
  onLoad CheckTransfer 0 $ el (height 45) Icons.spinner


viewTransfer :: (HyperView id, Action id ~ TransferAction) => Id Task -> Task -> View id ()
viewTransfer it task =
  case task.status of
    Succeeded -> onLoad TaskSucceeded 0 none
    Failed -> onLoad TaskFailed 0 none
    _ ->
      viewPollTransfer it task


viewPollTransfer :: (HyperView id, Action id ~ TransferAction) => Id Task -> Task -> View id ()
viewPollTransfer it task = do
  onLoad CheckTransfer 5000 $ do
    viewTransferProgress it task


viewTransferProgress :: Id Task -> Task -> View c ()
viewTransferProgress it task = do
  row id $ do
    el_ $ text $ "Transferring... (" <> cs rate <> " Mb/s)"
    space
    activityLink it
  View.progress (taskPercentComplete task)
 where
  rate :: String
  rate = showFFloat (Just 2) (fromIntegral task.effective_bytes_per_second / (1000 * 1000) :: Float) ""


viewTransferFailed :: Id Task -> View c ()
viewTransferFailed it = do
  row id $ do
    el (color Danger) "Transfer Failed"
    space
    activityLink it


activityLink :: Id Task -> View c ()
activityLink it =
  WebView.link activityUrl (Style.link . newTab) "View Transfer on Globus"
 where
  activityUrl = Url "https://" "app.globus.org" ["activity", it.fromId] []
  newTab = att "target" "_blank"

module App.View.Transfer where

import App.Colors
import App.Effect.Transfer as Transfer
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons as Icons
import Effectful
import Effectful.Globus (Task, TaskStatus (..))
import Effectful.Log
import NSO.Prelude
import NSO.Types.Common (Id (..))
import Network.Globus qualified as Globus
import Numeric (showFFloat)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI (path, (./.))


-----------------------------------------------------
-- TransferAction -- view helpers
-----------------------------------------------------

data TransferAction
  = CheckTransfer
  | TaskFailed
  | TaskSucceeded
  deriving (Generic, ViewAction, ToParam, FromParam)


-- I want it to reload itself and call these when necessary
checkTransfer :: (ViewAction (Action id), Hyperbole :> es, Transfer :> es, Log :> es) => (TransferAction -> Action id) -> Id Task -> Eff es (View id ())
checkTransfer action it = do
  task <- Transfer.transferStatus it
  pure $ viewTransfer action it task


viewLoadTransfer :: (ViewAction (Action id)) => (TransferAction -> Action id) -> View id ()
viewLoadTransfer action = do
  el ~ height 45 @ onLoad (action CheckTransfer) 0 $ Icons.spinner


viewTransfer :: (ViewAction (Action id)) => (TransferAction -> Action id) -> Id Task -> Task -> View id ()
viewTransfer action it task =
  case task.status of
    Succeeded -> el @ onLoad (action TaskSucceeded) 0 $ none
    Failed -> el @ onLoad (action TaskFailed) 0 $ none
    _ -> viewPollTransfer action it task


viewPollTransfer :: (ViewAction (Action id)) => (TransferAction -> Action id) -> Id Task -> Task -> View id ()
viewPollTransfer action it task = do
  col @ onLoad (action CheckTransfer) 1000 $ do
    viewTransferProgress it task


viewTransferStatus :: Task -> View c ()
viewTransferStatus task = do
  let it :: Id Task = Id task.task_id.unTagged
  case task.status of
    Succeeded -> viewTransferSucceeded it
    Failed -> viewTransferFailed it
    _ -> viewTransferProgress it task


viewTransferProgress :: Id Task -> Task -> View c ()
viewTransferProgress it task = do
  row ~ gap 5 $ do
    el ~ width 20 $ Icons.spinnerCircle
    el $ text $ "Transferring... (" <> cs rate <> " Mb/s)"
    space
    activityLink it
  View.progress (Globus.taskPercentComplete task)
 where
  rate :: String
  rate = showFFloat (Just 2) (fromIntegral task.effective_bytes_per_second / (1000 * 1000) :: Float) ""


viewTransferSucceeded :: Id Task -> View c ()
viewTransferSucceeded it = do
  row ~ flexWrap Wrap $ do
    space
    activityLink it
  View.progressComplete


-- viewTransferError :: Id Task -> GlobusError -> View c ()
-- viewTransferError taskId err =
--   viewTransferFailed' message taskId
--  where
--   message =
--     case err of
--       Unauthorized req _ -> "Unauthorized: " <> cs (show req)
-- _ -> cs $ show err

viewTransferFailed :: Id Task -> View c ()
viewTransferFailed = do
  viewTransferFailed' "Transfer Failed"


viewTransferFailed' :: Text -> Id Task -> View c ()
viewTransferFailed' msg it = do
  row ~ flexWrap Wrap $ do
    el ~ color Danger $ text msg
    space
    activityLink it


activityLink :: Id Task -> View c ()
activityLink it =
  link activityUrl ~ Style.link @ newTab $ "View Transfer on Globus"
 where
  activityUrl = [uri|https://app.globus.org/activity|] ./. path it.fromId
  newTab = att "target" "_blank"

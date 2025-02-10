module App.View.Transfer where

import App.Colors
import App.Effect.Auth
import App.Globus as Globus
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons as Icons
import Data.Default (Default (..))
import Data.Map.Strict qualified as M
import Effectful
import Effectful.Error.Static
import Effectful.Log
import NSO.Prelude
import NSO.Types.Common (Id (..))
import Network.HTTP.Client qualified as HTTP
import Numeric (showFFloat)
import Web.Hyperbole
import Web.View qualified as WebView


-----------------------------------------------------
-- ActiveTransfer - stored in session
-----------------------------------------------------

-- without implementing Session manually and setting the cookie path
-- the active transfer is unique to each route
-- I should create an effect for this...

data ActiveTransfers = ActiveTransfers
  { transfers :: Map Text (Id Task)
  }
  deriving (Generic, Show, Read, ToParam, FromParam)
instance Default ActiveTransfers where
  def = ActiveTransfers mempty
instance Session ActiveTransfers where
  cookiePath = Just []


saveActiveTransfer :: (Hyperbole :> es) => Id a -> Id Task -> Eff es ()
saveActiveTransfer ida taskId = do
  ActiveTransfers xfers <- session
  saveSession $ ActiveTransfers $ M.insert ida.fromId taskId xfers


activeTransfer :: (Hyperbole :> es) => Id a -> Eff es (Maybe (Id Task))
activeTransfer ida = do
  ActiveTransfers xfers <- session
  pure $ M.lookup ida.fromId xfers


-----------------------------------------------------
-- TransferAction -- view helpers
-----------------------------------------------------

data TransferAction
  = CheckTransfer
  | TaskFailed
  | TaskSucceeded
  deriving (Show, Read, ViewAction)


-- I want it to reload itself and call these when necessary
checkTransfer :: (Log :> es, ViewAction (Action id), Globus :> es, Hyperbole :> es, Auth :> es) => (TransferAction -> Action id) -> Id Task -> Eff es (View id ())
checkTransfer toAction it = do
  res <- requireLogin $ runErrorNoCallStack @GlobusError $ Globus.transferStatus it
  pure $ case res of
    Left err -> viewTransferError err it
    Right task -> viewTransfer toAction it task


viewLoadTransfer :: (ViewAction (Action id)) => (TransferAction -> Action id) -> View id ()
viewLoadTransfer toAction = do
  el (height 45 . onLoad (toAction CheckTransfer) 0) Icons.spinner


viewTransfer :: (ViewAction (Action id)) => (TransferAction -> Action id) -> Id Task -> Task -> View id ()
viewTransfer toAction it task =
  case task.status of
    Succeeded -> el (onLoad (toAction TaskSucceeded) 0) none
    Failed -> el (onLoad (toAction TaskFailed) 0) none
    _ ->
      viewPollTransfer toAction it task


viewPollTransfer :: (ViewAction (Action id)) => (TransferAction -> Action id) -> Id Task -> Task -> View id ()
viewPollTransfer toAction it task = do
  col (onLoad (toAction CheckTransfer) 1000) $ do
    viewTransferProgress it task


viewTransferProgress :: Id Task -> Task -> View c ()
viewTransferProgress it task = do
  row (gap 5) $ do
    el (width 20) Icons.spinnerCircle
    el_ $ text $ "Transferring... (" <> cs rate <> " Mb/s)"
    space
    activityLink it
  View.progress (taskPercentComplete task)
 where
  rate :: String
  rate = showFFloat (Just 2) (fromIntegral task.effective_bytes_per_second / (1000 * 1000) :: Float) ""


viewTransferError :: GlobusError -> Id Task -> View c ()
viewTransferError err =
  viewTransferFailed' message
 where
  message =
    case err of
      Unauthorized req -> "Unauthorized: " <> cs (HTTP.host req) <> cs (HTTP.path req)
      _ -> cs $ show err


viewTransferFailed :: Id Task -> View c ()
viewTransferFailed = do
  viewTransferFailed' "Transfer Failed"


viewTransferFailed' :: Text -> Id Task -> View c ()
viewTransferFailed' msg it = do
  row Style.flexWrap $ do
    el (color Danger) (text msg)
    space
    activityLink it


activityLink :: Id Task -> View c ()
activityLink it =
  WebView.link activityUrl (Style.link . newTab) "View Transfer on Globus"
 where
  activityUrl = Url "https://" "app.globus.org" ["activity", it.fromId] []
  newTab = att "target" "_blank"

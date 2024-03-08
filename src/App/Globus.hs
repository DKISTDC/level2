module App.Globus
  ( GlobusClient (..)
  , Globus
  , authUrl
  , accessToken
  , redirectUri
  , fileManagerUrl
  , transferStatus
  , Uri
  , Id
  , Token
  , Task (..)
  , TaskStatus (..)
  , Tagged (..)
  , Token' (..)
  , Id' (..)
  , runGlobus
  , getAccessToken
  , saveAccessToken
  , clearAccessToken
  , taskPercentComplete
  , parseTransferForm
  , initDownload
  ) where

import App.Error (expectAuth)
import App.Route qualified as Route
import App.Types
import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import GHC.Generics
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import Network.Globus.Transfer (taskPercentComplete)
import Network.HTTP.Types (QueryItem)
import System.FilePath
import Web.Hyperbole
import Web.Hyperbole.Effect (Host (..))
import Web.View as WebView


authUrl :: (Globus :> es) => Eff es WebView.Url
authUrl = do
  uri <- send $ AuthUrl [TransferAll] (State "_")
  pure $ convertUrl uri
 where
  convertUrl :: Uri a -> WebView.Url
  convertUrl u =
    let Query ps = u.params
     in Url{scheme = scheme u.scheme, domain = u.domain, path = u.path, query = map query ps}
  scheme Https = "https://"
  scheme Http = "http://"
  query :: (Text, Maybe Text) -> QueryItem
  query (t, mt) = (cs t, cs <$> mt)


-- the interplay with the routes and this module is weird ...
redirectUri :: AppDomain -> Uri Globus.Redirect
redirectUri domain =
  Uri Https domain.unTagged (routePath Route.Redirect) (Query [])


accessToken :: (Globus :> es) => Token Exchange -> Eff es (Token Access)
accessToken tok = send $ AccessToken tok


-- accessToken :: Globus -> Uri Redirect -> Token Exchange -> m TokenResponse
-- accessToken (Token cid) (Token sec) red (Token code) =

fileManagerUrl :: (Route r) => r -> Text -> Request -> Url
fileManagerUrl r lbl req =
  Url
    "https://"
    "app.globus.org"
    ["file-manager"]
    [ ("method", Just "POST")
    , ("action", Just $ cs (renderUrl submitUrl))
    , ("folderlimit", Just "1")
    , ("filelimit", Just "0")
    , ("cancelurl", Just $ cs (renderUrl currentUrl))
    , ("label", Just $ cs lbl)
    ]
 where
  -- <> "&origin_id=d26bd00b-62dc-40d2-9179-7aece2b8c437"
  -- <> "&origin_path=%2Fdata%2Fpid_1_118%2F"
  -- <> "&two_pane=true"

  serverUrl :: [Text] -> Url
  serverUrl p = Url "https://" (cs req.host.text) p []

  currentUrl :: Url
  currentUrl = serverUrl req.path

  submitUrl :: Url
  submitUrl =
    serverUrl $ routePath r


data TransferForm a = TransferForm
  { label :: Field a Text
  , endpoint :: Field a Text
  , path :: Field a Text
  , endpoint_id :: Field a Text
  , folder :: Field a (Maybe Text)
  }
  deriving (Generic)


instance Form TransferForm where
  fromForm =
    genericFromForm
      defaultFormOptions
        { fieldLabelModifier = fields
        }
   where
    -- from my field names to theirs
    fields "folder" = "folder[0]"
    fields f = f


transferStatus :: (Hyperbole :> es, Globus :> es) => Id Task -> Eff es Task
transferStatus (Id ti) = do
  tok <- getAccessToken >>= expectAuth
  send $ StatusTask tok (Tagged ti)


getAccessToken :: (Hyperbole :> es) => Eff es (Maybe (Token Access))
getAccessToken = do
  fmap Tagged <$> session "globus"


saveAccessToken :: (Hyperbole :> es) => Token Access -> Eff es ()
saveAccessToken (Tagged acc) = setSession "globus" acc


clearAccessToken :: (Hyperbole :> es) => Eff es ()
clearAccessToken = clearSession "globus"


parseTransferForm :: (Hyperbole :> es) => Eff es (TransferForm Identity)
parseTransferForm =
  parseForm @TransferForm


initDownload :: (Hyperbole :> es, Globus :> es) => TransferForm Identity -> [Dataset] -> Eff es (Id Task)
initDownload t ds = do
  -- TODO: not sure if this belongs here. How can we handle errors?
  acc <- getAccessToken >>= expectAuth
  sub <- send $ SubmissionId acc
  res <- send $ Globus.Transfer acc $ downloadTransferRequest sub t ds
  pure $ Id res.task_id.unTagged
 where
  downloadTransferRequest :: Globus.Id Submission -> TransferForm Identity -> [Dataset] -> TransferRequest
  downloadTransferRequest submission_id tform datasets =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just tform.label
      , source_endpoint = dkistDataTransfer
      , destination_endpoint = Tagged tform.endpoint_id
      , data_ = map transferItem datasets
      , sync_level = SyncTimestamp
      }
   where
    dkistDataTransfer :: Globus.Id Collection
    dkistDataTransfer = Tagged "d26bd00b-62dc-40d2-9179-7aece2b8c437"

    datasetsFolder :: Id Proposal -> FilePath
    datasetsFolder pid = "/data" </> cs pid.fromId

    datasetSourcePath :: Dataset -> FilePath
    datasetSourcePath d = datasetsFolder d.primaryProposalId </> cs d.datasetId.fromId

    transferItem :: Dataset -> TransferItem
    transferItem d =
      TransferItem
        { data_type = DataType
        , source_path = datasetSourcePath d
        , destination_path = destinationFolder </> cs d.instrumentProgramId.fromId </> cs d.datasetId.fromId
        , recursive = True
        }

    destinationFolder :: FilePath
    destinationFolder =
      case tform.folder of
        Nothing -> cs tform.path
        Just f -> cs tform.path </> cs f

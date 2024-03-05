module App.Globus
  ( GlobusClient (..)
  , Globus
  , authUrl
  , accessToken
  , redirectUri
  , fileManagerUrl
  , handleTransfer
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
  ) where

import App.Error (expectAuth, expectFound)
import App.Route qualified as Route
import App.Types
import Data.Tagged
import Data.Text qualified as Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import GHC.Generics
import NSO.Data.Datasets (Datasets)
import NSO.Data.Datasets qualified as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import Network.Globus.Transfer (taskPercentComplete)
import Network.HTTP.Types (urlEncode)
import System.FilePath
import Web.Hyperbole
import Web.Hyperbole.Effect (Host (..))
import Web.View as WebView


authUrl :: (Globus :> es) => Eff es WebView.Url
authUrl = do
  uri <- send $ AuthUrl [TransferAll] (State "_")
  pure $ Url $ renderUri uri


redirectUri :: AppDomain -> Uri Globus.Redirect
redirectUri domain =
  let Url path = pathUrl $ routePath Route.Redirect
   in Uri Https domain.unTagged [path] (Query [])


accessToken :: (Globus :> es) => Token Exchange -> Eff es (Token Access)
accessToken tok = send $ AccessToken tok


-- accessToken :: Globus -> Uri Redirect -> Token Exchange -> m TokenResponse
-- accessToken (Token cid) (Token sec) red (Token code) =

-- different url
fileManagerUrl :: Id Inversion -> Id InstrumentProgram -> Request -> Url
fileManagerUrl inv ip req =
  Url $
    "https://app.globus.org/file-manager?method=POST&action="
      <> cs (urlEncode True (cs submitUrl))
      <> "&folderlimit=1"
      <> "&filelimit=0"
      <> "&cancelurl="
      <> cs (urlEncode True (cs currentUrl))
      -- <> "&origin_id=d26bd00b-62dc-40d2-9179-7aece2b8c437"
      -- <> "&origin_path=%2Fdata%2Fpid_1_118%2F"
      -- <> "&two_pane=true"
      <> "&label=Transfer+Instrument+Program+"
      <> ip.fromId
 where
  serverUrl = "https://" <> cs req.host.text
  currentUrl = serverUrl <> "/" <> Text.intercalate "/" req.path
  submitUrl =
    let Url path = pathUrl . routePath $ Route.SubmitDownload inv
     in serverUrl <> path


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


handleTransfer :: (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, IOE :> es) => Id Inversion -> Page es Response
handleTransfer iv = do
  load $ do
    t <- parseForm @TransferForm

    (inv :| _) <- send (Inversions.ById iv) >>= expectFound
    ds <- send $ Datasets.Query $ Datasets.ByProgram inv.programId

    acc <- getAccessToken >>= expectAuth
    sub <- send $ SubmissionId acc
    res <- send $ Globus.Transfer acc $ transferRequest sub t ds

    -- TODO: this doesn't belong here. We need a "page" or something
    send $ Inversions.SetDownloading iv (Id res.task_id.unTagged)

    -- Redirect back to the inversion page
    redirect $ pathUrl . routePath $ Route.Program inv.programId


transferStatus :: (Hyperbole :> es, Globus :> es) => Id Task -> Eff es Task
transferStatus (Id ti) = do
  tok <- getAccessToken >>= expectAuth
  send $ TaskStatus tok (Tagged ti)


transferRequest :: Globus.Id Submission -> TransferForm Identity -> [Dataset] -> TransferRequest
transferRequest submission_id tform datasets =
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

  -- proposalId :: NonEmpty Dataset -> Id Proposal
  -- proposalId (d :| _) = d.primaryProposalId

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


getAccessToken :: (Hyperbole :> es) => Eff es (Maybe (Token Access))
getAccessToken = do
  fmap Tagged <$> session "globus"


saveAccessToken :: (Hyperbole :> es) => Token Access -> Eff es ()
saveAccessToken (Tagged acc) = setSession "globus" acc


clearAccessToken :: (Hyperbole :> es) => Eff es ()
clearAccessToken = clearSession "globus"

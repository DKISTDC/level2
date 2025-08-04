module App.Effect.Transfer where

import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import GHC.Generics
import NSO.Data.Scratch (Scratch)
import NSO.Data.Scratch qualified as Scratch
import NSO.Image.Files qualified as Files
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.Dataset
import Web.FormUrlEncoded qualified as FUE
import Web.Hyperbole


-- 1. User selects a download path (DKIST -> user+formpath)
-- 2. Users selects an upload path (user+formpath -> system)
-- 3. System downloads (DKIST ->
-- 4. Publish (system -> DKIST)

-- what's the final thing? An endpoint + path
-- Id Collection (a globus collection, sure)
-- File f a

newtype TransferSource = TransferSource (Id Collection)
newtype TransferDest = TransferDest (Id Collection)


data Source
data Dest


data RemoteFiles x a = RemoteFiles
  { endpoint :: Id Collection
  , path :: Path a
  }


data Transfer :: Effect where
  TransferFiles :: RemoteFiles Source a -> RemoteFiles Dest a -> Transfer m (Id Task)
  TaskStatus :: Id Task -> Transfer m Task
type instance DispatchOf Transfer = 'Dynamic


-- runTransfer
--   :: (Globus :> es, Error GlobusError :> es)
--   => Eff (Transfer : es) a
--   -> Eff es a
-- runTransfer = interpret $ \_ -> \case
--   TransferFiles src dest -> _
--   TaskStatus taskId -> _

data TransferForm = TransferForm
  { label :: Text
  , path :: Path' Dir TransferForm
  , endpoint_id :: Text
  }
  deriving (Generic, FromForm)
instance Show TransferForm where
  show tf = "TransferForm " <> unwords [show tf.label, show tf.path, show tf.endpoint_id]


transferStatus :: (Log :> es, Reader (Token Access) :> es, Globus :> es, Error GlobusError :> es) => Id Task -> Eff es Task
transferStatus (Id ti) = do
  acc <- ask
  send $ StatusTask acc (Tagged ti)


initTransfer :: (Globus :> es, Log :> es, Error GlobusError :> es, Reader (Token Access) :> es) => (Globus.Id Submission -> TransferRequest) -> Eff es (App.Id Task)
initTransfer toRequest = do
  acc <- ask
  sub <- send $ Globus.GetSubmissionId acc
  res <- send $ Globus.Transfer acc $ toRequest sub
  pure $ Id res.task_id.unTagged


data DownloadFolder = DownloadFolder
  { folder :: Maybe (Path' Dir DownloadFolder)
  }
  deriving (Generic)
instance FromForm DownloadFolder where
  fromForm f = do
    DownloadFolder <$> FUE.parseUnique "folder[0]" f


initDownloadL1Inputs :: (Globus :> es, Log :> es, Error GlobusError :> es, Reader (Token Access) :> es) => TransferForm -> DownloadFolder -> [Dataset] -> Eff es (App.Id Task)
initDownloadL1Inputs tform df ds = do
  initTransfer downloadTransferRequest
 where
  downloadTransferRequest :: Globus.Id Submission -> TransferRequest
  downloadTransferRequest submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just tform.label
      , source_endpoint = dkistEndpoint
      , destination_endpoint = Tagged tform.endpoint_id
      , data_ = map (\d -> datasetTransferItem (destinationPath d) d) ds
      , sync_level = SyncTimestamp
      , store_base_path_info = True
      }
   where
    destinationPath :: Dataset -> Path' Dir Dataset
    destinationPath d =
      downloadDestinationFolder tform df </> Path (cs d.instrumentProgramId.fromId) </> Path (cs d.datasetId.fromId)

    downloadDestinationFolder :: TransferForm -> DownloadFolder -> Path' Dir TransferForm
    downloadDestinationFolder tform' df' =
      -- If they didn't select a folder, use the current folder
      case df'.folder of
        Just f -> tform'.path </> f
        Nothing -> tform'.path


dkistEndpoint :: Globus.Id Collection
dkistEndpoint = Tagged "d26bd00b-62dc-40d2-9179-7aece2b8c437"


datasetTransferItem :: Path' Dir Dataset -> Dataset -> TransferItem
datasetTransferItem dest d =
  TransferItem
    { data_type = DataType
    , source_path = datasetSourcePath.filePath
    , destination_path = dest.filePath
    , recursive = True
    }
 where
  datasetSourcePath :: Path' Dir Dataset
  datasetSourcePath = Path (cs d.bucket) </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)


initScratchDatasets :: (IOE :> es, Log :> es, Globus :> es, Error GlobusError :> es, Reader (Token Access) :> es, Scratch :> es) => [Dataset] -> Eff es (App.Id Task)
initScratchDatasets ds = do
  scratch <- Scratch.collection
  initTransfer (transfer scratch)
 where
  transfer :: Globus.Id Collection -> Globus.Id Submission -> TransferRequest
  transfer scratch submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just $ "Datasets: " <> cs (show (fmap (.datasetId) ds))
      , source_endpoint = dkistEndpoint
      , destination_endpoint = scratch
      , data_ = fmap (\d -> datasetTransferItem (Files.dataset d) d) ds
      , sync_level = SyncTimestamp
      , store_base_path_info = True
      }

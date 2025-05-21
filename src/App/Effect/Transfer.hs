module App.Effect.Transfer where

import App.Effect.Scratch (Scratch)
import App.Effect.Scratch qualified as Scratch
import Data.List qualified as L
import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import GHC.Generics
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Common as App
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram (Proposal)
import Web.FormUrlEncoded qualified as FUE
import Web.Hyperbole
import Web.Hyperbole.View.Forms (formLookup)


-- data Status = Status
--   { taskId :: Id Task
--   , progress :: Float
--   , taskStatus :: TaskStatus
--   }
--
--
-- data Transfer :: Effect where
--   TransferStatus :: Id Task -> Transfer es Status
-- type instance DispatchOf Transfer = 'Dynamic
--
--
-- runTransfer
--   :: (Globus :> es, Scratch :> es)
--   => Eff (Transfer : es) a
--   -> Eff es a
-- runTransfer = interpret $ \_ -> \case
--   TransferStatus taskId -> pure _

data TransferForm = TransferForm
  { label :: Text
  , -- , endpoint :: Field Text
    path :: Path' Dir TransferForm
  , endpoint_id :: Text
  }
  deriving (Generic, FromForm)
instance Show TransferForm where
  show tf = "TransferForm " <> unwords [show tf.label, show tf.path, show tf.endpoint_id]


data DownloadFolder = DownloadFolder
  { folder :: Maybe (Path' Dir DownloadFolder)
  }
  deriving (Generic)
instance FromForm DownloadFolder where
  fromForm f = do
    DownloadFolder <$> formLookup "folder[0]" f


data UploadFiles t f = UploadFiles
  { quantities :: Field f (Path' t InvQuantities)
  , profileFit :: Field f (Path' t InvProfileFit)
  , profileOrig :: Field f (Path' t InvProfileOrig)
  -- , timestamps :: Path' t Timestamps
  }
  deriving (Generic)
instance Show (UploadFiles Filename Maybe) where
  show (UploadFiles q pf po) = "UploadFiles " <> show q <> " " <> show pf <> " " <> show po
instance FromForm (UploadFiles Filename Maybe) where
  fromForm f = do
    fs <- files f
    let quantities = findFile Scratch.fileQuantities fs
    let profileFit = findFile Scratch.fileProfileFit fs
    let profileOrig = findFile Scratch.fileProfileOrig fs
    pure UploadFiles{quantities, profileFit, profileOrig}
   where
    files :: FUE.Form -> Either Text [Path' Filename ()]
    files frm = do
      f0 <- formLookup "file[0]" frm
      f1 <- formLookup "file[1]" frm
      f2 <- formLookup "file[2]" frm
      f3 <- formLookup "file[3]" frm
      pure $ catMaybes [f0, f1, f2, f3]

    findFile :: Path' Filename a -> [Path' Filename ()] -> Maybe (Path' Filename a)
    findFile file fs = do
      Path ff <- L.find (isFile file) fs
      pure $ Path ff

    isFile :: Path' Filename a -> Path' Filename () -> Bool
    isFile (Path fa) (Path fb) = fa == fb


transferStatus :: (Log :> es, Reader (Token Access) :> es, Globus :> es, Error GlobusError :> es) => App.Id Task -> Eff es Task
transferStatus (Id ti) = do
  acc <- ask
  send $ StatusTask acc (Tagged ti)


initTransfer :: (Globus :> es, Log :> es, Error GlobusError :> es, Reader (Token Access) :> es) => (Globus.Id Submission -> TransferRequest) -> Eff es (App.Id Task)
initTransfer toRequest = do
  acc <- ask
  sub <- send $ Globus.GetSubmissionId acc
  res <- send $ Globus.Transfer acc $ toRequest sub
  pure $ Id res.task_id.unTagged


initUpload
  :: (Hyperbole :> es, Globus :> es, Log :> es, Error GlobusError :> es, Scratch :> es, Reader (Token Access) :> es)
  => TransferForm
  -> UploadFiles Filename Maybe
  -> App.Id Proposal
  -> App.Id Inversion
  -> Eff es (App.Id Task)
initUpload tform up ip ii = do
  scratch <- Scratch.collection
  initTransfer (transferRequest scratch)
 where
  transferRequest :: Globus.Id Collection -> Globus.Id Submission -> TransferRequest
  transferRequest scratch submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just tform.label
      , source_endpoint = Tagged tform.endpoint_id
      , destination_endpoint = scratch
      , data_ = catMaybes [transferItem <$> up.quantities, transferItem <$> up.profileFit, transferItem <$> up.profileOrig]
      , sync_level = SyncChecksum
      , store_base_path_info = True
      }
   where
    transferItem :: Path' Filename a -> TransferItem
    transferItem f =
      TransferItem
        { data_type = DataType
        , source_path = (source tform.path f).filePath
        , destination_path = (dest f).filePath
        , recursive = False
        }

    dest :: Path' Filename a -> Path' File a
    dest fn = Scratch.blanca ip ii </> fn

    source :: Path' Dir TransferForm -> Path' Filename a -> Path' File a
    source t fn = t </> fn


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
downloadDestinationFolder tform df =
  -- If they didn't select a folder, use the current folder
  case df.folder of
    Just f -> tform.path </> f
    Nothing -> tform.path


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


initScratchDataset :: (IOE :> es, Log :> es, Globus :> es, Error GlobusError :> es, Reader (Token Access) :> es, Scratch :> es) => Dataset -> Eff es (App.Id Task)
initScratchDataset d = do
  scratch <- Scratch.collection
  initTransfer (transfer scratch)
 where
  transfer :: Globus.Id Collection -> Globus.Id Submission -> TransferRequest
  transfer scratch submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just $ "Dataset " <> d.datasetId.fromId
      , source_endpoint = dkistEndpoint
      , destination_endpoint = scratch
      , data_ = [datasetTransferItem (Scratch.dataset d) d]
      , sync_level = SyncTimestamp
      , store_base_path_info = True
      }

-- really, I want to just open the file manager at that location, let thme handle it.
-- initDownloadL2Gen :: (Globus :> es, Reader (Token Access) :> es) => TransferForm -> DownloadFolder -> Inversion -> Eff es (App.Id Task)
-- initDownloadL2Gen tform df inv = do
--   initTransfer downloadTransferRequest
--  where
--   downloadTransferRequest :: Globus.Id Submission -> TransferRequest
--   downloadTransferRequest submission_id =
--     TransferRequest
--       { data_type = DataType
--       , submission_id
--       , label = Just tform.label.value
--       , source_endpoint = dkistEndpoint
--       , destination_endpoint = Tagged tform.endpoint_id.value
--       , data_ = [transferItem $ Scratch.outputL2Dir inv.proposalId inv.inversionId]
--       , sync_level = SyncTimestamp
--       , store_base_path_info = True
--       }
--    where
--     transferItem :: Path' Dir L2Frame -> TransferItem
--     transferItem dir =
--       TransferItem
--         { data_type = DataType
--         , source_path = dir.filePath
--         , destination_path = (downloadDestinationFolder tform df).filePath
--         , recursive = False
--         }

-- catchHttpGlobus :: (Log :> es, Error GlobusError :> es) => Eff es a -> Eff es a
-- catchHttpGlobus eff =
--   catch eff onGlobusErr
--  where
--   onGlobusErr = \case
--     Req.VanillaHttpException (HttpExceptionRequest req (StatusCodeException res body)) -> do
--       log Err $ dump "GLOBUS StatusCodeException" (req, res)
--       onStatusErr req (responseStatus res) body
--     ex -> do
--       log Err $ dump "GLOBUS HttpException" ex
--       throwError $ ReqError ex
--
--   onStatusErr req (status :: Status) body
--     | status == unauthorized401 = throwError $ Unauthorized req
--     | otherwise = throwError $ StatusError req status body

-- pure $ Hyperbole.Err $ Hyperbole.ErrOther $ "Globus request to " <> cs (path req) <> " failed with:\n " <> cs (show status)
-- onGlobusErr ex = do
--   log Err $ dump "GLOBUS" ex
--   pure $ Hyperbole.Err $ Hyperbole.ErrOther "Globus Error"

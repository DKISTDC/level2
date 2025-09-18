module App.Effect.Transfer where

import App.Effect.Auth (Auth)
import App.Effect.Auth qualified as Auth
import Data.Tagged
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Files.DKIST as DKIST
import NSO.Files.Image qualified as Image
import NSO.Files.Inversion (InversionFiles (..))
import NSO.Files.RemoteFolder
import NSO.Files.Scratch (Mounted, Scratch)
import NSO.Files.Scratch qualified as Scratch
import NSO.Files.TransferForm (DownloadFolder (..), TransferForm (..), User)
import NSO.Files.TransferForm qualified as TransferForm
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)


data FileTransfer sys dest f a = FileTransfer
  { sourcePath :: Path sys f a
  , destPath :: Path dest f a
  , recursive :: Bool
  }
  deriving (Show)


data Transfer :: Effect where
  -- download: DKIST to local
  -- upload: local to Scratch
  -- generate: DKIST to Scratch
  TransferFiles :: Text -> Remote src -> Remote dest -> [FileTransfer src dest f a] -> Transfer m (Id Task)
  TransferStatus :: Id Task -> Transfer m Task
type instance DispatchOf Transfer = 'Dynamic


runTransfer
  :: (Globus :> es, Log :> es, Auth :> es)
  => Eff (Transfer : es) a
  -> Eff es a
runTransfer = interpret $ \_ -> \case
  TransferStatus taskId -> do
    Auth.waitForAdmin $ do
      acc <- ask @(Token Access)
      send $ StatusTask acc (Tagged taskId.fromId)
  TransferFiles lbl source dest files ->
    Auth.waitForAdmin $ do
      transferFiles lbl source dest files
 where
  transferFiles :: (Log :> es, Globus :> es, Reader (Token Access) :> es) => Text -> Remote src -> Remote dest -> [FileTransfer src dest f a] -> Eff es (Id Task)
  transferFiles lbl source dest files = do
    acc <- ask @(Token Access)
    log Debug "TRANSFER"
    log Debug $ dump " source: " source
    log Debug $ dump " dest: " dest
    log Debug $ dump " files: " files
    sub <- send $ Globus.GetSubmissionId acc
    let items = fmap transferItem files
    let req = transferRequest sub
    log Debug $ dump " items: " $ fmap (\t -> (t.source_path, t.destination_path)) items
    res <- send $ Globus.Transfer acc req
    pure $ Id res.task_id.unTagged
   where
    transferItem :: FileTransfer src dest f a -> TransferItem
    transferItem file =
      TransferItem
        { data_type = DataType
        , source_path = file.sourcePath.filePath
        , destination_path = file.destPath.filePath
        , recursive = file.recursive
        }

    transferRequest :: Globus.Id Submission -> TransferRequest
    transferRequest submission_id =
      let items = fmap transferItem files
       in TransferRequest
            { data_type = DataType
            , submission_id
            , label = Just lbl
            , source_endpoint = source.collection
            , destination_endpoint = dest.collection
            , data_ = items
            , sync_level = SyncTimestamp
            , store_base_path_info = True
            }


transferStatus :: (Transfer :> es) => Id Task -> Eff es Task
transferStatus = send . TransferStatus


-- Download Datasets ----------------------------------------------------------------------

userDownloadDatasets :: (Transfer :> es) => TransferForm -> DownloadFolder -> [Dataset] -> Eff es (Id Task)
userDownloadDatasets tform df ds = do
  let dest = TransferForm.remote tform
  downloadL1To tform.label (TransferForm.dataset tform df) dest ds


scratchDownloadDatasets :: (Transfer :> es, Scratch :> es) => [Dataset] -> Eff es (Id Task)
scratchDownloadDatasets ds = do
  scratch <- Scratch.remote
  let lbl = "Sync Datasets: " <> T.intercalate "," (fmap (\d -> d.datasetId.fromId) ds)
  downloadL1To lbl Image.dataset scratch.remote ds


downloadL1To :: forall dest es. (Transfer :> es) => Text -> (Dataset -> Path dest File Dataset) -> Remote dest -> [Dataset] -> Eff es (Id Task)
downloadL1To lbl toDestPath dest ds = do
  let xfers = fmap datasetTransfer ds
  send $ TransferFiles lbl DKIST.remote dest xfers
 where
  datasetTransfer :: Dataset -> FileTransfer DKIST dest File Dataset
  datasetTransfer d =
    FileTransfer
      { sourcePath = DKIST.dataset d
      , destPath = toDestPath d
      , recursive = True
      }


-- Upload ---------------------------------------------------------------------------------

uploadInversionResults
  :: (Transfer :> es, Scratch :> es)
  => TransferForm
  -> InversionFiles Maybe Filename
  -> App.Id Proposal
  -> App.Id Inversion
  -> Eff es (Id Task)
uploadInversionResults tform upfiles propId invId = do
  scratch <- Scratch.remote
  let source = TransferForm.remote tform
  send $ TransferFiles tform.label source scratch.remote (fileTransfers scratch)
 where
  fileTransfers :: RemoteFolder Scratch () -> [FileTransfer User Scratch File Inversion]
  fileTransfers scratch = catMaybes [fmap fileTransfer upfiles.quantities, fmap fileTransfer upfiles.profileFit, fmap fileTransfer upfiles.profileOrig]
   where
    fileTransfer :: Path Scratch Filename a -> FileTransfer User Scratch File Inversion
    fileTransfer p@(Path a) =
      FileTransfer
        { sourcePath = TransferForm.directory tform </> Path a
        , destPath = scratch.directory </> Image.blancaFile (Image.blancaInput propId invId) p
        , recursive = False
        }


-- sourceBlanca :: Path User Dir a -> Path User Dir User
-- sourceBlanca (Path a) = Path a

-- Publish -------------------------------------------------------------------------------

-- /level2/generated/pid_1_118/inv.SD9T3L/
-- /level2/generated/pid_1_118/inv.SD9T3L/
transferSoftPublish
  :: (Transfer :> es, Scratch :> es)
  => App.Id Proposal
  -> App.Id Inversion
  -> Eff es (App.Id Task)
transferSoftPublish propId invId = do
  scratch <- Scratch.remote
  source <- Scratch.mountedPath (Image.outputL2Dir propId invId)
  let lbl = "Inversion " <> invId.fromId
  send $ TransferFiles lbl scratch.remote DKIST.remote [fileTransfer source]
 where
  fileTransfer :: Path Scratch (Mounted Dir) Inversion -> FileTransfer Scratch DKIST Dir Inversion
  fileTransfer src =
    FileTransfer
      { sourcePath = Path src.filePath
      , destPath = DKIST.softPublishDir propId invId
      , recursive = True
      }


-- transfer inversion files proper place on DKIST Data Globus
--   needs to match dataset .bucket: (data | public)
--   path: <proposal_id>/<inversion_id>
--   example: pid_2_114/inv.UR5P96/xxx.fits, pid_2_114/inv.UR5P96/xxx.asdf
transferPublish
  :: (Transfer :> es, Scratch :> es)
  => Bucket
  -> App.Id Proposal
  -> App.Id Inversion
  -> Eff es (App.Id Task)
transferPublish bucket propId invId = do
  scratch <- Scratch.remote
  let lbl = "Publish " <> invId.fromId
  source <- Scratch.mountedPath (Image.outputL2Dir propId invId)
  let dest = DKIST.publishDir bucket propId invId
  send $ TransferFiles lbl scratch.remote DKIST.remote [fileTransfer source dest]
 where
  fileTransfer :: Path Scratch (Mounted Dir) Inversion -> Path DKIST Dir Inversion -> FileTransfer Scratch DKIST Dir Inversion
  fileTransfer src destPath =
    FileTransfer
      { sourcePath = Path src.filePath
      , destPath = destPath
      , recursive = True
      }

module App.Effect.Transfer where

import Data.Tagged
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Log
import NSO.Files as Files
import NSO.Files.DKIST as DKIST
import NSO.Files.Image qualified as Image
import NSO.Files.RemoteFolder
import NSO.Files.Scratch qualified as Scratch
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
  :: (Globus :> es, Log :> es)
  => Token Access
  -> Eff (Transfer : es) a
  -> Eff es a
runTransfer acc = interpret $ \_ -> \case
  TransferStatus taskId -> do
    send $ StatusTask acc (Tagged taskId.fromId)
  TransferFiles lbl source dest files ->
    transferFiles lbl source dest files
 where
  transferFiles :: (Log :> es, Globus :> es) => Text -> Remote src -> Remote dest -> [FileTransfer src dest f a] -> Eff es (Id Task)
  transferFiles lbl source dest files = do
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


{- | Run Transfer, requiring auth, catching and displaying globus errors
requireTransfer :: (Log :> es, Auth :> es, Globus :> es, Hyperbole :> es) => Eff (Transfer : Reader (Token Access) : es) a -> Eff es a
requireTransfer eff =
  Auth.requireLogin $ runTransfer eff
-}
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
  downloadL1To lbl Image.dataset scratch ds


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
  xfers <- sequence $ catMaybes [fileTransfer upfiles.quantities, fileTransfer upfiles.profileFit, fileTransfer upfiles.profileOrig]
  send $ TransferFiles tform.label source scratch xfers
 where
  fileTransfer :: (Scratch :> es) => Maybe (Path Scratch Filename a) -> Maybe (Eff es (FileTransfer User Scratch File Inversion))
  fileTransfer Nothing = Nothing
  fileTransfer (Just p@(Path a)) =
    pure $ do
      Path mp <- Scratch.mountedPath $ Image.blancaFile (Image.blancaInput propId invId) p
      pure $
        FileTransfer
          { sourcePath = TransferForm.directory tform </> Path a
          , destPath = Path mp
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
  -- \$ Files.outputParentProposalDir propId
  -- \$ DKIST.proposalPublishDir propId
  let lbl = "Inversion " <> invId.fromId
  -- let ft = DKIST.inversion invId
  send $ TransferFiles lbl scratch DKIST.remote [fileTransfer]
 where
  fileTransfer :: FileTransfer Scratch DKIST Dir Inversion
  fileTransfer =
    FileTransfer
      { sourcePath = Image.outputL2Dir propId invId
      , destPath = DKIST.publishedDir propId invId
      , recursive = True
      }

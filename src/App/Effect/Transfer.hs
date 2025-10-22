module App.Effect.Transfer where

import App.Effect.Auth (Auth)
import App.Effect.Auth qualified as Auth
import Control.Monad.Loops (untilM_)
import Data.Either (lefts)
import Data.List as L (isPrefixOf, stripPrefix)
import Data.Tagged
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Debug as Debug (Debug, delay)
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Files.DKIST as DKIST
import NSO.Files.Image qualified as Image
import NSO.Files.Inversion (InversionFiles (..))
import NSO.Files.RemoteFolder (Remote (..), remotePath)
import NSO.Files.Scratch (Scratch)
import NSO.Files.Scratch qualified as Scratch
import NSO.Files.TransferForm (DownloadFolder (..), TransferForm (..), User)
import NSO.Files.TransferForm qualified as TransferForm
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import System.FileCopy (copyRecursive)


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
  RemoteLevel1 :: Transfer m (Remote Level1)
  RemotePublish :: Transfer m (Remote Publish)
  RemoteScratch :: Transfer m (Remote Scratch)
type instance DispatchOf Transfer = 'Dynamic


runTransfer
  :: (Globus :> es, Log :> es, Auth :> es, Scratch :> es, IOE :> es, Debug :> es)
  => Remote Level1
  -> Remote Publish
  -> Remote Scratch
  -> Eff (Transfer : es) a
  -> Eff es a
runTransfer level1 publish scratch = interpret $ \_ -> \case
  TransferStatus taskId -> do
    if taskId == Id fakeLocalTask.task_id.unTagged
      then do
        Debug.delay 1000
        pure fakeLocalTask
      else do
        Auth.waitForAdmin $ do
          acc <- ask @(Token Access)
          send $ StatusTask acc (Tagged taskId.fromId)
  TransferFiles lbl source dest files ->
    Auth.waitForAdmin $ do
      transferFiles lbl source dest files
  RemoteLevel1 -> pure level1
  RemotePublish -> pure publish
  RemoteScratch -> pure scratch
 where
  transferFiles
    :: (Log :> es, Globus :> es, Reader (Token Access) :> es, Scratch :> es, IOE :> es)
    => Text
    -> Remote src
    -> Remote dest
    -> [FileTransfer src dest f a]
    -> Eff es (Id Task)
  transferFiles lbl source dest files = do
    let items = fmap transferItem files
    if isLocalScratch
      then transferLocalScratch items
      else transferRemote items
   where
    isLocalScratch =
      source.collection == scratch.collection && dest.collection == scratch.collection

    -- https://localhost/proposal/pid_2_114/program/id.156511.623969/upload/inv.3RH5LC?profileOrig=Uploaded
    transferLocalScratch :: (Log :> es, Scratch :> es, IOE :> es) => [TransferItem] -> Eff es (Id Task)
    transferLocalScratch tfers = do
      log Debug "TRANSFER LOCAL SCRATCH"
      log Debug $ dump " source: " source
      log Debug $ dump " dest: " dest
      res <- forM tfers $ \t -> do
        -- log Debug $ "  xfer1: " <> t.source_path <> " " <> t.destination_path
        src <- localSource source t.source_path
        dst <- localSource dest t.destination_path
        log Debug $ "  xfer: " <> src <> " " <> dst
        copyRecursive src dst
      case lefts res of
        [] -> pure ()
        (l : _) -> throwIO $ LocalCopyFailed l
      pure $ Id fakeLocalTask.task_id.unTagged

    -- WARNING: for this to work, the remote must be set to the exact same thing as scratch
    --   GLOBUS_PUBLISH=globus://03232d38-5e57-11ef-b967-17fffa478f3e/Data/level2
    --   GLOBUS_SCRATCH=globus://03232d38-5e57-11ef-b967-17fffa478f3e/Data/level2
    localSource :: (Scratch :> es, Log :> es) => Remote sys -> FilePath -> Eff es FilePath
    localSource remote src
      -- if the remote IS scratch, get its mounted path
      | remote.directory.filePath == scratch.directory.filePath = do
          let cleanPath = maybe (Path src) (Path . dropWhile (== '/')) $ stripPrefix scratch.directory.filePath src
          mnt <- Scratch.mountedPath cleanPath
          log Debug $ " local scratch " <> src <> " " <> cleanPath.filePath <> " " <> mnt.filePath
          pure mnt.filePath
      -- if the remote isn't, assume the remote directory is absolute
      | "/" `isPrefixOf` remote.directory.filePath =
          pure (remote.directory </> Path src).filePath
      | otherwise = do
          throwIO $ LocalCopyNoAbsolutePath remote.collection remote.directory.filePath

    transferRemote :: (Reader (Token Access) :> es, Log :> es, Globus :> es) => [TransferItem] -> Eff es (Id Task)
    transferRemote items = do
      acc <- ask @(Token Access)
      log Debug "TRANSFER REMOTE"
      log Debug $ dump " source: " source
      log Debug $ dump " dest: " dest
      log Debug $ dump " files: " files
      sub <- send $ Globus.GetSubmissionId acc
      let req = transferRequest sub
      log Debug $ dump " items: " $ fmap (\t -> (t.source_path, t.destination_path)) items
      res <- send $ Globus.Transfer acc req
      pure $ Id res.task_id.unTagged

    transferItem FileTransfer{sourcePath, destPath, recursive} =
      -- this must be the relative globus path
      TransferItem
        { data_type = DataType
        , source_path = (remotePath source sourcePath).filePath
        , destination_path = (remotePath dest destPath).filePath
        , recursive
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

  fakeLocalTask :: Task
  fakeLocalTask =
    Task
      { status = Succeeded
      , task_id = Tagged "local"
      , label = "local"
      , files = 1
      , directories = 1
      , files_skipped = 0
      , files_transferred = 1
      , bytes_transferred = 1
      , bytes_checksummed = 1
      , effective_bytes_per_second = 1
      , nice_status = Nothing
      , source_endpoint_id = Tagged "source"
      , destination_endpoint_id = Tagged "dest"
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
  downloadL1To lbl Image.dataset scratch ds


downloadL1To
  :: forall dest es
   . (Transfer :> es)
  => Text
  -> (Dataset -> Path dest File Dataset)
  -> Remote dest
  -> [Dataset]
  -> Eff es (Id Task)
downloadL1To lbl toDestPath dest ds = do
  let xfers = fmap datasetTransfer ds
  source <- send RemoteLevel1
  send $ TransferFiles lbl source dest xfers
 where
  datasetTransfer :: Dataset -> FileTransfer Level1 dest File Dataset
  datasetTransfer d =
    FileTransfer
      { sourcePath = DKIST.dataset d
      , destPath = toDestPath d
      , recursive = True
      }


-- Upload ---------------------------------------------------------------------------------

uploadInversionResults
  :: (Transfer :> es, Scratch :> es, Log :> es)
  => TransferForm
  -> InversionFiles Maybe Filename
  -> App.Id Proposal
  -> App.Id Inversion
  -> Eff es (Id Task)
uploadInversionResults tform upfiles propId invId = do
  scratch <- Scratch.remote
  let source :: Remote User = TransferForm.remote tform
  send $ TransferFiles tform.label source scratch (fileTransfers scratch)
 where
  fileTransfers :: Remote Scratch -> [FileTransfer User Scratch File Inversion]
  fileTransfers scratch = catMaybes [fmap (fileTransfer scratch) upfiles.quantities, fmap (fileTransfer scratch) upfiles.profileFit, fmap (fileTransfer scratch) upfiles.profileOrig]
   where
    fileTransfer :: Remote Scratch -> Path Scratch Filename a -> FileTransfer User Scratch File Inversion
    fileTransfer _folder p@(Path a) =
      FileTransfer
        { sourcePath = Path a
        , destPath = Image.blancaFile (Image.blancaInput propId invId) p
        , recursive = False
        }


-- Publish -------------------------------------------------------------------------------

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
  dest <- send RemotePublish
  let lbl = "Publish " <> invId.fromId
  let sourcePath = Image.outputL2Dir propId invId
  let destPath = DKIST.publishDir bucket propId invId
  let transferItem = FileTransfer{sourcePath, destPath, recursive = True}
  send $ TransferFiles lbl scratch dest [transferItem]


data TransferException
  = LocalCopyFailed String
  | LocalCopyNoAbsolutePath (Globus.Id Collection) String
  deriving (Show, Eq, Exception)


-- helpers ------------------------------------------------------

waitForTransfer
  :: forall err es
   . (Concurrent :> es, Transfer :> es, Error err :> es, Show err)
  => (Task -> err)
  -> Id Globus.Task
  -> Eff es ()
waitForTransfer toError taskId = do
  untilM_ delay2s taskComplete
 where
  taskComplete :: Eff es Bool
  taskComplete = do
    tsk <- transferStatus taskId
    case tsk.status of
      Failed -> throwError @err $ toError tsk
      Succeeded -> pure True
      _ -> pure False

  delay2s = threadDelay $ 2 * 1000 * 1000

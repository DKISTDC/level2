{-# LANGUAGE AllowAmbiguousTypes #-}

module App.Effect.Transfer where

import App.Effect.Auth (Auth)
import App.Effect.GlobusAccess
import Control.Monad.Loops (untilM_)
import Data.Either (lefts)
import Data.List qualified as L
import Data.Tagged
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Log
import NSO.Files
import NSO.Files.DKIST as DKIST
import NSO.Files.Image qualified as Image
import NSO.Files.RemoteFolder (Remote (..), remotePath)
import NSO.Files.Scratch (Scratch (..))
import NSO.Files.Scratch qualified as Scratch
import NSO.Files.TransferForm qualified as TransferForm
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.Dataset
import NSO.Types.Inversion (Inversion)
import NSO.Types.Proposal (Proposal)
import NSO.Types.User (CurrentAccess (..))
import System.FileCopy (copyRecursive)


-- 1. User -> Scratch Workspace (Ingest)
-- 2. Level1 -> Scratch Workspace (Output)
-- 3. Scratch Clean ->  DKIST Data (publish)

data FileTransfer sys dest f a = FileTransfer
  { sourcePath :: Path sys f a
  , destPath :: Path dest f a
  , recursive :: Bool
  }
  deriving (Show)


-- RemoteLevel1 :: Transfer remotes m (Remote Level1)
-- RemotePublish :: Transfer remotes m (Remote Publish)
-- RemoteScratch :: Transfer remotes m (Remote Scratch)

data Transfer src dest :: Effect where
  -- Just run a reader for them
  RemoteSource :: Transfer src dest m (Remote src)
  RemoteDest :: Transfer src dest m (Remote dest)
  TransferFiles :: Text -> [FileTransfer src dest f a] -> Transfer src dest m (Id Task)
  TransferStatus :: Id Task -> Transfer src dest m Task


type instance DispatchOf (Transfer src dest) = 'Dynamic


runTransferSkip
  :: forall src dest a es
   . ( Globus :> es
     , GlobusAccess src :> es
     , GlobusAccess dest :> es
     , Log :> es
     )
  => Remote src
  -> Remote dest
  -> Eff (Transfer src dest : es) a
  -> Eff es a
runTransferSkip source dest = interpret $ \_ -> \case
  RemoteSource -> pure source
  RemoteDest -> pure dest
  TransferFiles _lbl _files -> pure (Id "fake")
  TransferStatus tid -> pure $ fakeLocalTask tid


-- we KNOW that this transfer is local
-- User -> Ingest
-- Output -> Publish (behaves differently!)
runTransferLocalDebug
  :: forall src dest a es
   . ( Globus :> es
     , GlobusAccess src :> es
     , GlobusAccess dest :> es
     , Scratch Ingest :> es
     , IOE :> es
     , Log :> es
     )
  => Remote src
  -> Remote dest
  -> Eff (Transfer src dest : es) a
  -> Eff es a
runTransferLocalDebug source dest = interpret $ \_ -> \case
  RemoteSource -> pure source
  RemoteDest -> pure dest
  TransferStatus tid -> pure $ fakeLocalTask tid
  TransferFiles _lbl files -> transferLocalScratch $ fmap (transferItem source dest) files
   where
    transferLocalScratch :: (Log :> es, Scratch Ingest :> es, IOE :> es) => [TransferItem] -> Eff es (Id Task)
    transferLocalScratch tfers = do
      log Debug "TRANSFER LOCAL SCRATCH"
      log Debug $ dump " source: " source
      log Debug $ dump " dest: " dest
      res <- forM tfers $ \t -> do
        -- log Debug $ "  xfer1: " <> t.source_path <> " " <> t.destination_path
        -- WARNING: this works differently for User -> Ingest and Output -> Publish
        -- the latter needs to get a correct local path?
        -- test it
        -- src <- localSource source t.source_path
        let src = t.source_path
        dst <- localSource dest t.destination_path
        log Debug $ "  xfer: " <> src <> " " <> dst
        copyRecursive src dst
      case lefts res of
        [] -> pure ()
        (l : _) -> throwIO $ LocalCopyFailed l
      pure $ Id (fakeLocalTask (Id "local")).task_id.unTagged

    -- no... we already have the rightr source path
    localSource :: (Scratch Ingest :> es, Log :> es) => Remote sys -> FilePath -> Eff es FilePath
    localSource remote src = do
      let cleanPath = maybe (Path src) (Path . dropWhile (== '/')) $ L.stripPrefix remote.directory.filePath src
      mnt <- Scratch.mountedPath @Ingest cleanPath
      log Debug $ " local scratch " <> src <> " " <> cleanPath.filePath <> " " <> mnt.filePath
      pure mnt.filePath


-- if the remote IS scratch, get its mounted path
-- \| remote.directory.filePath == scratch.directory.filePath = do
-- if the remote isn't, assume the remote directory is absolute
-- \| "/" `isPrefixOf` remote.directory.filePath =
--     pure (remote.directory </> Path src).filePath
-- \| otherwise = do
--     throwIO $ LocalCopyNoAbsolutePath remote.collection remote.directory.filePath

runTransfer
  :: forall src dest a es
   . ( Globus :> es
     , GlobusAccess src :> es
     , GlobusAccess dest :> es
     , Log :> es
     -- , Concurrent :> es
     )
  => Remote src
  -> Remote dest
  -> Eff (Transfer src dest : es) a
  -> Eff es a
runTransfer source dest = interpret $ \_ -> \case
  RemoteSource -> pure source
  RemoteDest -> pure dest
  TransferStatus tid -> send $ TaskStatus @src tid
  TransferFiles lbl files -> do
    -- source <- send GetRemote
    -- dest <- send GetRemote
    let items = fmap (transferItem source dest) files
    acc <- getCurrentAccess @src
    transferRemote acc items
   where
    -- if isLocalScratch
    --   then transferLocalScratch items
    --   else transferRemote items

    -- isLocalScratch =
    --   source.collection == scratch.collection && dest.collection == scratch.collection
    --
    -- -- https://localhost/proposal/pid_2_114/program/id.156511.623969/upload/inv.3RH5LC?profileOrig=Uploaded
    -- transferLocalScratch :: (Log :> es, Scratch :> es, IOE :> es) => [TransferItem] -> Eff es (Id Task)
    -- transferLocalScratch tfers = do
    --   log Debug "TRANSFER LOCAL SCRATCH"
    --   log Debug $ dump " source: " source
    --   log Debug $ dump " dest: " dest
    --   res <- forM tfers $ \t -> do
    --     -- log Debug $ "  xfer1: " <> t.source_path <> " " <> t.destination_path
    --     src <- localSource source t.source_path
    --     dst <- localSource dest t.destination_path
    --     log Debug $ "  xfer: " <> src <> " " <> dst
    --     copyRecursive src dst
    --   case lefts res of
    --     [] -> pure ()
    --     (l : _) -> throwIO $ LocalCopyFailed l
    --   pure $ Id fakeLocalTask.task_id.unTagged
    --

    transferRemote :: (Log :> es, Globus :> es) => CurrentAccess -> [TransferItem] -> Eff es (Id Task)
    transferRemote acc items = do
      -- source <- send GetRemote
      -- dest <- send GetRemote
      log Debug "TRANSFER REMOTE"
      log Debug $ dump " source: " source
      log Debug $ dump " dest: " dest
      log Debug $ dump " files: " files
      sub <- send $ Globus.GetSubmissionId acc.token
      let req = transferRequest source dest lbl files sub
      log Debug $ dump " items: " $ fmap (\t -> (t.source_path, t.destination_path)) items
      res <- send $ Globus.Transfer acc.token req
      pure $ Id res.task_id.unTagged


transferItem :: Remote src -> Remote dest -> FileTransfer src dest f x -> TransferItem
transferItem source dest FileTransfer{sourcePath, destPath, recursive} =
  -- this must be the relative globus path
  TransferItem
    { data_type = DataType
    , source_path = (remotePath source sourcePath).filePath
    , destination_path = (remotePath dest destPath).filePath
    , recursive
    }


transferRequest :: Remote src -> Remote dest -> Text -> [FileTransfer src dest a x] -> Globus.Id Submission -> TransferRequest
transferRequest source dest lbl files submission_id =
  let items = fmap (transferItem source dest) files
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


fakeLocalTask :: Id Task -> Task
fakeLocalTask tid =
  Task
    { status = Succeeded
    , task_id = Tagged tid.fromId
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


--

-- remoteSource :: forall src dest es. Transfer src dest :> es => Eff es (Remote src)
-- remoteSource = send (RemoteSource @src @dest)
--
-- remoteDestination :: forall src dest es. Transfer src dest :> es => Eff es (Remote dest)
-- remoteDestination = send (RemoteDest @src @dest)

-- Download Datasets ----------------------------------------------------------------------

-- userDownloadDatasets :: (Transfer :> es) => TransferForm -> DownloadFolder -> [Dataset] -> Eff es (Id Task)
-- userDownloadDatasets tform df ds = do
--   let dest = TransferForm.remote tform
--   downloadL1To tform.label (TransferForm.dataset tform df) dest ds

scratchDownloadDatasets :: (Transfer Level1 Ingest :> es, Scratch Ingest :> es) => [Dataset] -> Eff es (Id Task)
scratchDownloadDatasets ds = do
  let lbl = "Sync Datasets: " <> T.intercalate "," (fmap (\d -> d.datasetId.fromId) ds)
  downloadL1To lbl Image.dataset ds


downloadL1To
  :: forall dest es
   . (Transfer Level1 dest :> es)
  => Text
  -> (Dataset -> Path dest Dir Dataset)
  -> [Dataset]
  -> Eff es (Id Task)
downloadL1To lbl toDestPath ds = do
  let xfers = fmap datasetTransfer ds
  send $ TransferFiles lbl xfers
 where
  datasetTransfer :: Dataset -> FileTransfer Level1 dest Dir Dataset
  datasetTransfer d =
    FileTransfer
      { sourcePath = DKIST.dataset d
      , destPath = toDestPath d
      , recursive = True
      }


-- Upload ---------------------------------------------------------------------------------

-- TODO: fix me
uploadInversionResults
  :: (Globus :> es, Auth :> es, GlobusAccess User :> es, Scratch Ingest :> es, Log :> es, GlobusAccess Ingest :> es, IOE :> es)
  => TransferForm
  -> InversionFiles Maybe Filename
  -> App.Id Proposal
  -> App.Id Inversion
  -> Eff es (Id Task)
uploadInversionResults tform upfiles propId invId = do
  ingest <- send GetConfig
  let source :: Remote User = TransferForm.remote tform
  -- runTransfer @User @Ingest source ingest.remote $ do
  runTransferLocalDebug @User @Ingest source ingest.remote $ do
    send $ TransferFiles tform.label (fileTransfers ingest.remote)
 where
  fileTransfers :: Remote Ingest -> [FileTransfer User Ingest File Inversion]
  fileTransfers ingest = catMaybes [fmap (fileTransfer ingest) upfiles.quantities, fmap (fileTransfer ingest) upfiles.profileFit, fmap (fileTransfer ingest) upfiles.profileOrig]
   where
    fileTransfer :: Remote Ingest -> Path Ingest Filename a -> FileTransfer User Ingest File Inversion
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
  :: (Transfer Output Publish :> es, Scratch Output :> es)
  => Bucket
  -> App.Id Proposal
  -> App.Id Inversion
  -> Eff es (App.Id Task)
transferPublish bucket propId invId = do
  -- scratch <- Scratch.remote
  -- dest <- send RemotePublish
  let lbl = "Publish " <> invId.fromId
  let sourcePath = Image.outputL2Dir propId invId
  let destPath = bucketedInversionPath $ DKIST.publishDir bucket propId invId
  let item = FileTransfer{sourcePath, destPath, recursive = True}
  send $ TransferFiles lbl [item]
 where
  bucketedInversionPath :: Path Publish Dir (Bucketed Inversion) -> Path Publish Dir Inversion
  bucketedInversionPath (Path f) = Path f


data TransferException
  = LocalCopyFailed String
  | LocalCopyNoAbsolutePath (Globus.Id Collection) String
  deriving (Show, Eq, Exception)


waitForTransfer
  :: forall src dest err es
   . (Concurrent :> es, Transfer src dest :> es, Error err :> es, Show err)
  => (Task -> err)
  -> Id Globus.Task
  -> Eff es ()
waitForTransfer toError taskId = do
  untilM_ delay2s taskComplete
 where
  taskComplete :: Eff es Bool
  taskComplete = do
    tsk <- send $ TransferStatus @src @dest taskId
    case tsk.status of
      Failed -> throwError @err $ toError tsk
      Succeeded -> pure True
      _ -> pure False

  delay2s = threadDelay $ 2 * 1000 * 1000

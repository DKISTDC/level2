module App.Effect.Transfer where

import Data.List.NonEmpty qualified as NE
import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Reader.Dynamic
import NSO.Files as Files
import NSO.Files.DKIST as DKIST
import NSO.Files.Image qualified as Files
import NSO.Files.RemoteFolder
import NSO.Files.Scratch (Scratch)
import NSO.Files.Scratch qualified as Scratch
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import Web.FormUrlEncoded qualified as FUE
import Web.Hyperbole


data Transfer :: Effect where
  -- download: DKIST to local
  -- upload: local to Scratch
  -- generate: DKIST to Scratch
  TransferFiles :: Text -> RemoteFolder src a -> RemoteFolder dest a -> [Path any Filename a] -> Transfer m (Id Task)
  TransferStatus :: Id Task -> Transfer m Task
type instance DispatchOf Transfer = 'Dynamic


-- maybe we should force the user to handle globus errors...
runTransfer
  :: (Globus :> es, Reader (Token Access) :> es)
  => Eff (Transfer : es) a
  -> Eff es a
runTransfer = interpret $ \_ -> \case
  TransferStatus taskId -> do
    acc <- ask
    send $ StatusTask acc (Tagged taskId.fromId)
  TransferFiles lbl source dest files -> do
    acc <- ask
    sub <- send $ Globus.GetSubmissionId acc
    res <- send $ Globus.Transfer acc $ transferRequest lbl source dest files sub
    pure $ Id res.task_id.unTagged
 where
  transferItem :: RemoteFolder src a -> RemoteFolder dest a -> Path any Filename a -> TransferItem
  transferItem source dest (Path file) =
    TransferItem
      { data_type = DataType
      , source_path = (source.directory </> Path file).filePath
      , destination_path = (dest.directory </> Path file).filePath
      , recursive = True
      }

  transferRequest :: Text -> RemoteFolder src a -> RemoteFolder dest a -> [Path any Filename a] -> Globus.Id Submission -> TransferRequest
  transferRequest lbl source dest files submission_id =
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


transferStatus :: (Transfer :> es) => Id Task -> Eff es Task
transferStatus = send . TransferStatus


-- Download Datasets ----------------------------------------------------------------------

userDownloadDatasets :: (Transfer :> es) => TransferForm -> DownloadFolder -> NonEmpty Dataset -> Eff es (Id Task)
userDownloadDatasets tform df ds = do
  let dest = Files.remoteDatasets tform df (head ds)
  downloadL1To tform.label dest ds


scratchDownloadDatasets :: (Transfer :> es, Scratch :> es) => NonEmpty Dataset -> Eff es (Id Task)
scratchDownloadDatasets ds = do
  dest <- Scratch.remote $ Files.datasetParentFolder (head ds)
  let lbl = "Datasets: " <> cs (show (fmap (.datasetId) ds))
  downloadL1To lbl dest ds


downloadL1To :: (Transfer :> es) => Text -> RemoteFolder dest Dataset -> NonEmpty Dataset -> Eff es (Id Task)
downloadL1To lbl dest ds = do
  let source = DKIST.remote $ DKIST.datasetParentFolder (head ds)
      paths = fmap (\d -> Path (cs d.datasetId.fromId)) ds
  send $ TransferFiles lbl source dest (NE.toList paths)


-- Upload ---------------------------------------------------------------------------------

uploadInversionResults
  :: (Transfer :> es, Scratch :> es)
  => TransferForm
  -> InversionFiles Maybe Filename
  -> App.Id Proposal
  -> App.Id Inversion
  -> Eff es (Id Task)
uploadInversionResults tform upfiles propId invId = do
  dest <- Scratch.remote $ Files.blancaInput propId invId
  let source = Files.remoteTransfer tform
  let files = catMaybes [filename upfiles.quantities, filename upfiles.profileFit, filename upfiles.profileOrig]
  send $ TransferFiles tform.label source dest files
 where
  filename :: Maybe (Path s Filename a) -> Maybe (Path s Filename Inversion)
  filename (Just (Path a)) = Just $ Path a
  filename Nothing = Nothing


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
  source <- Scratch.remote $ Files.outputParentProposalDir propId
  let dest = DKIST.remote $ DKIST.proposalPublishDir propId
  let lbl = "Inversion " <> invId.fromId
  send $ TransferFiles lbl source dest [DKIST.inversion invId]

--  initTransfer (transfer scratch)
-- where
--  transfer :: Globus.Id Collection -> Globus.Id Submission -> TransferRequest
--  transfer scratch submission_id =
--    let src = Files.outputL2Dir propId invId
--        dst = publishedDir propId invId
--     in TransferRequest
--          { data_type = DataType
--          , submission_id
--          , label = Just $ "Inversion " <> invId.fromId
--          , source_endpoint = scratch
--          , destination_endpoint = dkistEndpoint
--          , data_ = [inversionTransferItem src dst]
--          , sync_level = SyncTimestamp
--          , store_base_path_info = True
--          }

-- inversionTransferItem :: Path' Dir Inversion -> Path' Dir (Published Inversion) -> TransferItem
-- inversionTransferItem src dest =
--   TransferItem
--     { data_type = DataType
--     , source_path = src.filePath
--     , destination_path = dest.filePath
--     , recursive = True
--     }

-- Endpoints --------------------------------------------------------

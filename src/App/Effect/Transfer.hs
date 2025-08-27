module App.Effect.Transfer where

import Data.List.NonEmpty qualified as NE
import Data.Tagged
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Files as Files
import NSO.Files.DKIST as DKIST
import NSO.Files.Image qualified as Files
import NSO.Files.RemoteFolder
import NSO.Files.Scratch qualified as Scratch
import NSO.Prelude
import NSO.Types.Common as App
import App.Effect.Auth
import NSO.Types.Dataset
import Control.Monad.Catch
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import App.View.Error (userFacingError)
import Web.Hyperbole

data Transfer :: Effect where
  -- download: DKIST to local
  -- upload: local to Scratch
  -- generate: DKIST to Scratch
  TransferFiles :: Text -> Bool -> RemoteFolder src a -> RemoteFolder dest a -> [Path any f a] -> Transfer m (Id Task)
  TransferStatus :: Id Task -> Transfer m Task
type instance DispatchOf Transfer = 'Dynamic


-- maybe we should force the user to handle globus errors...
runTransfer
  :: (Globus :> es, Reader (Token Access) :> es, Log :> es)
  => Eff (Transfer : es) a
  -> Eff es a
runTransfer = interpret $ \_ -> \case
  TransferStatus taskId -> do
    acc <- ask @(Token Access)
    send $ StatusTask acc (Tagged taskId.fromId)
  TransferFiles lbl recursive source dest files ->
    transferFiles lbl recursive source dest files
 where
  transferFiles :: (Log :> es, Reader (Token Access) :> es, Globus :> es) => Text -> Bool -> RemoteFolder src a -> RemoteFolder dest a -> [Path any f a] -> Eff es (Id Task)
  transferFiles lbl recursive source dest files = do
    log Debug "TRANSFER"
    log Debug $ dump " source: " source
    log Debug $ dump " dest: " dest
    log Debug $ dump " files: " files
    acc <- ask
    sub <- send $ Globus.GetSubmissionId acc
    let items = fmap (transferItem recursive source dest) files
    let req = transferRequest lbl recursive source dest files sub
    log Debug $ dump " items: " $ fmap (\t -> (t.source_path, t.destination_path)) items
    res <- send $ Globus.Transfer acc req
    pure $ Id res.task_id.unTagged

  transferItem :: Bool -> RemoteFolder src a -> RemoteFolder dest a -> Path any f a -> TransferItem
  transferItem recursive source dest (Path file) =
    TransferItem
      { data_type = DataType
      , source_path = (source.directory </> Path file).filePath
      , destination_path = (dest.directory </> Path file).filePath
      , -- because it's recursive?
        recursive
      }

  transferRequest :: Text -> Bool -> RemoteFolder src a -> RemoteFolder dest a -> [Path any f a] -> Globus.Id Submission -> TransferRequest
  transferRequest lbl recursive source dest files submission_id =
    let items = fmap (transferItem recursive source dest) files
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


-- | Run Transfer, requiring auth, catching and displaying globus errors
requireTransfer :: (Log :> es, Auth :> es, Globus :> es, Hyperbole :> es) => Eff (Transfer : Reader (Token Access) : es) a -> Eff es a
requireTransfer eff =
  catch (requireLogin $ runTransfer eff) (userFacingError @GlobusError)



transferStatus :: (Transfer :> es) => Id Task -> Eff es Task
transferStatus = send . TransferStatus


-- Download Datasets ----------------------------------------------------------------------

userDownloadDatasets :: (Transfer :> es) => TransferForm -> DownloadFolder -> NonEmpty Dataset -> Eff es (Id Task)
userDownloadDatasets tform df ds = do
  let dest = Files.remoteDatasets tform df
  downloadL1To tform.label dest ds


scratchDownloadDatasets :: (Transfer :> es, Scratch :> es) => NonEmpty Dataset -> Eff es (Id Task)
scratchDownloadDatasets ds = do
  dest <- Scratch.remote $ Files.datasetParentFolder (head ds)
  let lbl = "Sync Datasets: " <> T.intercalate "," (NE.toList $ fmap (\d -> d.datasetId.fromId) ds)
  downloadL1To lbl dest ds


downloadL1To :: (Transfer :> es) => Text -> RemoteFolder dest Dataset -> NonEmpty Dataset -> Eff es (Id Task)
downloadL1To lbl dest ds = do
  let source = DKIST.remote $ DKIST.datasetParentFolder (head ds)
      paths = fmap (\d -> Path (cs d.datasetId.fromId)) ds
  send $ TransferFiles lbl True source dest (NE.toList paths)


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
  send $ TransferFiles tform.label False source dest files
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
  send $ TransferFiles lbl True source dest [DKIST.inversion invId]

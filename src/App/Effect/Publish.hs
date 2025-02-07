module App.Effect.Publish where

import App.Effect.Scratch (Scratch, outputL2Dir)
import App.Globus (Globus, Task, Token, Token' (..), dkistEndpoint, fileManagerOpenDir, initTransfer, scratchCollection)
import Effectful
import Effectful.Reader.Dynamic
import NSO.Image.Frame (L2Frame)
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Network.Globus as Globus (DataType (..), Id, Id' (..), SyncLevel (..), TransferItem (..), TransferRequest (..))
import Web.View.Types.Url (Url)


data Published a
data SoftPublish


baseDir :: Path' Dir SoftPublish
baseDir = Path "etc/data1653/L2"


publishedDir :: App.Id Proposal -> App.Id Inversion -> Path' Dir (Published Inversion)
publishedDir ip ii =
  baseDir </> Path (cs ip.fromId) </> Path (cs ii.fromId)


fileManagerOpenPublish :: Path' Dir (Published Inversion) -> Url
fileManagerOpenPublish = fileManagerOpenDir (App.Id dkistEndpoint.unTagged)


-- /level2/generated/pid_1_118/inv.SD9T3L/
-- /level2/generated/pid_1_118/inv.SD9T3L/
transferSoftPublish
  :: (Globus :> es, Reader (Token Access) :> es, Scratch :> es)
  => App.Id Proposal
  -> App.Id Inversion
  -> Eff es (App.Id Task)
transferSoftPublish propId invId = do
  scratch <- scratchCollection
  initTransfer (transfer scratch)
 where
  transfer :: Globus.Id Collection -> Globus.Id Submission -> TransferRequest
  transfer scratch submission_id =
    let src = outputL2Dir propId invId
        dst = publishedDir propId invId
     in TransferRequest
          { data_type = DataType
          , submission_id
          , label = Just $ "Inversion " <> invId.fromId
          , source_endpoint = scratch
          , destination_endpoint = dkistEndpoint
          , data_ = [inversionTransferItem src dst]
          , sync_level = SyncTimestamp
          , store_base_path_info = True
          }


inversionTransferItem :: Path' Dir L2Frame -> Path' Dir (Published Inversion) -> TransferItem
inversionTransferItem src dest =
  TransferItem
    { data_type = DataType
    , source_path = src.filePath
    , destination_path = dest.filePath
    , recursive = True
    }

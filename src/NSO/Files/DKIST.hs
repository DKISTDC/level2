module NSO.Files.DKIST where

import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import NSO.Files.RemoteFolder
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)


-- DKIST -----------------------------------------
data DKIST


remote :: Path DKIST Dir a -> RemoteFolder DKIST a
remote d =
  RemoteFolder
    { collection = endpoint
    , directory = d
    }


endpoint :: Globus.Id Collection
endpoint = Tagged "d26bd00b-62dc-40d2-9179-7aece2b8c437"


-- datasetSourcePath :: Dataset -> Path DKIST Dir Dataset
-- datasetSourcePath d = Path (cs d.bucket) </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)

datasetParentFolder :: Dataset -> Path DKIST Dir Dataset
datasetParentFolder d = Path (cs d.bucket) </> Path (cs d.primaryProposalId.fromId)


-- Publish --------------------------------------

data SoftPublish


baseDir :: Path DKIST Dir SoftPublish
baseDir = Path "etc/data1653/L2"


proposalPublishDir :: Id Proposal -> Path DKIST Dir Inversion
proposalPublishDir ip =
  baseDir </> Path (cs ip.fromId)


inversion :: Id Inversion -> Path s Filename Inversion
inversion invId = Path (cs invId.fromId)

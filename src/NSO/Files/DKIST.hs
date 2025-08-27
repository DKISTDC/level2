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


remote :: Remote DKIST
remote =
  Remote{collection = endpoint}


endpoint :: Globus.Id Collection
endpoint = Tagged "d26bd00b-62dc-40d2-9179-7aece2b8c437"


-- datasetSourcePath :: Dataset -> Path DKIST Dir Dataset
-- datasetSourcePath d = Path (cs d.bucket) </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)

-- datasetParentFolder :: Dataset -> Path DKIST Dir Dataset
-- datasetParentFolder d = Path (cs d.bucket) </> Path (cs d.primaryProposalId.fromId)

dataset :: Dataset -> Path DKIST File Dataset
dataset d = Path (cs d.bucket) </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)


-- Publish --------------------------------------

data SoftPublish


-- is this part of the *PATH* ?
baseDir :: Path DKIST Dir SoftPublish
baseDir = Path "etc/data1653/L2"


publishedDir :: Id Proposal -> Id Inversion -> Path DKIST Dir Inversion
publishedDir propId invId = baseDir </> Path (cs propId.fromId) </> Path (cs invId.fromId)

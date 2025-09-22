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
data Level1


-- remote :: Remote Level1
-- remote =
--   Remote{collection = endpoint}
--
--
-- datasetFolder :: RemoteFolder DKIST ()
-- datasetFolder = RemoteFolder{remote, directory = Path ""}

-- endpoint :: Globus.Id Collection
-- endpoint = Tagged "d26bd00b-62dc-40d2-9179-7aece2b8c437"

-- datasetSourcePath :: Dataset -> Path DKIST Dir Dataset
-- datasetSourcePath d = Path (cs d.bucket.bucketName) </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)

-- datasetParentFolder :: Dataset -> Path DKIST Dir Dataset
-- datasetParentFolder d = Path (cs d.bucket.bucketName) </> Path (cs d.primaryProposalId.fromId)

dataset :: Dataset -> Path Level1 File Dataset
dataset d = Path (cs d.bucket.bucketName) </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)


-- Publish --------------------------------------

data Publish


-- is this part of the *PATH* ?
-- baseDir :: Path DKIST Dir SoftPublish
-- baseDir = Path "etc/data1653/L2"

-- softPublishDir :: Id Proposal -> Id Inversion -> Path Publish Dir Inversion
-- softPublishDir propId invId = Path (cs propId.fromId) </> Path (cs invId.fromId)

-- softPublishFolder :: Remote Publish SoftPublish
-- softPublishFolder = RemoteFolder{remote, directory = baseDir}

publishDir :: Bucket -> Id Proposal -> Id Inversion -> Path Publish Dir Inversion
publishDir bucket propId invId =
  bucketRoot bucket </> Path (cs propId.fromId) </> Path (cs invId.fromId)


bucketRoot :: Bucket -> Path Publish Dir ()
bucketRoot bucket = Path (cs bucket.bucketName)

-- publishFolder :: Bucket -> RemoteFolder DKIST Publish
-- publishFolder bucket = RemoteFolder{remote, directory = publishRoot bucket}

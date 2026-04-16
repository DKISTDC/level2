{-# LANGUAGE DerivingVia #-}

module App.Worker.SyncMetadata where

import Data.Grouped
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Sync as Sync
import NSO.Generic
import NSO.Metadata
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram


-- SYNC METADATA -----------------------------------------------

data SyncMetadataTask = SyncMetadataTask {syncId :: Id Sync}
  deriving (Generic, Eq)
  deriving (Show, Read) via (NoFields SyncMetadataTask)


instance WorkerTask SyncMetadataTask


needsMetadataSync :: (MetadataSync :> es, Time :> es) => Eff es Bool
needsMetadataSync = do
  syc <- Sync.lastSync
  now <- currentTime
  case syc of
    Nothing -> pure True
    Just s -> pure $ hasDayPassed s.started now
 where
  hasDayPassed earlier later = diffUTCTime later earlier >= nominalDay

  nominalDay :: NominalDiffTime
  nominalDay = 86400


syncMetadataTask
  :: ( Log :> es
     , MetadataSync :> es
     , Metadata es
     , Tasks :> es
     , Queue SyncProposalTask :> es
     )
  => SyncMetadataTask
  -> Eff es ()
syncMetadataTask task = do
  log Info "Scan Metadata for Proposals"
  taskSetStatus task True
  gs <- Sync.runScanAvailable
  let propIds = fmap (\g -> Id (sample g).primaryProposalId) gs
  send $ Sync.SetProposals task.syncId propIds
  let tasks = fmap (\g -> SyncProposalTask task.syncId $ Id (sample g).primaryProposalId) gs
  queueAddAll tasks


-- SYNC PROPOSAL -------------------------------------------------------------------------------------------------------------------

data SyncProposalTask = SyncProposalTask {syncId :: Id Sync, proposalId :: Id Proposal}
  deriving (Generic, Eq)
  deriving (Show, Read) via (NoFields SyncProposalTask)


instance WorkerTask SyncProposalTask where
  type Status SyncProposalTask = SyncProgress
  idle = Wait


data SyncProgress
  = Wait
  | Scan
  | Exec [ScanError] [SyncItem (Id Dataset)]
  deriving (Eq, Show, Read)


syncProposalTask :: (Error TaskFail :> es, Log :> es, Time :> es, Datasets :> es, Metadata es, MetadataSync :> es, Tasks :> es) => SyncProposalTask -> Eff es ()
syncProposalTask task = do
  logStatus $ "started " <> show task.syncId
  taskSetStatus task Scan
  scan <- Sync.runScanProposal task.proposalId

  send $ Sync.SetScan task.syncId task.proposalId scan

  -- TODO: did the task *fail*, or are these acceptable errors? Should we stop?
  -- oh, we can have *multiple* errors. erm...
  -- well, let's throw the first one eh?
  -- should we capture errors in worker and save them?
  forM_ scan.errors $ \err -> do
    logStatus "ERROR"
    log Err $ dump "ScanError" err
    throwError $ TaskFail (show err)

  let syncDatasetIds = fmap (\sd -> SyncItem sd.item.datasetId sd.sync) scan.datasets
  taskSetStatus task $ Exec scan.errors syncDatasetIds

  Sync.execSync scan.datasets

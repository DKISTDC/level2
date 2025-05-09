module App.Worker.SyncMetadata where

import Data.Grouped
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Sync as Sync
import NSO.Metadata
import NSO.Prelude
import NSO.Types.InstrumentProgram


-- SYNC METADATA -----------------------------------------------

data SyncMetadataTask = SyncMetadataTask {syncId :: SyncId}
  deriving (Show, Eq)


instance WorkerTask SyncMetadataTask where
  type Status SyncMetadataTask = Bool
  idle = False


needsMetadataSync :: (MetadataSync :> es, Time :> es) => Eff es Bool
needsMetadataSync = do
  syc <- Sync.lastSync
  now <- currentTime
  case syc of
    Nothing -> pure True
    Just s -> pure $ hasDayPassed s now
 where
  hasDayPassed earlier later = diffUTCTime later earlier >= nominalDay

  nominalDay :: NominalDiffTime
  nominalDay = 86400


syncMetadataTask
  :: ( Log :> es
     , MetadataSync :> es
     , Metadata es
     , Tasks SyncProposalTask :> es
     , Tasks SyncMetadataTask :> es
     )
  => SyncMetadataTask
  -> Eff es ()
syncMetadataTask task = do
  log Info "Scan Metadata for Proposals"
  send $ TaskSetStatus task True
  gs <- Sync.runScanAvailable
  let propIds = fmap (\g -> Id (sample g).primaryProposalId) gs
  send $ Sync.SetProposals task.syncId propIds
  let tasks = fmap (\g -> SyncProposalTask task.syncId $ Id (sample g).primaryProposalId) gs
  send $ TasksAdd tasks


-- -- -- maybe I don't need this at all? But it would be nice to catch things...
-- saveErrorAndCrash :: (MetadataSync :> es) => SyncId -> IOError -> Eff es a
-- saveErrorAndCrash s e = do
--   send $ Sync.SetError s (show e)
--   -- doesn't matter because we crash anyway!
--   -- TEST: not sure what happens if we have an error. Test by not being connected
--   throwM e

-- SYNC PROPOSAL -------------------------------------------------------------------------------------------------------------------

data SyncProposalTask = SyncProposalTask
  { syncId :: SyncId
  , proposalId :: Id Proposal
  }
  deriving (Eq, Show)


instance WorkerTask SyncProposalTask where
  type Status SyncProposalTask = SyncProgress
  idle = Wait


data SyncProgress
  = Wait
  | Scan
  | Exec [ScanError] [SyncDataset]


syncProposalTask :: (Log :> es, Time :> es, Datasets :> es, Metadata es, MetadataSync :> es, Tasks SyncProposalTask :> es) => SyncProposalTask -> Eff es ()
syncProposalTask task = do
  log Info $ dump "SyncProposalTask" task.syncId
  send $ TaskSetStatus task Scan
  scan <- Sync.runScanProposal task.proposalId

  send $ Sync.SetScan task.syncId task.proposalId scan

  forM_ scan.errors $ \err ->
    log Err $ dump "ScanError" err

  send $ TaskSetStatus task $ Exec scan.errors scan.datasets

  Sync.execSync scan.datasets
  pure ()

module App.Worker.SyncMetadata where

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


-- SYNC PROPOSAL -------------------------------------------------------------------------------------------------------------------

data SyncProposal = SyncProposal
  { proposalId :: Id Proposal
  }
  deriving (Eq, Show)


instance WorkerTask SyncProposal where
  type Status SyncProposal = SyncProgress
  idle = Wait


data SyncProgress
  = Wait
  | Scan
  | Exec [ScanError] [SyncDataset]


-- eh... by default we delete all tasks from the queue afterwards.... booo

-- a single task that reports its progress? Or a bunch of workers?
syncProposalTask :: (Log :> es, Time :> es, Datasets :> es, Metadata :> es, Tasks SyncProposal :> es) => SyncProposal -> Eff es ()
syncProposalTask task = do
  send $ TaskSetStatus task Scan
  scan <- Sync.runScanProposal task.proposalId

  forM_ scan.errors $ \err ->
    log Err $ dump "ScanError" err

  send $ TaskSetStatus task $ Exec scan.errors scan.datasets

  Sync.execSync scan.datasets
  pure ()

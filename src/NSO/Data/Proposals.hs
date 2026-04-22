module NSO.Data.Proposals where

import Effectful
import NSO.Data.Datasets as Datasets
import NSO.Prelude
import NSO.Types.Proposal


-- Load these from datasets. Otherwise we would need to sync the proposal description to every program?
loadAllProposals :: (Datasets :> es) => Eff es [Proposal]
loadAllProposals = do
  ds <- Datasets.distinct DistinctProposals
  pure $ fmap proposalFromDataset ds


proposalFromDataset :: Dataset -> Proposal
proposalFromDataset d =
  Proposal
    { proposalId = d.primaryProposalId
    , description = d.experimentDescription
    , startTime = d.startTime
    }

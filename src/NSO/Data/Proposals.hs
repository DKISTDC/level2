module NSO.Data.Proposals
  ( Proposal
  , ProposalDetails (..)
  , allProposalIds
  , loadAllProposals
  , lookupProposal
  ) where

import Effectful
import NSO.Data.Datasets as Datasets
import NSO.Data.Programs as Programs
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Proposal


allProposalIds :: (Programs :> es) => Eff es [Id Proposal]
allProposalIds = do
  send Programs.ProposalIds


loadAllProposals :: (Programs :> es, Datasets :> es) => Eff es [ProposalDetails]
loadAllProposals = do
  propIds <- send Programs.ProposalIds
  catMaybes <$> mapM lookupProposal propIds


lookupProposal :: (Datasets :> es) => Id Proposal -> Eff es (Maybe ProposalDetails)
lookupProposal propId = do
  ds <- Datasets.distinct (DistinctProposal propId)
  pure $ case ds of
    [] -> Nothing
    (d : _) -> Just $ ProposalDetails d

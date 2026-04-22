module NSO.Types.Proposal where

import NSO.Prelude
import NSO.Types.Common


data ObservingProgram
data Experiment


data Proposal = Proposal
  { proposalId :: Id Proposal
  , description :: Text
  , startTime :: UTCTime
  }
  deriving (Eq, Show)

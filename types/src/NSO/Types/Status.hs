module NSO.Types.Status where

import Data.Grouped
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram (InstrumentProgram, Proposal)
import NSO.Types.Inversion


data ProgramStatus
  = StatusInvalid
  | StatusQualified
  | StatusError Text
  | -- we have a "latest" inversion, use its status
    StatusInversion Inversion
  deriving (Eq, Show)


data ProposalPrograms = ProposalPrograms
  { proposal :: Proposal
  , programs :: Grouped (Id Proposal) ProgramFamily
  }


data ProgramFamily = ProgramFamily
  { program :: InstrumentProgram
  , status :: ProgramStatus
  , datasets :: Grouped (Id InstrumentProgram) Dataset
  , inversions :: [Inversion]
  }

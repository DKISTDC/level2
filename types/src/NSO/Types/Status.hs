module NSO.Types.Status where

import Data.Grouped
import NSO.Prelude
import NSO.Types.InstrumentProgram (InstrumentProgram, Proposal)
import NSO.Types.Inversion (InversionStep)


data ProgramStatus
  = StatusInvalid
  | StatusQualified
  | StatusError Text
  | -- we have a "latest" inversion, use its status
    StatusInversion InversionStep
  deriving (Eq)


data InstrumentProgramStatus = InstrumentProgramStatus
  { program :: InstrumentProgram
  , status :: ProgramStatus
  }


data ProposalPrograms = ProposalPrograms
  { proposal :: Proposal
  , programs :: Grouped Proposal InstrumentProgramStatus
  }

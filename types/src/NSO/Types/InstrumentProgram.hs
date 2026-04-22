module NSO.Types.InstrumentProgram where

import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Proposal
import NSO.Types.Status
import NSO.Types.Wavelength


data InstrumentProgram = InstrumentProgram
  { programId :: Id InstrumentProgram
  , proposalId :: Id Proposal
  , experimentId :: Id Experiment
  , instrument :: Instrument
  , createDate :: UTCTime
  , startTime :: UTCTime
  , stokesParameters :: StokesParameters
  , spectralLines :: [SpectralLine]
  , embargo :: Maybe UTCTime
  , status :: ProgramStatus
  }
  deriving (Show, Eq)

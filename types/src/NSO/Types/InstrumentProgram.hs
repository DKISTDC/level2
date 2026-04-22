module NSO.Types.InstrumentProgram where

import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Proposal
import NSO.Types.Status
import NSO.Types.Wavelength
import Rel8


type InstrumentProgram = InstrumentProgram' Identity
data InstrumentProgram' f = InstrumentProgram'
  { programId :: Column f (Id InstrumentProgram)
  , proposalId :: Column f (Id Proposal)
  , experimentId :: Column f (Id Experiment)
  , instrument :: Column f Instrument
  , createDate :: Column f UTCTime
  , startTime :: Column f UTCTime
  , stokesParameters :: Column f StokesParameters
  , spectralLines :: Column f [SpectralLine]
  , embargo :: Column f Bool
  , status :: Column f ProgramStatus
  }


deriving stock instance (f ~ Result) => Show (InstrumentProgram' f)
deriving stock instance (f ~ Result) => Eq (InstrumentProgram' f)

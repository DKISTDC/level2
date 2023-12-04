module NSO.Types.InstrumentProgram where

import Data.Grouped
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.Wavelength


-- These do NOT have datasets
data InstrumentProgram = InstrumentProgram
  { programId :: Id InstrumentProgram
  , instrument :: Instrument
  , experimentId :: Id Experiment
  , experimentDescription :: Text
  , createDate :: UTCTime
  , startTime :: UTCTime
  , stokesParameters :: StokesParameters
  , onDisk :: Bool
  , spectralLines :: [SpectralLine]
  , otherWavelengths :: [Wavelength Nm]
  , status :: ProgramStatus
  , embargo :: Maybe UTCTime
  }


data Experiment = Experiment
  { experimentId :: Id Experiment
  , description :: Text
  , startTime :: UTCTime
  , programs :: Grouped Experiment InstrumentProgram
  }


data ProgramStatus
  = Invalid
  | Qualified
  | Queued
  | Inverted
  deriving (Eq)

module NSO.Types.InstrumentProgram where

import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Wavelength


-- These do NOT have datasets
data InstrumentProgram = InstrumentProgram
  { programId :: Id InstrumentProgram
  , instrument :: Instrument
  , proposalId :: Id Proposal
  , experimentDescription :: Text
  , createDate :: UTCTime
  , startTime :: UTCTime
  , stokesParameters :: StokesParameters
  , onDisk :: Bool
  , spectralLines :: [SpectralLine]
  , otherWavelengths :: [Wavelength Nm]
  , embargo :: Maybe UTCTime
  , qualified :: Bool
  }
  deriving (Eq, Show)


data Proposal = Proposal
  { proposalId :: Id Proposal
  , description :: Text
  , startTime :: UTCTime
  }
  deriving (Eq, Show)

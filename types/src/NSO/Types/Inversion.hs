{-# LANGUAGE StrictData #-}

module NSO.Types.Inversion
  ( Inversion (..)
  , InversionRow (..)
  , module NSO.Types.Status
  ) where

import Data.Time.Clock
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Status
import Rel8


data Inversion = Inversion
  { inversionId :: Id Inversion
  , programId :: Id InstrumentProgram
  , step :: InversionStep
  }


-- The database definition is flattened. Needs validation on return from DB!
data InversionRow f = InversionRow
  { inversionId :: Column f (Id Inversion)
  , programId :: Column f (Id InstrumentProgram)
  , created :: Column f UTCTime
  , download :: Column f (Maybe UTCTime)
  , calibration :: Column f (Maybe UTCTime)
  , calibrationUrl :: Column f (Maybe Url)
  , inversion :: Column f (Maybe UTCTime)
  , inversionSoftware :: Column f (Maybe InversionSoftware)
  , postProcess :: Column f (Maybe UTCTime)
  , publish :: Column f (Maybe UTCTime)
  }
  deriving (Generic, Rel8able)

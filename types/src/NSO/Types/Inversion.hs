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
  deriving (Show)


-- The database definition is flattened. Needs validation on return from DB!
data InversionRow f = InversionRow
  { inversionId :: Column f (Id Inversion)
  , programId :: Column f (Id InstrumentProgram)
  , created :: Column f UTCTime
  , download :: Column f (Maybe UTCTime)
  , calibration :: Column f (Maybe UTCTime)
  , calibrationSoftware :: Column f (Maybe GitCommit)
  , inversion :: Column f (Maybe UTCTime)
  , inversionSoftware :: Column f (Maybe GitCommit)
  , postProcess :: Column f (Maybe UTCTime)
  , publish :: Column f (Maybe UTCTime)
  }
  deriving (Generic, Rel8able)


deriving stock instance (f ~ Result) => Show (InversionRow f)
deriving stock instance (f ~ Result) => Eq (InversionRow f)

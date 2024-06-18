{-# LANGUAGE StrictData #-}

module NSO.Types.Inversion
  ( Inversion (..)
  , InversionRow (..)
  , module NSO.Types.Status
  ) where

import Data.Time.Clock
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Status
import Network.Globus (Task)
import Rel8


data Inversion = Inversion
  { inversionId :: Id Inversion
  , programId :: Id InstrumentProgram
  , created :: UTCTime
  , updated :: UTCTime
  , step :: InversionStep
  }
  deriving (Show)


-- The database definition is flattened. Needs validation on return from DB!
data InversionRow f = InversionRow
  { inversionId :: Column f (Id Inversion)
  , programId :: Column f (Id InstrumentProgram)
  , created :: Column f UTCTime
  , updated :: Column f UTCTime
  , download :: Column f (Maybe UTCTime)
  , downloadTaskId :: Column f (Maybe (Id Task))
  , downloadDatasets :: Column f [Id Dataset]
  , preprocess :: Column f (Maybe UTCTime)
  , preprocessSoftware :: Column f (Maybe GitCommit)
  , upload :: Column f (Maybe UTCTime)
  , uploadTaskId :: Column f (Maybe (Id Task))
  , inversion :: Column f (Maybe UTCTime)
  , inversionSoftware :: Column f (Maybe GitCommit)
  , generate :: Column f (Maybe UTCTime)
  , generateTaskId :: Column f (Maybe (Id Task))
  , generateL1FrameDir :: Column f (Maybe Text)
  , publish :: Column f (Maybe UTCTime)
  }
  deriving (Generic, Rel8able)


deriving stock instance (f ~ Result) => Show (InversionRow f)
deriving stock instance (f ~ Result) => Eq (InversionRow f)

{-# LANGUAGE DerivingVia #-}
module NSO.Data.Dataset where

import NSO.Prelude
import Rel8
import Hasql.Statement
import Data.Time.Clock (UTCTime)

newtype Id a = Id Text
  deriving newtype (Show, DBType)

data ObservingProgram


newtype StokesParameters = StokesParameters [Stokes]
  deriving newtype (DBType, Show)

data Stokes = I | Q | U | V
  deriving (Show, Read)
  deriving DBType via ReadShow Stokes

data Dataset f = Dataset
  { datasetId :: Column f (Id Dataset)
  , programId :: Column f (Id ObservingProgram)
  , stokesParameters :: Column f StokesParameters
  , createDate :: Column f UTCTime
  , wavelengthMin :: Column f Double
  , wavelengthMax :: Column f Double
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance f ~ Result => Show (Dataset f)

datasets :: TableSchema (Dataset Name)
datasets = TableSchema
  { name = "datasets"
  , schema = Nothing
  , columns = Dataset
      { datasetId = "dataset_id"
      , programId = "observing_program_id"
      , stokesParameters = "stokes_parameters"
      , createDate = "create_date"
      , wavelengthMin = "wavelength_min"
      , wavelengthMax = "wavelength_max"
      }
  }

allDatasets :: Statement () [Dataset Result]
allDatasets = select $ each datasets

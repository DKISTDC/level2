module NSO.Data.Dataset where


import NSO.Prelude
import Data.Int (Int64)
import Rel8
import Hasql.Statement
import Data.Time.Clock (UTCTime)

newtype Id a = Id Int64
  deriving newtype (Show, DBType)

data ObservingProgram

data Dataset f = Dataset
  { datasetId :: Column f (Id Dataset)
  , programId :: Column f (Id ObservingProgram)
  , stokesParameters :: Column f [Text]
  , createDate :: Column f UTCTime
  , wavelengthMin :: Column f Float
  , wavelengthMax :: Column f Float
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

{-# LANGUAGE DerivingVia #-}

module NSO.Data.Dataset where

import Data.Int (Int16, Int64)
import Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Hasql.Statement
import NSO.Prelude
import Rel8

newtype Id a = Id {fromId :: Text}
  deriving newtype (Show, Eq, Ord, DBType)

newtype StokesParameters = StokesParameters [Stokes]
  deriving newtype (DBType)

instance Show StokesParameters where
  show (StokesParameters ss) = mconcat $ fmap show ss

data ObservingProgram = ObservingProgram
  { programId :: Id ObservingProgram
  , datasets :: NonEmpty Dataset
  }

data ObserveFrames

data Stokes = I | Q | U | V
  deriving (Show, Read)
  deriving (DBType) via ReadShow Stokes

type Dataset = Dataset' Identity
data Dataset' f = Dataset
  { datasetId :: Column f (Id Dataset)
  , scanDate :: Column f UTCTime
  , programId :: Column f (Id ObservingProgram)
  , stokesParameters :: Column f StokesParameters
  , createDate :: Column f UTCTime
  , wavelengthMin :: Column f Double
  , wavelengthMax :: Column f Double
  , startTime :: Column f UTCTime
  , endTime :: Column f UTCTime
  , frameCount :: Column f Int16
  , primaryExperimentId :: Column f Text
  , primaryProposalId :: Column f Text
  , experimentDescription :: Column f Text
  , inputDatasetObserveFramesPartId :: Column f (Id ObserveFrames)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance (f ~ Result) => Show (Dataset' f)

datasets :: TableSchema (Dataset' Name)
datasets =
  TableSchema
    { name = "datasets"
    , schema = Nothing
    , columns =
        Dataset
          { datasetId = "dataset_id"
          , programId = "observing_program_id"
          , scanDate = "scan_date"
          , stokesParameters = "stokes_parameters"
          , createDate = "create_date"
          , wavelengthMin = "wavelength_min"
          , wavelengthMax = "wavelength_max"
          , startTime = "start_time"
          , endTime = "end_time"
          , frameCount = "frame_count"
          , primaryExperimentId = "primary_experiment_id"
          , primaryProposalId = "primary_proposal_id"
          , inputDatasetObserveFramesPartId = "input_observe_frames_id"
          , experimentDescription = "experiment_description"
          }
    }

allDatasets :: Statement () [Dataset]
allDatasets = select $ each datasets

insertAll :: [Dataset] -> Statement () Int64
insertAll ds =
  Rel8.insert
    $ Insert
      { into = datasets
      , rows = values $ fmap lit ds
      , onConflict = DoNothing
      , returning = NumberOfRowsAffected
      }

toObservingPrograms :: [Dataset] -> [ObservingProgram]
toObservingPrograms = fmap toProgram . groupWith (.programId) . sortOn (.programId)
 where
  toProgram (d :| ds) = ObservingProgram d.programId (d :| ds)

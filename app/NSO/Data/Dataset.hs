{-# LANGUAGE DerivingVia #-}

module NSO.Data.Dataset where

import Data.Int (Int16, Int64)
import Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Hasql.Statement
import NSO.Metadata.Types
import NSO.Prelude
import Rel8

type Dataset = Dataset' Identity
data Dataset' f = Dataset
  { datasetId :: Column f (Id Dataset)
  , scanDate :: Column f UTCTime
  , observingProgramExecutionId :: Column f (Id ObservingProgramExecution)
  , instrumentProgramExecutionId :: Column f Text
  , instrumentName :: Column f Text
  , stokesParameters :: Column f StokesParameters
  , createDate :: Column f UTCTime
  , wavelengthMin :: Column f Double
  , wavelengthMax :: Column f Double
  , startTime :: Column f UTCTime
  , endTime :: Column f UTCTime
  , frameCount :: Column f Int16
  , primaryExperimentId :: Column f (Id Experiment)
  , primaryProposalId :: Column f (Id Proposal)
  , experimentDescription :: Column f Text
  , exposureTime :: Column f Float
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
          , observingProgramExecutionId = "observing_program_execution_id"
          , instrumentProgramExecutionId = "instrument_program_execution_id"
          , instrumentName = "instrument_name"
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
          , exposureTime = "exposure_time"
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

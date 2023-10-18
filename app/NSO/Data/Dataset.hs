{-# LANGUAGE DerivingVia #-}

module NSO.Data.Dataset where

import Data.Int (Int16, Int64)
import Data.List.NonEmpty as NE
import Data.Set as S
import Data.Time.Clock (UTCTime)
import Effectful
import Effectful.Rel8
import Hasql.Statement
import NSO.Data.Types
import NSO.Prelude
import Rel8

data Proposal
data Experiment = Experiment
  { experimentId :: Id Experiment
  , observingProgramExecutions :: NonEmpty ObservingProgram
  }

data ObservingProgram = ObservingProgram
  { observingProgramExecutionId :: Id ObservingProgram
  , datasets :: NonEmpty Dataset
  }

data InstrumentProgram

data Instrument
  = VBI
  | VISP
  deriving (Show, Ord, Eq, Read)
  deriving (DBType) via ReadShow Instrument

type Dataset = Dataset' Identity
data Dataset' f = Dataset
  { datasetId :: Column f (Id Dataset)
  , scanDate :: Column f UTCTime
  , observingProgramExecutionId :: Column f (Id ObservingProgram)
  , instrument :: Column f Instrument
  , instrumentProgramExecutionId :: Column f (Id InstrumentProgram)
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
          , instrument = "instrument"
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

queryAll :: (Rel8 :> es) => Eff es [Dataset]
queryAll = query () $ select $ each datasets

queryExperiment :: (Rel8 :> es) => Id Experiment -> Eff es [Dataset]
queryExperiment eid = query () $ select $ do
  row <- each datasets
  where_ (row.primaryExperimentId ==. lit eid)
  return row

insertAll :: [Dataset] -> Statement () Int64
insertAll ds =
  Rel8.insert
    $ Insert
      { into = datasets
      , rows = values $ fmap lit ds
      , onConflict = DoNothing
      , returning = NumberOfRowsAffected
      }

toObservingPrograms :: NonEmpty Dataset -> NonEmpty ObservingProgram
toObservingPrograms = fmap toProgram . groupSort (.observingProgramExecutionId)
 where
  toProgram :: NonEmpty Dataset -> ObservingProgram
  toProgram ds = ObservingProgram (head ds).observingProgramExecutionId ds

toExperiments :: NonEmpty Dataset -> NonEmpty Experiment
toExperiments = fmap toExperiment . groupSort (.primaryExperimentId)
 where
  toExperiment :: NonEmpty Dataset -> Experiment
  toExperiment ds = Experiment (head ds).primaryExperimentId (toObservingPrograms ds)

groupSort :: (Eq b, Ord b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupSort f = groupWith1 f . sortWith f

stokes :: NonEmpty Dataset -> StokesParameters
stokes =
  mconcat . NE.toList . fmap (.stokesParameters)

instruments :: NonEmpty Dataset -> Set Instrument
instruments = S.fromList . NE.toList . fmap (.instrument)

maxCreateDate :: NonEmpty Dataset -> UTCTime
maxCreateDate = maximum . fmap (.createDate)

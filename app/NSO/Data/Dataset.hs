{-# LANGUAGE DerivingVia #-}

module NSO.Data.Dataset where

import Data.Aeson (FromJSON, ToJSON)
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
import Web.Hyperbole

data Proposal
data Experiment = Experiment
  { experimentId :: Id Experiment
  , startTime :: UTCTime
  , observingPrograms :: NonEmpty ObservingProgram
  , instrumentPrograms :: NonEmpty InstrumentProgram
  }

data ObservingProgram = ObservingProgram
  { observingProgramId :: Id ObservingProgram
  , datasets :: NonEmpty Dataset
  }

data InstrumentProgram = InstrumentProgram
  { instrumentProgramId :: Id InstrumentProgram
  , instrument :: Instrument
  , createDate :: UTCTime
  , startTime :: UTCTime
  , stokesParameters :: StokesParameters
  , datasets :: NonEmpty Dataset
  }

data Instrument
  = VBI
  | VISP
  deriving (Show, Ord, Eq, Read, Param)
  deriving (DBType) via ReadShow Instrument

type Dataset = Dataset' Identity
data Dataset' f = Dataset
  { datasetId :: Column f (Id Dataset)
  , scanDate :: Column f UTCTime
  , latest :: Column f Bool
  , observingProgramExecutionId :: Column f (Id ObservingProgram)
  , instrument :: Column f Instrument
  , instrumentProgramExecutionId :: Column f (Id InstrumentProgram)
  , stokesParameters :: Column f StokesParameters
  , createDate :: Column f UTCTime
  , wavelengthMin :: Column f (Wavelength Nm)
  , wavelengthMax :: Column f (Wavelength Nm)
  , startTime :: Column f UTCTime
  , endTime :: Column f UTCTime
  , frameCount :: Column f Int16
  , primaryExperimentId :: Column f (Id Experiment)
  , primaryProposalId :: Column f (Id Proposal)
  , experimentDescription :: Column f Text
  , exposureTime :: Column f Float
  , inputDatasetObserveFramesPartId :: Column f (Id ObserveFrames)
  , boundingBox :: Column f (Maybe BoundingBox)
  --   health :: Column f (JSONEncoded Health)
  -- , gosStatus :: Column f (JSONEncoded GOSStatus)
  -- , aoLocked :: Column f Int16
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Dataset' f)
deriving stock instance (f ~ Result) => Eq (Dataset' f)

data Health = Health
  { good :: Int
  , bad :: Int
  , ill :: Int
  , unknown :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data GOSStatus = GOSStatus
  { open :: Int
  , opening :: Int
  , closed :: Int
  , closing :: Int
  , undefined :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

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
          , boundingBox = "bounding_box"
          , latest = "latest"
          -- , health = "health"
          -- , gosStatus = "gos_status"
          -- , aoLocked = "ao_locked"
          }
    }

queryLatest :: (Rel8 :> es) => Eff es [Dataset]
queryLatest = query () $ select $ do
  row <- each datasets
  where_ (row.latest ==. lit True)
  return row

queryExperiment :: (Rel8 :> es) => Id Experiment -> Eff es [Dataset]
queryExperiment eid = query () $ select $ do
  row <- each datasets
  where_ (row.primaryExperimentId ==. lit eid)
  return row

queryProgram :: (Rel8 :> es) => Id InstrumentProgram -> Eff es [Dataset]
queryProgram ip = query () $ select $ do
  row <- each datasets
  where_ (row.instrumentProgramExecutionId ==. lit ip)
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

updateOld :: [Id Dataset] -> Statement () Int64
updateOld ids =
  let ids' = fmap lit ids
   in Rel8.update
        $ Update
          { target = datasets
          , set = \_ row -> row{latest = lit False}
          , updateWhere = \_ row -> row.datasetId `in_` ids'
          , from = pure ()
          , returning = NumberOfRowsAffected
          }

toObservingPrograms :: NonEmpty Dataset -> NonEmpty ObservingProgram
toObservingPrograms = fmap toProgram . groupSort (.observingProgramExecutionId)
 where
  toProgram :: NonEmpty Dataset -> ObservingProgram
  toProgram ds = ObservingProgram (head ds).observingProgramExecutionId ds

toInstrumentPrograms :: NonEmpty Dataset -> NonEmpty InstrumentProgram
toInstrumentPrograms = fmap instrumentProgram . groupSort (.instrumentProgramExecutionId)

instrumentProgram :: NonEmpty Dataset -> InstrumentProgram
instrumentProgram ds =
  let d = head ds
   in InstrumentProgram
        { instrumentProgramId = d.instrumentProgramExecutionId
        , createDate = d.createDate
        , stokesParameters = d.stokesParameters
        , startTime = d.startTime
        , instrument = d.instrument
        , datasets = ds
        }

toExperiments :: NonEmpty Dataset -> NonEmpty Experiment
toExperiments = fmap toExperiment . groupSort (.primaryExperimentId)
 where
  toExperiment :: NonEmpty Dataset -> Experiment
  toExperiment ds =
    let d = head ds
     in Experiment
          { experimentId = d.primaryExperimentId
          , startTime = d.startTime
          , observingPrograms = toObservingPrograms ds
          , instrumentPrograms = toInstrumentPrograms ds
          }

groupSort :: (Eq b, Ord b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupSort f = groupWith1 f . sortWith f

stokes :: NonEmpty Dataset -> StokesParameters
stokes =
  mconcat . NE.toList . fmap (.stokesParameters)

instruments :: NonEmpty Dataset -> Set Instrument
instruments = S.fromList . NE.toList . fmap (.instrument)

maxCreateDate :: NonEmpty Dataset -> UTCTime
maxCreateDate = maximum . fmap (.createDate)

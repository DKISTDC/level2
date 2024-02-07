{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module NSO.Data.Datasets
  ( Dataset
  , Dataset' (..)
  , module NSO.Types.Dataset
  , Id (..)
  , Wavelength (..)
  , Nm
  , queryLatest
  , queryExperiment
  , queryProgram
  , queryById
  , insertAll
  , updateOld
  )
where

import Control.Monad (void)
import Data.Int (Int16)
import Effectful
import Effectful.Rel8
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength
import Rel8


data Dataset' f = Dataset'
  { datasetId :: Column f (Id Dataset)
  , scanDate :: Column f UTCTime
  , latest :: Column f Bool
  , observingProgramId :: Column f (Id ObservingProgram)
  , instrument :: Column f Instrument
  , instrumentProgramId :: Column f (Id InstrumentProgram)
  , stokesParameters :: Column f StokesParameters
  , createDate :: Column f UTCTime
  , updateDate :: Column f UTCTime
  , wavelengthMin :: Column f (Wavelength Nm)
  , wavelengthMax :: Column f (Wavelength Nm)
  , startTime :: Column f UTCTime
  , endTime :: Column f UTCTime
  , frameCount :: Column f Int16
  , primaryExperimentId :: Column f (Id Experiment)
  , primaryProposalId :: Column f (Id Proposal)
  , experimentDescription :: Column f Text
  , exposureTime :: Column f Float
  , boundingBox :: Column f (Maybe BoundingBox)
  , health :: Column f Health
  , gosStatus :: Column f GOSStatus
  , aoLocked :: Column f Int16
  , lightLevel :: Column f Distribution
  , polarimetricAccuracy :: Column f Distribution
  , friedParameter :: Column f Distribution
  , embargo :: Column f (Maybe UTCTime)
  }
  deriving (Generic, Rel8able)


-- deriving stock instance (f ~ Result) => Show (Dataset' f)
-- deriving stock instance (f ~ Result) => Eq (Dataset' f)

datasets :: TableSchema (Dataset' Name)
datasets =
  TableSchema
    { name = "datasets"
    , schema = Nothing
    , columns =
        Dataset'
          { datasetId = "dataset_id"
          , observingProgramId = "observing_program_id"
          , instrumentProgramId = "instrument_program_id"
          , instrument = "instrument"
          , scanDate = "scan_date"
          , stokesParameters = "stokes_parameters"
          , createDate = "create_date"
          , updateDate = "update_date"
          , wavelengthMin = "wavelength_min"
          , wavelengthMax = "wavelength_max"
          , startTime = "start_time"
          , endTime = "end_time"
          , frameCount = "frame_count"
          , primaryExperimentId = "primary_experiment_id"
          , primaryProposalId = "primary_proposal_id"
          , experimentDescription = "experiment_description"
          , exposureTime = "exposure_time"
          , boundingBox = "bounding_box"
          , latest = "latest"
          , health = "health"
          , gosStatus = "gos_status"
          , aoLocked = "ao_locked"
          , friedParameter = "fried_parameter"
          , polarimetricAccuracy = "polarimetric_accuracy"
          , lightLevel = "light_level"
          , embargo = "embargo"
          }
    }


toDataset :: Dataset' Result -> Dataset
toDataset Dataset'{..} =
  Dataset
    { datasetId
    }


fromDataset :: Dataset -> Dataset' Identity
fromDataset Dataset{..} =
  Dataset'
    { datasetId
    }


queryLatest :: (Rel8 :> es) => Eff es [Dataset]
queryLatest = do
  ds <- query () $ select $ do
    row <- each datasets
    where_ (row.latest ==. lit True)
    return row
  pure $ fmap toDataset ds


queryExperiment :: (Rel8 :> es) => Id Experiment -> Eff es [Dataset]
queryExperiment eid = do
  ds <- query () $ select $ do
    row <- each datasets
    where_ (row.primaryExperimentId ==. lit eid)
    return row
  pure $ fmap toDataset ds


-- pure $ fmap dataset ds

queryProgram :: (Rel8 :> es) => Id InstrumentProgram -> Eff es [Dataset]
queryProgram ip = do
  ds <- query () $ select $ do
    -- note that this DOESN'T limit by latest
    row <- each datasets
    where_ (row.instrumentProgramId ==. lit ip)
    return row
  pure $ fmap toDataset ds


queryById :: (Rel8 :> es) => Id Dataset -> Eff es [Dataset]
queryById i = do
  ds <- query () $ select $ do
    row <- each datasets
    where_ (row.datasetId ==. lit i)
    return row
  pure $ fmap toDataset ds


insertAll :: (Rel8 :> es) => [Dataset] -> Eff es ()
insertAll ds =
  void $
    query () $
      Rel8.insert $
        Insert
          { into = datasets
          , rows = values $ fmap (lit . fromDataset) ds
          , onConflict = DoNothing
          , returning = NumberOfRowsAffected
          }


updateOld :: (Rel8 :> es) => [Id Dataset] -> Eff es ()
updateOld ids = do
  let ids' = fmap lit ids
  void $
    query () $
      Rel8.update $
        Update
          { target = datasets
          , set = \_ row -> setOld row
          , updateWhere = \_ row -> row.datasetId `in_` ids'
          , from = pure ()
          , returning = NumberOfRowsAffected
          }
 where
  setOld :: Dataset' Expr -> Dataset' Expr
  setOld row = row{latest = lit False}

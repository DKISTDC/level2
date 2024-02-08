{-# LANGUAGE DerivingVia #-}

module NSO.Data.Datasets
  ( Datasets (..)
  , Filter (..)
  , Modify (..)
  , module NSO.Types.Dataset
  , module NSO.Types.Common
  , module NSO.Types.Wavelength
  , runDataDatasets
  )
where

import Control.Monad (void)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Rel8
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength
import Rel8


-- Put all the operations here?
data Datasets :: Effect where
  Query :: Filter -> Datasets m [Dataset]
  Create :: [Dataset] -> Datasets m ()
  Modify :: Modify -> [Id Dataset] -> Datasets m ()


type instance DispatchOf Datasets = 'Dynamic


data Filter
  = Latest
  | ByExperiment (Id Experiment)
  | ByProgram (Id InstrumentProgram)
  | ById (Id Dataset)


data Modify
  = SetOld


runDataDatasets
  :: (IOE :> es, Rel8 :> es)
  => Eff (Datasets : es) a
  -> Eff es a
runDataDatasets = interpret $ \_ -> \case
  Query Latest -> queryLatest
  Query (ByExperiment eid) -> queryExperiment eid
  Query (ByProgram pid) -> queryProgram pid
  Query (ById did) -> queryById did
  Create ds -> insertAll ds
  Modify SetOld ids -> updateOld ids
 where
  queryLatest :: (Rel8 :> es) => Eff es [Dataset]
  queryLatest = do
    query () $ select $ do
      row <- each datasets
      where_ (row.latest ==. lit True)
      return row

  queryExperiment :: (Rel8 :> es) => Id Experiment -> Eff es [Dataset]
  queryExperiment eid = do
    query () $ select $ do
      row <- each datasets
      where_ (row.primaryExperimentId ==. lit eid)
      return row

  queryProgram :: (Rel8 :> es) => Id InstrumentProgram -> Eff es [Dataset]
  queryProgram ip = do
    query () $ select $ do
      -- note that this DOESN'T limit by latest
      row <- each datasets
      where_ (row.instrumentProgramId ==. lit ip)
      return row

  queryById :: (Rel8 :> es) => Id Dataset -> Eff es [Dataset]
  queryById i = do
    query () $ select $ do
      row <- each datasets
      where_ (row.datasetId ==. lit i)
      return row

  insertAll :: (Rel8 :> es) => [Dataset] -> Eff es ()
  insertAll ds =
    void $
      query () $
        Rel8.insert $
          Insert
            { into = datasets
            , rows = values $ fmap lit ds
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

  -- toDataset :: Dataset' Result -> Dataset
  -- toDataset d@Dataset'{..} =
  --   let frameCount' = fromIntegral d.frameCount
  --       aoLocked' = fromIntegral d.aoLocked
  --    in Dataset
  --         { frameCount = frameCount'
  --         , aoLocked = aoLocked'
  --         , ..
  --         }
  --
  -- fromDataset :: Dataset -> Dataset' Identity
  -- fromDataset d@Dataset{..} =
  --   let frameCount' = fromIntegral d.frameCount
  --       aoLocked' = fromIntegral d.aoLocked
  --    in Dataset'
  --         { frameCount = frameCount'
  --         , aoLocked = aoLocked'
  --         , ..
  --         }

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

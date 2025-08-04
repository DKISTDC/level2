{-# LANGUAGE DerivingVia #-}

module NSO.Data.Datasets
  ( Datasets (..)
  , Filter (..)
  , find
  , module NSO.Types.Dataset
  , runDataDatasets
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Rel8 as Rel8
import NSO.Prelude hiding (find)
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram


-- Put all the operations here?
data Datasets :: Effect where
  Find :: Filter -> Datasets m [Dataset]
  Create :: [Dataset] -> Datasets m ()
  Save :: Dataset -> Datasets m ()
  Ids :: Datasets m [Id Dataset]


type instance DispatchOf Datasets = 'Dynamic


data Filter
  = All
  | ByProposal (Id Proposal)
  | ByProgram (Id InstrumentProgram)
  | ByIds [Id Dataset]
  | DistinctProposals
  | DistinctPrograms (Id Proposal)


runDataDatasets
  :: (IOE :> es, Rel8 :> es)
  => Eff (Datasets : es) a
  -> Eff es a
runDataDatasets = interpret $ \_ -> \case
  Ids -> do
    run $ select $ fmap (.datasetId) $ distinctOn (.datasetId) $ do
      each datasets :: Query (Dataset' Expr)
  Find All -> do
    run $ select $ each datasets
  Find (ByProposal pid) -> queryProposal pid
  Find (ByProgram pid) -> queryProgram pid
  Find (ByIds dids) -> do
    run $ select $ do
      row <- each datasets
      where_ (row.datasetId `in_` fmap lit dids)
      return row
  Find (DistinctPrograms pid) -> do
    run $ select $ distinctOn (.instrumentProgramId) $ do
      row <- each datasets :: Query (Dataset' Expr)
      where_ (row.primaryProposalId ==. lit pid)
      return row
  Find DistinctProposals -> do
    run $ select $ do
      let q = each datasets :: Query (Dataset' Expr)
      distinctOn (.primaryProposalId) q
  Create ds -> insertAll ds
  Save ds -> updateDataset ds
 where
  queryProposal :: (Rel8 :> es) => Id Proposal -> Eff es [Dataset]
  queryProposal eid = do
    run $ select $ do
      row <- each datasets
      where_ (row.primaryProposalId ==. lit eid)
      return row

  queryProgram :: (Rel8 :> es) => Id InstrumentProgram -> Eff es [Dataset]
  queryProgram ip = do
    run $ select $ do
      row <- each datasets
      where_ (row.instrumentProgramId ==. lit ip)
      return row

  insertAll :: (Rel8 :> es) => [Dataset] -> Eff es ()
  insertAll ds =
    run_ $
      insert $
        Insert
          { into = datasets
          , rows = values $ fmap lit ds
          , onConflict = DoNothing
          , returning = NoReturning
          }

  updateDataset :: (Rel8 :> es) => Dataset -> Eff es ()
  updateDataset ds = do
    run_ $
      update $
        Update
          { target = datasets
          , set = \_ _row -> lit ds
          , updateWhere = \_ row -> row.datasetId ==. lit ds.datasetId
          , from = pure ()
          , returning = NoReturning
          }

  datasets :: TableSchema (Dataset' Name)
  datasets =
    TableSchema
      { name = "datasets"
      , columns =
          Dataset'
            { datasetId = "dataset_id"
            , observingProgramId = "observing_program_id"
            , bucket = "bucket"
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
            , health = "health"
            , gosStatus = "gos_status"
            , aoLocked = "ao_locked"
            , friedParameter = "fried_parameter"
            , polarimetricAccuracy = "polarimetric_accuracy"
            , lightLevel = "light_level"
            , embargo = "embargo"
            , spectralLines = "spectral_lines"
            }
      }


find :: (Datasets :> es) => Filter -> Eff es [Dataset]
find = send . Find

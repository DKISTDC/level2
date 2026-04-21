{-# LANGUAGE DerivingVia #-}

module NSO.Data.Datasets
  ( Datasets (..)
  , Filter (..)
  , DistinctBy (..)
  , findLatest
  , findAll
  , findIds
  , distinct
  , module NSO.Types.Dataset
  , runDataDatasets
  )
where

import Data.Functor.Contravariant ((>$<))
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Rel8 as Rel8
import NSO.Prelude hiding (find)
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import Rel8 (Order, asc, distinctOnBy)


-- Put all the operations here?
data Datasets :: Effect where
  Find :: Filter -> Reprocessing -> Datasets m [Dataset]
  Distinct :: DistinctBy -> Datasets m [Dataset]
  Create :: [Dataset] -> Datasets m ()
  Save :: Dataset -> Datasets m ()
  Ids :: Datasets m [Id Dataset]


type instance DispatchOf Datasets = 'Dynamic


data Reprocessing
  = Latest
  | Complete


data DistinctBy
  = DistinctPrograms (Id Proposal)
  | DistinctProposal (Id Proposal)
  | DistinctProposals


data Filter
  = All
  | ByProposal (Id Proposal)
  | ByProgram (Id InstrumentProgram)
  | ByIds [Id Dataset]


runDataDatasets
  :: (IOE :> es, Rel8 :> es)
  => Eff (Datasets : es) a
  -> Eff es a
runDataDatasets = interpret $ \_ -> \case
  Ids -> do
    run $ select $ fmap (.datasetId) do
      each datasets
  Find All _ -> do
    run $ select $ each datasets
  Find (ByProposal pid) r -> do
    run $ select $ distinctProducts do
      row <- each datasets
      where_ (row.primaryProposalId ==. lit pid)
      return row
  Find (ByProgram pid) r -> do
    run $ select $ distinctProducts do
      row <- each datasets
      where_ (row.instrumentProgramId ==. lit pid)
      return row
  Find (ByIds dids) r -> do
    run $ select $ do
      row <- each datasets
      where_ (row.datasetId `in_` fmap lit dids)
      return row
  Distinct (DistinctPrograms pid) -> do
    run $ select $ distinctOn (.instrumentProgramId) do
      row <- each datasets
      where_ (row.primaryProposalId ==. lit pid)
      return row
  Distinct DistinctProposals -> do
    run $ select $ distinctOn (.primaryProposalId) do
      each datasets
  Distinct (DistinctProposal pid) -> do
    run $ select $ distinctOn (.primaryProposalId) do
      row <- each datasets
      where_ (row.primaryProposalId ==. lit pid)
      pure row
  Create ds -> insertAll ds
  Save ds -> updateDataset ds
 where
  distinctProducts :: Query (Dataset' Expr) -> Query (Dataset' Expr)
  distinctProducts = do
    distinctOnBy (.productId) orderByCreateDate

  orderByCreateDate :: Order (Dataset' Expr)
  orderByCreateDate = (.createDate) >$< asc

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
            , productId = "product_id"
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
            , browseMovieUrl = "browse_movie_url"
            }
      }


findLatest :: (Datasets :> es) => Filter -> Eff es [Dataset]
findLatest f = send $ Find f Latest


findAll :: (Datasets :> es) => Filter -> Eff es [Dataset]
findAll f = send $ Find f Complete


findIds :: (Datasets :> es) => [Id Dataset] -> Eff es [Dataset]
findIds ds = send $ Find (ByIds ds) Complete


distinct :: (Datasets :> es) => DistinctBy -> Eff es [Dataset]
distinct d = send $ Distinct d

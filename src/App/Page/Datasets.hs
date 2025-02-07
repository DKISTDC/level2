{-# LANGUAGE UndecidableInstances #-}

module App.Page.Datasets where

import App.Colors
import App.Effect.Auth
import App.Route
import App.Route qualified as Route
import App.Style qualified as Style
import App.View.Common
import App.View.DataRow (dataRows)
import App.View.Icons as Icons
import App.View.Layout
import Data.Aeson qualified as A
import Effectful.Error.Static
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Scan
import NSO.Error
import NSO.Metadata (Metadata)
import NSO.Prelude
import Numeric (showFFloat)
import Web.Hyperbole
import Web.View.Style (addClass, cls, prop)


-- import NSO.Data.Dataset
-- import NSO.Data.Types

page :: (Hyperbole :> es, Time :> es, Datasets :> es, Metadata :> es, Error DataError :> es, Auth :> es) => Eff es (Page '[AllDatasets, DatasetRow])
page = do
  ds <- Datasets.find Datasets.All
  appLayout (Datasets DatasetRoot) $ do
    hyper AllDatasets $ viewPage $ viewDatasetSummary ds


data AllDatasets = AllDatasets
  deriving (Show, Read, ViewId)


instance (Time :> es, Datasets :> es, Metadata :> es, Error DataError :> es) => HyperView AllDatasets es where
  data Action AllDatasets
    = RunScan
    | Existing
    deriving (Show, Read, ViewAction)


  type Require AllDatasets = '[DatasetRow]


  update RunScan = do
    sync <- syncDatasets
    pure $ do
      viewPage $ viewScanResults sync
  update Existing = do
    ds <- Datasets.find Datasets.All
    pure $ do
      viewPage $ viewExistingDatasets ds


viewDeleted :: [Id Dataset] -> View AllDatasets ()
viewDeleted ds = do
  col Style.page $ do
    col (gap 5) $ do
      el bold "Deleted Old Datasets:"
      forM_ ds $ \d -> do
        el_ (text d.fromId)


viewPage :: View AllDatasets () -> View AllDatasets ()
viewPage content = do
  loading
  col (Style.page . onRequest hide) $ do
    col (gap 10) $ do
      button RunScan (pad 10 . bold . fontSize 24 . Style.btn Primary) "Run Scan"
      button Existing (pad 10 . bold . fontSize 24 . Style.btnOutline Primary) "Current Datasets"
    content
 where
  loading = el (hide . pad 100 . grow . onRequest flexRow) $ do
    space
    el (width 200 . color (light Primary)) Icons.spinner
    space


viewDatasetSummary :: [Dataset] -> View AllDatasets ()
viewDatasetSummary ds = do
  row (gap 5 . section) $ do
    el_ "Datasets: "
    el_ $ text $ cs $ show $ length ds


-- viewAllDatasets :: Either [Dataset] SyncResults -> View AllDatasets ()
-- viewAllDatasets res = do
--   case res of
--     Left ds -> viewExistingDatasets ds
--     Right sync -> viewScanResults sync

viewExistingDatasets :: [Dataset] -> View AllDatasets ()
viewExistingDatasets ds = do
  col section $ do
    el (bold . fontSize 24) "Datasets"
    datasetsTable ds


viewScanResults :: SyncResults -> View AllDatasets ()
viewScanResults sr = do
  col section $ do
    el (bold . fontSize 24) "Errors"
    errorsTable sr.errors

  col section $ do
    el (bold . fontSize 24) "Updated"
    datasetsTable sr.updated

  col section $ do
    el (bold . fontSize 24) "New"
    datasetsTable sr.new

  col section $ do
    el (bold . fontSize 24) "Unchanged"
    datasetsTable sr.unchanged


errorsTable :: [ScanError] -> View AllDatasets ()
errorsTable errs = do
  col (gap 10) $ do
    mapM_ errorRow errs
 where
  errorRow (ScanError err val) = do
    el (color Danger) $ text (cs err)
    code (bg (light Light) . fontSize 12 . wrap . pad 4) $ cs $ A.encode val
  wrap =
    addClass $
      cls "wrap"
        & prop @Text "word-wrap" "break-word"
        & prop @Text "white-space" "normal"


-----------------------------------------------------
-- Datasets (Debug)
-----------------------------------------------------

datasetsTable :: [Dataset] -> View AllDatasets ()
datasetsTable [] = none
datasetsTable ds = do
  let sorted = sortOn (\d -> (d.primaryProposalId, d.instrumentProgramId, d.datasetId)) ds :: [Dataset]
  col (gap 0) $ do
    dataRows sorted $ \d -> do
      hyper (DatasetRow d.datasetId) $ datasetRow d


-- table View.table sorted $ do
--   --    -- tcol (hd "Input Id") $ \d -> cell . cs . show $ d.inputDatasetObserveFramesPartId
--   tcol (hd "Id") $ \d -> View.cell $

--    tcol (hd "Instrument") $ \d -> cell . cs . show $ d.instrument
--    tcol (hd "Stokes") $ \d -> cell . cs . show $ d.stokesParameters
--    tcol (hd "Create Date") $ \d -> cell . showTimestamp $ d.createDate
--    tcol (hd "Wave Min") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMin ""
--    tcol (hd "Wave Max") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMax ""
--    -- tcol (hd "Start Time") $ \d -> cell . showTimestamp $ d.startTime
--    -- tcol (hd "Exposure Time") $ \d -> cell . cs . show $ d.exposureTime
--    -- tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
--    tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
-- tcol cell (hd "End Time") $ \d -> cell . cs . show $ d.endTime
-- tcol cell (hd "peid") $ \d -> cell . cs $ d.primaryExperimentId
-- tcol cell (hd "ppid") $ \d -> cell . cs $ d.primaryProposalId
-- tcol cell (hd "ExperimentDescription") $ \d -> cell . cs . show $ d.experimentDescription

-- tcol (hd "Bounding Box") $ \d -> cell . cs . show $ d.boundingBox
-- tcol (hd "On Disk") $ \d -> cell . cs $ maybe "" (show . isOnDisk (dayOfYear d.startTime)) d.boundingBox

section :: Mod AllDatasets
section = Style.card . gap 15 . pad 15


-----------------------------------------------------
-- Dataset Row
-----------------------------------------------------

data DatasetRow = DatasetRow (Id Dataset)
  deriving (Show, Read, ViewId)


instance (Datasets :> es) => HyperView DatasetRow es where
  data Action DatasetRow = Details
    deriving (Show, Read, ViewAction)


  update Details = do
    DatasetRow did <- viewId
    ds <- Datasets.find (ById did)
    pure $ mapM_ datasetRowDetails ds


datasetRow :: Dataset -> View DatasetRow ()
datasetRow d = do
  row (onLoad Details 100) $ do
    route (Route.Datasets $ Route.Dataset d.datasetId) (Style.link . width 100) $ text $ cs d.datasetId.fromId


datasetRowDetails :: Dataset -> View DatasetRow ()
datasetRowDetails d = do
  row id $ do
    route (Route.Datasets $ Route.Dataset d.datasetId) (Style.link . width 100) $ text $ cs d.datasetId.fromId
    route (Route.Proposal d.primaryProposalId Route.PropRoot) (Style.link . width 100) $ text d.primaryProposalId.fromId
    route (Route.Proposal d.primaryProposalId $ Route.Program d.instrumentProgramId Prog) (Style.link . width 180) $ text d.instrumentProgramId.fromId
    el cell $ text $ cs $ show d.instrument
    el (width 180) $ text $ showTimestamp d.createDate
    el cell $ text $ cs $ showFFloat (Just 1) d.wavelengthMin ""
    el cell $ text $ cs $ showFFloat (Just 1) d.wavelengthMax ""
 where
  cell = width 100

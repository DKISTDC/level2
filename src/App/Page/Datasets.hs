{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.Datasets where

import App.Colors
import App.Effect.Auth
import App.Route
import App.Route qualified as Route
import App.Style qualified as Style
import App.View.Common
import App.View.Common (showDate)
import App.View.DataRow (dataRows)
import App.View.Icons as Icons
import App.View.Layout
import App.View.Loading as View
import Data.Aeson qualified as A
import Data.Grouped
import Data.List qualified as L
import Data.Ord (Down (..))
import Data.String.Interpolate (i)
import Debug.Trace
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Sync as Sync
import NSO.Metadata (DatasetAvailable (..), Metadata)
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Numeric (showFFloat)
import Web.Hyperbole
import Web.View.Style (addClass, cls, prop)


-- TODO: we want to be able to show progress
-- start the work, then scan everything. Why not one at a time?
-- TODO: Store status in-memory using worker state
-- TODO: View errors from the in-memory process, etc. Recent jobs, etc
--
-- TODO: this page should show: recent scans... With their results... Which items were updated, etc
-- can click into different scans
--
-- TODO: A single Scan should show:
-- current progress! It might be in-progress
-- new datasets
-- updated datasets
-- uchanged datasets
-- errors
-- filter by each

-- import NSO.Data.Dataset
-- import NSO.Data.Types

page :: (Hyperbole :> es, Datasets :> es, MetadataSync :> es, Auth :> es) => Eff es (Page '[Current, Syncs, ScanProp, DatasetRow])
page = do
  ids <- send $ Datasets.Ids
  syncs <- loadSyncs
  appLayout (Datasets DatasetRoot) $ do
    col (Style.page) $ do
      col section $ do
        hyper Syncs $ viewSyncs syncs

      col section $ do
        el (bold . fontSize 24) "Current Datasets"
        hyper Current $ viewExistingDatasets ids []


-- Scan --------------------------------------------------

data Syncs = Syncs
  deriving (Show, Read, ViewId)


instance (MetadataSync :> es) => HyperView Syncs es where
  data Action Syncs
    = SyncsRefresh
    deriving (Show, Read, ViewAction)


  type Require Syncs = '[DatasetRow, ScanProp]


  update SyncsRefresh = do
    syncs <- loadSyncs
    -- res <- runScanProposals gds
    pure $ viewSyncs syncs


loadSyncs :: (MetadataSync :> es) => Eff es [SyncState]
loadSyncs = do
  syncIds <- send $ Sync.History
  mapM (\s -> send $ Sync.Get s) syncIds


viewSyncs :: [SyncState] -> View Syncs ()
viewSyncs syncs = do
  col (gap 10 . onLoad SyncsRefresh 1000) $ do
    el (bold . fontSize 24) "Last Metadata Sync"
    case (L.sortOn (Down . (.started)) syncs) of
      [] -> do
        View.loadingCard
      [s] -> do
        viewSyncDetails s
      (s : ss) -> do
        viewSyncDetails s
        tag "hr" id none
        el (bold . fontSize 24) "Metadata Sync History"
        mapM_ viewSyncSummary ss


viewSyncSummary :: SyncState -> View Syncs ()
viewSyncSummary s = do
  row (gap 10) $ do
    el_ $ text $ cs $ showDate s.started


-- each proposal needs its own section!
viewSyncDetails :: SyncState -> View Syncs ()
viewSyncDetails s = do
  el bold $ text $ cs $ showDate s.started
  -- el (color Danger) $ text $ cs $ show s.error
  -- code id $ cs $ show s.proposals
  forM_ s.scans $ \sc -> do
    viewSyncProposal sc


viewSyncProposal :: ScanProposal -> View Syncs ()
viewSyncProposal scan = do
  let skips = filter (\d -> d.sync == Skip) scan.datasets
  let other = filter (\d -> d.sync /= Skip) scan.datasets

  col (gap 5) $ do
    row (gap 10) $ do
      el bold $ text scan.proposalId.fromId
      el italic $ text $ (cs $ show $ length skips) <> " skipped"

    forM_ scan.errors $ \e -> do
      el (color Danger . pad (XY 5 0)) (text $ cs $ show e)

    mapM_ viewSyncDataset other


viewSyncDataset :: SyncDataset -> View Syncs ()
viewSyncDataset s = do
  row (gap 10 . pad (XY 5 0)) $ do
    route (Route.Datasets $ Route.Dataset s.dataset.datasetId) Style.link $ text $ s.dataset.datasetId.fromId
    case s.sync of
      New -> el_ "New"
      Update -> el_ "Update"
      Skip -> el_ "Skip"


-- viewStartScan :: View Syncs ()
-- viewStartScan = do
--   col (gap 10) $ do
--     button StartScan (pad 10 . bold . fontSize 24 . Style.btn Primary . onRequest Style.disabled) "Run Scan"
--     el (display None . onRequest (display Block) . section) View.loadingCard

-- viewScanProps :: [Grouped (Id Proposal) DatasetAvailable] -> View Syncs ()
-- viewScanProps gds =
--   col (gap 15) $ do
--     forM_ gds $ \g -> do
--       col (gap 10 . section) $ do
--         hyper (ScanProp (Id (sample g).primaryProposalId)) viewPropInit

-- Proposal Scan ----------------------------------------------------

data ScanProp = ScanProp (Id Proposal)
  deriving (Show, Read, ViewId)


instance (Datasets :> es, Time :> es, Metadata :> es) => HyperView ScanProp es where
  data Action ScanProp
    = RunScanProposal
    deriving (Show, Read, ViewAction)


  update RunScanProposal = do
    ScanProp propId <- viewId
    scan <- runScanProposal propId
    pure $ viewPropScan scan


viewPropInit :: View ScanProp ()
viewPropInit = do
  ScanProp propId <- context
  col (gap 10) $ do
    el (bold . onLoad RunScanProposal 100) $ text propId.fromId
    View.loadingCard


viewPropScan :: ScanProposal -> View ScanProp ()
viewPropScan scan = do
  ScanProp propId <- viewId
  el bold $ text propId.fromId
  forM_ scan.errors $ \e -> do
    el (color Danger) (text $ cs $ show e)
  forM_ scan.datasets $ \d -> do
    row (gap 10) $ do
      el id (text d.dataset.datasetId.fromId)
      case d.sync of
        New -> el_ "New"
        Update -> el_ "Update"
        Skip -> el_ "Skip"


-- Current Datasets --------------------------------------------------

data Current = Current
  deriving (Show, Read, ViewId)


instance (Time :> es, Datasets :> es, Metadata :> es) => HyperView Current es where
  data Action Current
    = LoadExisting
    deriving (Show, Read, ViewAction)


  type Require Current = '[DatasetRow]


  update LoadExisting = do
    ds <- Datasets.find Datasets.All
    pure $ viewExistingDatasets [] ds


viewDeleted :: [Id Dataset] -> View Current ()
viewDeleted ds = do
  col Style.page $ do
    col (gap 5) $ do
      el bold "Deleted Old Datasets:"
      forM_ ds $ \d -> do
        el_ (text d.fromId)


viewPage :: View Current () -> View Current ()
viewPage content = do
  col (gap 10) $ do
    content


viewDatasetSummary :: [Dataset] -> View Current ()
viewDatasetSummary ds = do
  row (gap 5 . section) $ do
    el_ "Datasets: "
    el_ $ text $ cs $ show $ length ds


-- viewCurrent :: Either [Dataset] SyncResults -> View Current ()
-- viewCurrent res = do
--   case res of
--     Left ds -> viewExistingDatasets ds
--     Right sync -> viewScanResults sync

viewExistingDatasets :: [Id Dataset] -> [Dataset] -> View Current ()
viewExistingDatasets ds [] = do
  el (onLoad LoadExisting 100) $ View.loadingCard
viewExistingDatasets _ ds = do
  mapM_ (\d -> el id $ text $ d.datasetId.fromId) ds


-- viewScanResults :: ScanResult -> View Current ()
-- viewScanResults sr = do
--   col section $ do
--     el (bold . fontSize 24) "Errors"
--     errorsTable sr.errors
--
--   col section $ do
--     el (bold . fontSize 24) "Updated"
--     datasetsTable sr.updated
--
--   col section $ do
--     el (bold . fontSize 24) "New"
--     datasetsTable sr.new
--
--   col section $ do
--     el (bold . fontSize 24) "Unchanged"
--     datasetsTable sr.unchanged

errorsTable :: [ScanError] -> View Current ()
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

-- datasetsTable :: [Dataset] -> View Current ()
-- datasetsTable [] = none
-- datasetsTable ds = do
--   let sorted = sortOn (\d -> (d.primaryProposalId, d.instrumentProgramId, d.datasetId)) ds :: [Dataset]
--   col (gap 0) $ do
--     dataRows sorted $ \d -> do
--       hyper (DatasetRow d.datasetId) $ datasetRow d

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

section :: Mod id
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


datasetRow :: Id Dataset -> View DatasetRow ()
datasetRow did = do
  row (onLoad Details 100) $ do
    route (Route.Datasets $ Route.Dataset did) (Style.link . width 100) $ text $ cs did.fromId


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

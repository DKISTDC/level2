module App.Page.Scan where

import App.Colors
import App.Globus
import App.Route
import App.Style qualified as Style
import App.View.Common
import App.View.DataRow qualified as View
import App.View.Icons as Icons
import App.View.Layout
import Effectful.Error.Static
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Qualify (dayOfYear, isOnDisk)
import NSO.Data.Scan
import NSO.Error
import NSO.Metadata
import NSO.Prelude
import Numeric (showFFloat)
import Web.Hyperbole


-- import NSO.Data.Dataset
-- import NSO.Data.Types

data ScanView = ScanView
  deriving (Generic, ViewId)


data PageEvent
  = RunScan
  deriving (Generic, ViewAction)


instance HyperView ScanView where
  type Action ScanView = PageEvent


page :: (Hyperbole :> es, Time :> es, Datasets :> es, Metadata :> es, Error DataError :> es, Auth :> es) => Page es Response
page = do
  handle pageEvent

  load $ do
    appLayout Scan $ do
      hyper ScanView $ viewScan Nothing


pageEvent :: (Hyperbole :> es, Time :> es, Datasets :> es, Metadata :> es, Error DataError :> es) => ScanView -> PageEvent -> Eff es (View ScanView ())
pageEvent _ RunScan = do
  ds <- syncDatasets
  pure $ viewScan (Just ds)


viewScan :: Maybe SyncResults -> View ScanView ()
viewScan msr =
  onRequest loading $ do
    col Style.page $ do
      button RunScan (pad 10 . bold . fontSize 24 . Style.btn Primary) "Run Scan"

      maybe (pure ()) viewScanResults msr
 where
  loading = row (pad 100 . grow) $ do
    space
    el (width 200 . color (light Primary)) Icons.spinner
    space


viewScanResults :: SyncResults -> View ScanView ()
viewScanResults sr = do
  col section $ do
    el (bold . fontSize 24) "Updated"
    datasetsTable sr.updated

  col section $ do
    el (bold . fontSize 24) "New"
    datasetsTable sr.new

  col section $ do
    el (bold . fontSize 24) "Unchanged"
    datasetsTable sr.unchanged
 where
  section = Style.card . gap 15 . pad 15


-----------------------------------------------------
-- Datasets (Debug)
-----------------------------------------------------

datasetsTable :: [Dataset] -> View ScanView ()
datasetsTable [] = none
datasetsTable ds = do
  let sorted = sortOn (.datasetId) ds :: [Dataset]

  table View.table sorted $ do
    -- tcol (hd "Input Id") $ \d -> cell . cs . show $ d.inputDatasetObserveFramesPartId
    tcol (hd "Id") $ \d -> cell d.datasetId.fromId
    -- tcol (hd "Obs Prog Id") $ \d -> cell d.observingProgramExecutionId.fromId
    tcol (hd "Instrument") $ \d -> cell . cs . show $ d.instrument
    tcol (hd "Stokes") $ \d -> cell . cs . show $ d.stokesParameters
    tcol (hd "Create Date") $ \d -> cell . showTimestamp $ d.createDate
    tcol (hd "Wave Min") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMin ""
    tcol (hd "Wave Max") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMax ""
    -- tcol (hd "Start Time") $ \d -> cell . showTimestamp $ d.startTime
    -- tcol (hd "Exposure Time") $ \d -> cell . cs . show $ d.exposureTime
    -- tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
    tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
    -- tcol (hd "Bounding Box") $ \d -> cell . cs . show $ d.boundingBox
    tcol (hd "On Disk") $ \d -> cell . cs $ maybe "" (show . isOnDisk (dayOfYear d.startTime)) d.boundingBox
 where
  hd = View.hd
  cell = View.cell . text

-- tcol cell (hd "End Time") $ \d -> cell . cs . show $ d.endTime
-- tcol cell (hd "peid") $ \d -> cell . cs $ d.primaryExperimentId
-- tcol cell (hd "ppid") $ \d -> cell . cs $ d.primaryProposalId
-- tcol cell (hd "ExperimentDescription") $ \d -> cell . cs . show $ d.experimentDescription

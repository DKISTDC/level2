module App.Page.Scan where

import App.Colors
import App.Route
import App.Style qualified as Style
import App.View.Common
import App.View.DataRow qualified as View
import App.View.Layout
import Data.String.Interpolate
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
  deriving (Show, Read, Param)


data PageEvent
  = RunScan
  deriving (Show, Read, Param)


instance HyperView ScanView where
  type Action ScanView = PageEvent


page :: (Hyperbole :> es, Time :> es, Datasets :> es, Metadata :> es, Error DataError :> es, Layout :> es) => Page es Response
page = do
  hyper pageEvent

  load $ do
    appLayout Scan $ do
      viewId ScanView $ viewScan Nothing


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
    el (width 200 . color (light Primary)) spinner
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

spinner :: View c ()
spinner =
  raw
    [i|
<!-- By Sam Herbert (@sherb), for everyone. More @ http://goo.gl/7AJzbL -->
<svg width="100%" height="100%" viewBox="0 0 57 57" xmlns="http://www.w3.org/2000/svg" stroke="currentColor">
    <g fill="none" fill-rule="evenodd">
        <g transform="translate(1 1)" stroke-width="2">
            <circle cx="5" cy="50" r="5">
                <animate attributeName="cy"
                     begin="0s" dur="2.2s"
                     values="50;5;50;50"
                     calcMode="linear"
                     repeatCount="indefinite" />
                <animate attributeName="cx"
                     begin="0s" dur="2.2s"
                     values="5;27;49;5"
                     calcMode="linear"
                     repeatCount="indefinite" />
            </circle>
            <circle cx="27" cy="5" r="5">
                <animate attributeName="cy"
                     begin="0s" dur="2.2s"
                     from="5" to="5"
                     values="5;50;50;5"
                     calcMode="linear"
                     repeatCount="indefinite" />
                <animate attributeName="cx"
                     begin="0s" dur="2.2s"
                     from="27" to="27"
                     values="27;49;5;27"
                     calcMode="linear"
                     repeatCount="indefinite" />
            </circle>
            <circle cx="49" cy="50" r="5">
                <animate attributeName="cy"
                     begin="0s" dur="2.2s"
                     values="50;50;5;50"
                     calcMode="linear"
                     repeatCount="indefinite" />
                <animate attributeName="cx"
                     from="49" to="49"
                     begin="0s" dur="2.2s"
                     values="49;5;27;49"
                     calcMode="linear"
                     repeatCount="indefinite" />
            </circle>
        </g>
    </g>
</svg>
|]

module App.Page.Scan where

import App.Colors
import App.Route
import Data.String.Interpolate
import Effectful.Debug
import Effectful.Error.Static
import Effectful.Rel8
import Effectful.Request
import Effectful.Time
import NSO.Data.Dataset
import NSO.Data.Scan
import NSO.Data.Types
import NSO.Prelude
import Numeric (showFFloat)
import Web.Hyperbole
import Web.UI

-- import NSO.Data.Dataset
-- import NSO.Data.Types

data ScanView = ScanView
  deriving (Show, Read, Param)

data PageEvent
  = RunScan
  deriving (Show, Read, Param)

instance LiveView ScanView where
  type Action ScanView = PageEvent

page :: (Page :> es, Debug :> es, Time :> es, Rel8 :> es, GraphQL :> es, Error RequestError :> es) => Eff es ()
page = do
  pageAction pageEvent

  pageLoad $ do
    pure $ appLayout Scan $ do
      liveView ScanView $ viewScan []

pageEvent :: (Page :> es, Debug :> es, Time :> es, Rel8 :> es, GraphQL :> es, Error RequestError :> es) => ScanView -> PageEvent -> Eff es (View ScanView ())
pageEvent _ RunScan = do
  ds <- syncDatasets
  delay 1000
  pure $ viewScan ds

viewScan :: [Dataset] -> View ScanView ()
viewScan ds =
  onRequest loading $ do
    col (gap 10 . pad 20) $ do
      liveButton RunScan (pad 10 . bold . fontSize 24 . bg PrimaryLight . hover |: bg Secondary . color White) "Run Scan"
      datasetsTable ds
 where
  loading = row (pad 100 . grow) $ do
    space
    el (width 200 . color PrimaryLight) spinner
    space

-----------------------------------------------------
-- Datasets (Debug)
-----------------------------------------------------

datasetsTable :: [Dataset] -> View ScanView ()
datasetsTable ds = do
  let sorted = sortOn (.inputDatasetObserveFramesPartId) ds :: [Dataset]

  table (border 1 . pad 0) sorted $ do
    tcol (hd "Input Id") $ \d -> cell . cs . show $ d.inputDatasetObserveFramesPartId
    tcol (hd "Id") $ \d -> cell d.datasetId.fromId
    tcol (hd "Obs Prog Id") $ \d -> cell d.observingProgramExecutionId.fromId
    tcol (hd "Instrument") $ \d -> cell . cs . show $ d.instrument
    tcol (hd "Stokes") $ \d -> cell . cs . show $ d.stokesParameters
    tcol (hd "Create Date") $ \d -> cell . showTimestamp $ d.createDate
    tcol (hd "Wave Min") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMin ""
    tcol (hd "Wave Max") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMax ""
    tcol (hd "Start Time") $ \d -> cell . showTimestamp $ d.startTime
    tcol (hd "Exposure Time") $ \d -> cell . cs . show $ d.exposureTime
    tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
 where
  -- tcol cell (hd "End Time") $ \d -> cell . cs . show $ d.endTime
  -- tcol cell (hd "peid") $ \d -> cell . cs $ d.primaryExperimentId
  -- tcol cell (hd "ppid") $ \d -> cell . cs $ d.primaryProposalId
  -- tcol cell (hd "ExperimentDescription") $ \d -> cell . cs . show $ d.experimentDescription

  hd :: View () () -> View Head ()
  hd = th (bold . bg GrayLight . pad 4 . border 1)

  cell :: Text -> View Dataset ()
  cell = td (pad 4 . border 1) . text

spinner :: View c ()
spinner =
  raw
    $ [i|
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

module App.Page.Dashboard where

import App.Colors
import Data.List.NonEmpty qualified as NE
import Data.Time (defaultTimeLocale, formatTime)
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 (Rel8, query)
import Effectful.Request
import Effectful.Time (Time)
import NSO.Data.Dataset hiding (Id)
import NSO.Data.Scan (scanDatasets)
import NSO.Prelude
import Numeric (showFFloat)
import Web.Hyperbole
import Web.UI hiding (head)

data Route
  = Main
  | Scan
  deriving (Show, Eq, Generic, PageRoute)

route :: (Wai :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => Route -> Eff es ()
route Main = do
  ms <- query () allDatasets
  let ops = toObservingPrograms ms
  view $ viewDashboard ops
route Scan = do
  -- ms <- query () allDatasets
  ds <- scanDatasets
  view $ viewScanRun ds

viewDashboard :: [ObservingProgram] -> View ()
viewDashboard ops = do
  swapTarget InnerHTML $ do
    row_ $ col (pad 10 . gap 10) $ do
      button (action Scan) "Scan Datasets"
      label (fontSize 32) "OBSERVING PROGRAMS"

      col (gap 40) $ do
        forM_ ops viewObservingProgram

viewScanRun :: [Dataset] -> View ()
viewScanRun ds = do
  row_ $ do
    col (pad 10 . gap 10) $ do
      label (fontSize 32) "SCAN RESULTS"
      datasetsTable ds

viewObservingProgram :: ObservingProgram -> View ()
viewObservingProgram op = do
  let ds1 = head op.datasets
  col (gap 8) $ do
    el bold $ text op.programId.fromId
    el_ $ text ds1.experimentDescription
    datasetsTable . NE.toList $ op.datasets

datasetsTable :: [Dataset] -> View ()
datasetsTable ds = do
  let sorted = sortOn (.inputDatasetObserveFramesPartId) ds

  table (border 1 . pad 0) sorted $ do
    tcol cell (hd "Input Id") $ \d -> dcell . cs $ d.inputDatasetObserveFramesPartId.fromId
    tcol cell (hd "Id") $ \d -> dcell d.datasetId.fromId
    tcol cell (hd "Stokes") $ \d -> dcell . cs . show $ d.stokesParameters
    tcol cell (hd "Date") $ \d -> dcell . timestamp $ d.createDate
    tcol cell (hd "Wave Min") $ \d -> dcell . cs $ showFFloat (Just 1) d.wavelengthMin ""
    tcol cell (hd "Wave Max") $ \d -> dcell . cs $ showFFloat (Just 1) d.wavelengthMax ""
    tcol cell (hd "Start Time") $ \d -> dcell . timestamp $ d.startTime
    -- tcol cell (hd "End Time") $ \d -> dcell . cs . show $ d.endTime
    tcol cell (hd "Frame Count") $ \d -> dcell . cs . show $ d.frameCount
    tcol cell (hd "peid") $ \d -> dcell . cs $ d.primaryExperimentId
    tcol cell (hd "ppid") $ \d -> dcell . cs $ d.primaryProposalId
 where
  -- tcol cell (hd "ExperimentDescription") $ \d -> dcell . cs . show $ d.experimentDescription

  timestamp = cs . formatTime defaultTimeLocale "%F %T"

  hd = el (bold . bg GrayLight . pad 4)
  cell = border 1
  dcell :: Text -> View ()
  dcell = el (pad 4) . text

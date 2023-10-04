module App.Page.Dashboard where

import App.Colors
import Data.List.NonEmpty qualified as NE
import Data.Time (defaultTimeLocale, formatTime)
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 (Rel8, query)
import Effectful.Request
import Effectful.Time (Time)
import NSO.Data.Scan (fetchDatasets)
import NSO.Metadata.Types
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
  -- ds <- query () allDatasets
  ds <- fetchDatasets
  view $ viewDashboard ds
route Scan = do
  -- ms <- query () allDatasets
  ds <- fetchDatasets
  view $ viewScanRun ds

viewScanRun :: [DatasetInventory] -> View ()
viewScanRun ds = do
  row_ $ do
    col (pad 10 . gap 10) $ do
      label (fontSize 32) "SCAN RESULTS"
      datasetsTable ds

viewDashboard :: [DatasetInventory] -> View ()
viewDashboard ds = do
  swapTarget InnerHTML $ do
    row_ $ col (pad 10 . gap 10) $ do
      button (action Scan) "Scan Datasets"
      viewExperiments ds
 where
  viewExperiments [] = el_ "No Datasets!"
  viewExperiments (d : ds') = do
    let exs = toExperiments $ d :| ds'
    label (bold . fontSize 32) "EXPERIMENTS"

    col (gap 40) $ do
      forM_ exs viewExperiment

viewExperiment :: Experiment -> View ()
viewExperiment e = do
  let ds1 = e.observingProgramExecutions & head & (.datasets) & head :: DatasetInventory
  col (gap 8) $ do
    el bold $ do
      text "Experiment: "
      text e.experimentId.fromId
    el_ $ text ds1.experimentDescription
    forM_ e.observingProgramExecutions $ \ob -> do
      col (gap 10) $ do
        el bold $ text ob.observingProgramExecutionId.fromId
        datasetsTable . NE.toList $ ob.datasets

viewObservingProgram :: ObservingProgramExecution -> View ()
viewObservingProgram op = do
  let ds1 = head op.datasets
  col (gap 8) $ do
    el bold $ do
      text "Program: "
      text op.observingProgramExecutionId.fromId
    el_ $ text ds1.experimentDescription
    datasetsTable . NE.toList $ op.datasets

datasetsTable :: [DatasetInventory] -> View ()
datasetsTable ds = do
  let sorted = sortOn (.inputDatasetObserveFramesPartId) ds

  table (border 1 . pad 0) sorted $ do
    tcol (hd "Input Id") $ \d -> cell . cs . show $ d.inputDatasetObserveFramesPartId
    tcol (hd "Id") $ \d -> cell d.datasetId.fromId
    tcol (hd "Inst Prog Id") $ \d -> cell d.instrumentProgramExecutionId
    tcol (hd "Instrument") $ \d -> cell d.instrumentName
    tcol (hd "Stokes") $ \d -> cell . cs . show $ d.stokesParameters
    tcol (hd "Date") $ \d -> cell . timestamp $ d.createDate
    tcol (hd "Wave Min") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMin ""
    tcol (hd "Wave Max") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMax ""
    tcol (hd "Start Time") $ \d -> cell . timestamp $ d.startTime
    tcol (hd "Exposure Time") $ \d -> cell . cs . show $ d.exposureTime
    -- tcol cell (hd "End Time") $ \d -> cell . cs . show $ d.endTime
    tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
 where
  -- tcol cell (hd "peid") $ \d -> cell . cs $ d.primaryExperimentId
  -- tcol cell (hd "ppid") $ \d -> cell . cs $ d.primaryProposalId

  -- tcol cell (hd "ExperimentDescription") $ \d -> cell . cs . show $ d.experimentDescription

  timestamp = cs . formatTime defaultTimeLocale "%F %T"

  hd = th (bold . bg GrayLight . pad 4 . border 1)
  cell = td (pad 4 . border 1) . text

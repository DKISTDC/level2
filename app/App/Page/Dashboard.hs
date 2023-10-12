module App.Page.Dashboard where

import App.Colors
import Data.List.NonEmpty qualified as NE
import Data.Time (defaultTimeLocale, formatTime)
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 (Rel8)
import Effectful.Request
import Effectful.Time (Time)
import NSO.Data.Dataset as Dataset
import NSO.Data.Scan qualified as Scan
import NSO.Data.Types
import NSO.Prelude
import Numeric (showFFloat)
import Web.Hyperbole as H
import Web.UI hiding (head)

data Route
  = Main
  | Program (Id ObservingProgram)
  | Programs
  | Experiments
  | Scan
  deriving (Show, Eq, Generic, PageRoute)

route :: (Wai :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => Route -> Eff es ()
route Main = do
  view $ el_ "MAIN"
route Experiments = do
  ds <- Dataset.queryAll
  view $ layout Experiments $ viewExperiments ds
route Programs = do
  ds <- Dataset.queryAll
  view $ layout Programs $ viewPrograms ds
route Scan = do
  ds <- Scan.syncDatasets
  view $ layout Scan $ viewScanRun ds
route (Program op) = do
  view $ el_ $ text $ "PROGRAM" <> cs (show op)

viewScanRun :: [Dataset] -> View ()
viewScanRun ds = do
  row_ $ do
    col (pad 10 . gap 20) $ do
      label (fontSize 32 . bg Warning . pad 25) "Scan Complete!"
      datasetsTable ds

layout :: Route -> View () -> View ()
layout r content = do
  swapTarget InnerHTML $ do
    row (bg Light . color Dark) $ do
      col (gap 0 . bg Primary . width 400 . color White) $ do
        row (pad 20) $ do
          space
          el (bold . fontSize 32) "Level 2"
          space
        nav Programs "Programs"
        nav Experiments "Experiments"
        row (pad 20) $ button (grow . action Scan . pad 20 . bg GrayLight . pointer . color Dark . hover |: bg Light . border 0 . rounded 4 . active |: bold . shadow . hxIndicator (H.Id "content")) "Sync Datasets"

      col (gap 25 . pad 25 . grow . att "id" "content") $ do
        content
 where
  nav r' = link (routeUrl r') (pad 20 . color White . if r' == r then current else id)
  current = bg PrimaryLight . border' (TRBL 0 0 0 5) . padX 15

viewExperiments :: [Dataset] -> View ()
viewExperiments [] = el_ "No Datasets!"
viewExperiments (d : ds') = do
  let exs = toExperiments $ d :| ds'
  label (bold . fontSize 32) "EXPERIMENTS"

  col (gap 40) $ do
    forM_ exs viewExperiment
 where
  viewExperiment :: Experiment -> View ()
  viewExperiment e = do
    let ds1 = e.observingProgramExecutions & head & (.datasets) & head :: Dataset
    col (gap 8) $ do
      el bold $ do
        text "Experiment: "
        text e.experimentId.fromId
      el_ $ text ds1.experimentDescription
      forM_ e.observingProgramExecutions $ \ob -> do
        col (gap 10) $ do
          el bold $ text ob.observingProgramExecutionId.fromId
          datasetsTable . NE.toList $ ob.datasets

viewPrograms :: [Dataset] -> View ()
viewPrograms [] = el_ "No Datasets!"
viewPrograms (d : ds') = do
  let ops = toObservingPrograms (d :| ds') & NE.sortWith (maxCreateDate . (.datasets))
  label (bold . fontSize 32) "OBSERVING PROGRAMS"

  col (gap 40) $ do
    forM_ ops viewProgram
 where
  viewProgram :: ObservingProgram -> View ()
  viewProgram op = do
    -- let ds1 = head op.datasets
    datasetsTable . NE.toList $ op.datasets

datasetsTable :: [Dataset] -> View ()
datasetsTable ds = do
  let sorted = sortOn (.inputDatasetObserveFramesPartId) ds

  table (border 1 . pad 0) sorted $ do
    tcol (hd "Input Id") $ \d -> cell . cs . show $ d.inputDatasetObserveFramesPartId
    tcol (hd "Id") $ \d -> cell d.datasetId.fromId
    tcol (hd "Inst Prog Id") $ \d -> cell d.instrumentProgramExecutionId.fromId
    tcol (hd "Instrument") $ \d -> cell . cs . show $ d.instrument
    tcol (hd "Stokes") $ \d -> cell . cs . show $ d.stokesParameters
    tcol (hd "Create Date") $ \d -> cell . timestamp $ d.createDate
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

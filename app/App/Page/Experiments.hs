module App.Page.Experiments where

import App.Colors
import App.Route
import Data.List.NonEmpty qualified as NE
import Data.Time (defaultTimeLocale, formatTime)
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 (Rel8)
import Effectful.Request
import Effectful.Time (Time)
import NSO.Data.Dataset as Dataset
import NSO.Data.Types
import NSO.Prelude
import Numeric (showFFloat)
import Web.Hyperbole as H
import Web.UI hiding (head)

data MainView = MainView
  deriving (Show, Read, Param)

type MainEvent = ()

-- data MainEvent
--   = Programs
--   | Experiments
--   | Scan
--   deriving (Show, Read, Eq, Param)

instance LiveView MainView where
  type Action MainView = MainEvent

page :: (Page :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => Eff es ()
page = do
  pageAction handle
  pageLoad $ do
    ds <- Dataset.queryAll
    pure $ layout Experiments $ liveView MainView $ viewExperiments ds

handle :: (Page :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => MainView -> MainEvent -> Eff es (View MainView ())
handle _ _ = do
  ds <- Dataset.queryAll
  pure $ viewExperiments ds

-- handle _ Programs = do
--   ds <- Dataset.queryAll
--   pure $ viewPrograms ds
-- handle _ Scan = do
--   ds <- Scan.syncDatasets
--   pure $ viewScanRun ds

viewScanRun :: [Dataset] -> View MainView ()
viewScanRun ds = onRequest loading $ do
  row_ $ do
    col (pad 10 . gap 20) $ do
      label (fontSize 32 . bg Warning . pad 25) "Scan Complete!"
      datasetsTable ds

loading :: View c ()
loading = el_ "loading..."

-- current = bg PrimaryLight . border' (TRBL 0 0 0 5) . padX 15

viewExperiments :: [Dataset] -> View MainView ()
viewExperiments [] = el_ "No Datasets!"
viewExperiments (d : ds') = onRequest loading $ do
  let exs = toExperiments $ d :| ds'
  label (bold . fontSize 32) "EXPERIMENTS"

  col (gap 40) $ do
    forM_ exs viewExperiment
 where
  viewExperiment :: Experiment -> View MainView ()
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

viewPrograms :: [Dataset] -> View MainView ()
viewPrograms [] = el_ "No Datasets!"
viewPrograms (d : ds') = onRequest loading $ do
  let ops = toObservingPrograms (d :| ds') & NE.sortWith (maxCreateDate . (.datasets))
  label (bold . fontSize 32) "OBSERVING PROGRAMS"

  col (gap 40) $ do
    forM_ ops viewProgram
 where
  viewProgram :: ObservingProgram -> View MainView ()
  viewProgram op = do
    -- let ds1 = head op.datasets
    datasetsTable . NE.toList $ op.datasets

datasetsTable :: [Dataset] -> View MainView ()
datasetsTable ds = do
  let sorted = sortOn (.inputDatasetObserveFramesPartId) ds :: [Dataset]

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
    tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
 where
  -- tcol cell (hd "End Time") $ \d -> cell . cs . show $ d.endTime
  -- tcol cell (hd "peid") $ \d -> cell . cs $ d.primaryExperimentId
  -- tcol cell (hd "ppid") $ \d -> cell . cs $ d.primaryProposalId
  -- tcol cell (hd "ExperimentDescription") $ \d -> cell . cs . show $ d.experimentDescription

  timestamp = cs . formatTime defaultTimeLocale "%F %T"

  hd :: View () () -> View Head ()
  hd = th (bold . bg GrayLight . pad 4 . border 1)

  cell :: Text -> View Dataset ()
  cell = td (pad 4 . border 1) . text

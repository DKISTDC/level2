module App.Page.Experiments where

import App.Colors
import App.Route as Route
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 (Rel8)
import Effectful.Request
import Effectful.Time (Time)
import NSO.Data.Dataset as Dataset
import NSO.Data.Types
import NSO.Prelude hiding (truncate)
import Numeric (showFFloat)
import Web.Hyperbole as H
import Web.UI hiding (head)

page :: (Page :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => Eff es ()
page = do
  pageAction handle
  pageAction handleIPRow
  pageLoad $ do
    ds <- Dataset.queryAll
    pure $ appLayout Experiments $ liveView MainView $ viewExperiments ds

loading :: View c ()
loading = el_ "loading..."

-----------------------------------------------------
-- Experiments --------------------------------------
-----------------------------------------------------

data MainView = MainView
  deriving (Show, Read, Param)

type MainEvent = ()

instance LiveView MainView where
  type Action MainView = MainEvent

handle :: (Page :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => MainView -> MainEvent -> Eff es (View MainView ())
handle _ _ = do
  ds <- Dataset.queryAll
  pure $ viewExperiments ds

viewExperiments :: [Dataset] -> View MainView ()
viewExperiments [] = el_ "No Datasets!"
viewExperiments (d : ds') = col (pad 15 . gap 20) $ onRequest loading $ do
  let exs = toExperiments $ d :| ds'
  label (bold . fontSize 32) "EXPERIMENTS"

  col (gap 40) $ do
    forM_ exs viewExperiment
 where
  viewExperiment :: Experiment -> View MainView ()
  viewExperiment e = do
    let ds1 = e.instrumentPrograms & head & (.datasets) & head :: Dataset
    col (gap 8 . bg White) $ do
      link (routeUrl $ Route.Experiment e.experimentId) (bold . border (TRBL 0 0 1 0) . pad 10) $ do
        text "Experiment "
        text e.experimentId.fromId
      col (gap 8 . pad 10) $ do
        el truncate $ text ds1.experimentDescription
        tableInstrumentPrograms $ NE.toList e.instrumentPrograms

tableInstrumentPrograms :: [InstrumentProgram] -> View MainView ()
tableInstrumentPrograms ips = do
  let sorted = ips
  col_ $ do
    row dataRow $ do
      el header none
      button header "Start Date"
      button header "Instrument"
    dataRows sorted $ \ip -> do
      liveView (IPRow ip.instrumentProgramId) $ rowInstrumentProgram ip
 where
  header = dataCell . bold

dataRows :: [a] -> (a -> View c ()) -> View c ()
dataRows as viewRow = forM_ (zip (cycle [True, False]) as) $ \(b, a) ->
  el (dataRow . alternateColor b) $ viewRow a
 where
  alternateColor b = if b then bg Light else id

dataCell :: Mod
dataCell = width 100

dataRow :: Mod
dataRow = gap 10 . pad (All dataRowPadding)

dataRowPadding :: PxRem
dataRowPadding = 5

dataRowHeight :: PxRem
dataRowHeight = 16 + 2 * dataRowPadding

-----------------------------------------------------
-- IPRow
-----------------------------------------------------

newtype IPRow = IPRow (Id InstrumentProgram)
  deriving newtype (Show, Read, Param)

data IPEvent
  = Expand
  | Collapse
  deriving (Show, Read, Param)

instance LiveView IPRow where
  type Action IPRow = IPEvent

handleIPRow :: (Page :> es, Rel8 :> es) => IPRow -> IPEvent -> Eff es (View IPRow ())
handleIPRow (IPRow i) a = do
  dss <- queryProgram i
  case dss of
    [] -> pure $ el_ "Mising Datasets!"
    (d : ds) -> action (instrumentProgram (d :| ds)) a
 where
  action ip Expand =
    pure $ viewInstrumentProgram ip
  action ip Collapse =
    pure $ rowInstrumentProgram ip

rowInstrumentProgram :: InstrumentProgram -> View IPRow ()
rowInstrumentProgram ip = el (transition Height 0.5 . height dataRowHeight) $ do
  liveButton Expand id $ row (gap 10) $ do
    el (dataCell . bg Warning) $ text "Status"
    el dataCell $ text $ showDate ip.createDate
    el dataCell $ text $ cs $ show ip.instrument
    el dataCell $ text $ cs $ show $ length ip.datasets

viewInstrumentProgram :: InstrumentProgram -> View IPRow ()
viewInstrumentProgram ip = el (transition Height 0.5 . height 200) $ do
  col (height 100) $ do
    el_ $ text $ cs $ show ip.instrumentProgramId
    liveButton Collapse (bg Primary . color White) "Collapse"

-----------------------------------------------------
-- Datasets (Debug)
-----------------------------------------------------

datasetsTable :: [Dataset] -> View MainView ()
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

viewScanRun :: [Dataset] -> View MainView ()
viewScanRun ds = onRequest loading $ do
  row_ $ do
    col (pad 10 . gap 20) $ do
      label (fontSize 32 . bg Warning . pad 25) "Scan Complete!"
      datasetsTable ds

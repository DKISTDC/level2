module App.Page.Experiments where

import App.Colors
import App.Route as Route
import App.View.DataRow (dataRowHeight, dataRows)
import App.View.DatasetsTable as DatasetsTable
import App.View.InstrumentProgramSummary as InstrumentProgramSummary
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 (Rel8)
import Effectful.Request
import Effectful.Time (Time)
import NSO.Data.Dataset as Dataset
import NSO.Data.Qualify as Qualify
import NSO.Data.Types
import NSO.Prelude hiding (truncate)
import Web.Hyperbole as H
import Web.UI

page :: (Page :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => Eff es ()
page = do
  pageAction handle
  pageAction handleIPRow
  pageLoad $ do
    ds <- Dataset.queryAll
    pure $ appLayout Experiments $ liveView MainView $ viewExperiments (Filters Nothing (Just VISP)) ds

loading :: View c ()
loading = el_ "loading..."

-----------------------------------------------------
-- Experiments --------------------------------------
-----------------------------------------------------

data MainView = MainView
  deriving (Show, Read, Param)

data MainEvent = Filter Filters
  deriving (Show, Read, Param)

data Filters = Filters
  { isInvertible :: Maybe Bool
  , isInstrument :: Maybe Instrument
  }
  deriving (Show, Read)

instance LiveView MainView MainEvent

handle :: (Page :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => MainView -> MainEvent -> Eff es (View MainView ())
handle _ (Filter fs) = do
  ds <- Dataset.queryAll
  pure $ viewExperiments fs ds

viewExperiments :: Filters -> [Dataset] -> View MainView ()
viewExperiments _ [] = el_ "No Datasets!"
viewExperiments fs (d : ds') = do
  col (pad 15 . gap 20) $ do
    viewFilters fs

    col (gap 40) $ do
      let exs = reverse $ sortWith (.startTime) $ toExperiments $ d :| ds'
      forM_ exs viewExperiment
 where
  viewExperiment :: Experiment -> View MainView ()
  viewExperiment e = do
    let ds1 = e.instrumentPrograms & head & (.datasets) & head :: Dataset
    col (gap 8 . bg White) $ do
      row (bg GrayLight) $ do
        link (routeUrl $ Route.Experiment e.experimentId) (bold . pad 10) $ do
          text "Experiment "
          text e.experimentId.fromId
        space
        el (pad 10) $ do
          -- text "Started "
          text $ showDate ds1.startTime

      col (gap 8 . pad 10) $ do
        -- row (gap 5) $ do
        --   el bold "Start Time:"
        --   el_ $ text $ showDate ds1.startTime
        link (routeUrl $ Route.Experiment e.experimentId) truncate $ text ds1.experimentDescription

        -- let filters = foldr _ (const True) fs :: InstrumentProgram -> Bool
        let shown = filter applyFilters $ NE.toList e.instrumentPrograms

        tableInstrumentPrograms shown

        let ignored = length e.instrumentPrograms - length shown
        when (ignored > 0) $ do
          link (routeUrl $ Route.Experiment e.experimentId) (fontSize 14 . color GrayDark) $ do
            text $ cs (show ignored)
            text "Hidden Instrument Programs"

  applyFilters :: InstrumentProgram -> Bool
  applyFilters ip = checkInstrument ip && checkInvertible ip

  checkInstrument :: InstrumentProgram -> Bool
  checkInstrument ip =
    case fs.isInstrument of
      Nothing -> True
      (Just i) -> i == ip.instrument

  checkInvertible :: InstrumentProgram -> Bool
  checkInvertible ip =
    case fs.isInvertible of
      Nothing -> True
      (Just i) -> i == Qualify.isQualified ip

-- applyFilter :: Filter -> (InstrumentProgram -> Bool) -> (InstrumentProgram -> Bool)
-- applyFilter Invertible f ip = f ip && Qualify.isQualified ip
-- applyFilter (IsInstrument i) f ip = f ip && ip.instrument == i

viewFilters :: Filters -> View MainView ()
viewFilters fs = do
  row (gap 10) $ do
    liveSelect (\i -> Filter $ fs{isInstrument = i}) (== fs.isInstrument) $ do
      option Nothing id ""
      option (Just VISP) id "VISP"
      option (Just VBI) id "VBI"

    liveSelect (\i -> Filter $ fs{isInvertible = i}) (== fs.isInvertible) $ do
      option Nothing id ""
      option (Just True) id "Invertible"
      option (Just False) id "Not Invertible"

tableInstrumentPrograms :: [InstrumentProgram] -> View MainView ()
tableInstrumentPrograms ips = do
  let sorted = ips
  col_ $ do
    -- row dataRow $ do
    --   el header "Instrument Programs"
    --   button header "Create Date"
    --   -- button header "Start Date"
    --   button header "Instrument"
    dataRows sorted $ \ip -> do
      liveView (IPRow ip.instrumentProgramId) $ rowInstrumentProgram Expand ip

-----------------------------------------------------
-- IPRow
-----------------------------------------------------

newtype IPRow = IPRow (Id InstrumentProgram)
  deriving newtype (Show, Read, Param)

data IPEvent
  = Expand
  | Collapse
  deriving (Show, Read, Param)

instance LiveView IPRow IPEvent

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
    pure $ rowInstrumentProgram Expand ip

rowInstrumentProgram :: IPEvent -> InstrumentProgram -> View IPRow ()
rowInstrumentProgram onClick ip = el (transition Height 500 . height dataRowHeight) $ do
  liveButton onClick id $ row (gap 10) $ do
    InstrumentProgramSummary.viewRow ip

viewInstrumentProgram :: InstrumentProgram -> View IPRow ()
viewInstrumentProgram ip = do
  el (transition Height 500 . height 400 . truncate) $ do
    let ds = NE.toList ip.datasets
    rowInstrumentProgram Collapse ip

    InstrumentProgramSummary.viewCriteria ip

    DatasetsTable.datasetsTable ds

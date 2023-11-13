module App.Page.Experiments where

import App.Colors
import App.Route as Route
import App.View.Common
import App.View.DataRow (dataRows)
import App.View.InstrumentProgramSummary as InstrumentProgramSummary
import Data.Grouped as G
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 (Rel8)
import Effectful.Request
import Effectful.Time (Time)
import NSO.Data.Dataset as Dataset
import NSO.Data.Program as Program
import NSO.Prelude hiding (truncate)
import NSO.Types.InstrumentProgram
import Web.Hyperbole as H
import Web.UI

page :: (Page :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => Eff es ()
page = do
  pageAction handle
  -- pageAction handleIPRow
  pageLoad $ do
    exs <- Program.loadAllExperiments
    pure $ appLayout Experiments $ do
      liveView MainView $ do
        viewExperiments (Filters Nothing (Just VISP)) exs

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
  exs <- Program.loadAllExperiments
  pure $ viewExperiments fs exs

-- ok, wait, I need to group them by experiment
viewExperiments :: Filters -> [Experiment] -> View MainView ()
viewExperiments fs exs = do
  col (pad 15 . gap 20) $ do
    viewFilters fs

    col (gap 40) $ do
      forM_ exs viewExperiment
 where
  viewExperiment :: Experiment -> View MainView ()
  viewExperiment e = do
    let shown = filter applyFilters $ G.toList e.programs
    experimentPrograms e shown

  experimentPrograms :: Experiment -> [InstrumentProgram] -> View MainView ()
  experimentPrograms _ [] = none
  experimentPrograms e ips = do
    col (gap 8 . bg White) $ do
      row (bg GrayLight) $ do
        -- link (routeUrl $ Route.Experiment e.experimentId) (bold . pad 10) $ do
        el (bold . pad 10) $ do
          text "Experiment "
          text e.experimentId.fromId
        space
        el (pad 10) $ do
          text $ showDate e.startTime

      col (gap 8 . pad 10) $ do
        -- row (gap 5) $ do
        --   el bold "Start Time:"
        --   el_ $ text $ showDate ds1.startTime
        -- link (routeUrl $ Route.Experiment e.experimentId) truncate $ text ds1.experimentDescription
        el truncate $ text e.description

        tableInstrumentPrograms ips

        let ignored = length e.programs - length ips
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
      (Just False) -> ip.status == Invalid
      (Just True) -> ip.status /= Invalid

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
    dataRows sorted $ \ip -> do
      rowInstrumentProgram ip

-----------------------------------------------------
-- IPRow
-----------------------------------------------------

-- newtype IPRow = IPRow (Id InstrumentProgram)
--   deriving newtype (Show, Read, Param)
--
-- data IPEvent
--   = Expand
--   | Collapse
--   deriving (Show, Read, Param)
--
-- instance LiveView IPRow IPEvent
--
-- handleIPRow :: (Page :> es, Rel8 :> es) => IPRow -> IPEvent -> Eff es (View IPRow ())
-- handleIPRow (IPRow i) a = do
--   dss <- queryProgram i
--   ps <- loadProvenance i
--   case dss of
--     [] -> pure $ el_ "Mising Datasets!"
--     (d : ds) -> do
--       let ip = instrumentProgram (d :| ds)
--       let psm = programSummary ip ps
--       action psm a
--  where
--   action psm Expand =
--     pure $ viewInstrumentProgram psm.program
--   action psm Collapse =
--     pure $ rowInstrumentProgram Expand psm

rowInstrumentProgram :: InstrumentProgram -> View c ()
rowInstrumentProgram psm = do
  -- liveButton onClick id $ do
  link (routeUrl (Program psm.programId)) id $ do
    InstrumentProgramSummary.viewRow psm

-- viewInstrumentProgram :: ProgramSummary -> View IPRow ()
-- viewInstrumentProgram psm = do
--   let ds = NE.toList psm.programm.datasets
--   el (transition 500 (Height (viewHeight ds)) . truncate) $ do
--     rowInstrumentProgram Collapse psm
--
--     InstrumentProgramSummary.viewCriteria psm
--
--     DatasetsTable.datasetsTable ds
--  where
--   viewHeight ds =
--     dataRowHeight
--       + ((PxRem (length ds) + 1) * DatasetsTable.rowHeight)
--       + InstrumentProgramSummary.summaryHeight

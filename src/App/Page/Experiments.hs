module App.Page.Experiments where

import App.Colors
import App.Route as Route
import App.Style qualified as Style
import App.View.Common as View
import App.View.DataRow (dataRows)
import App.View.ExperimentDetails (viewProgramRow)
import App.View.Layout
import Data.Grouped as G
import Data.Ord (Down (..))
import Effectful
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs as Programs
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole as H


page
  :: (Hyperbole :> es, Datasets :> es, Inversions :> es, Time :> es, Layout :> es)
  => Page es Response
page = do
  hyper experiments
  -- pageAction handleIPRow
  load $ do
    exs <- Programs.loadAllExperiments
    now <- currentTime

    let fs = Filters{isVBI = False, isVISP = True, inversionStatus = Any}

    appLayout Experiments $ do
      viewId ExView $ do
        viewExperiments now fs exs


loading :: View c ()
loading = el_ "loading..."


-----------------------------------------------------
-- Experiments --------------------------------------
-----------------------------------------------------

data ExView = ExView
  deriving (Show, Read, Param)


data ExEvent = Filter Filters
  deriving (Show, Read, Param)


instance HyperView ExView where
  type Action ExView = ExEvent


data Filters = Filters
  { inversionStatus :: InversionFilter
  , isVISP :: Bool
  , isVBI :: Bool
  }
  deriving (Show, Read)


data InversionFilter
  = Any
  | Qualified
  | Inverting
  | Complete
  deriving (Show, Read, Eq)


experiments
  :: (Hyperbole :> es, Datasets :> es, Inversions :> es, Time :> es)
  => ExView
  -> ExEvent
  -> Eff es (View ExView ())
experiments _ (Filter fs) = do
  exs <- Programs.loadAllExperiments
  now <- currentTime
  pure $ viewExperiments now fs exs


-- ok, wait, I need to group them by experiment
viewExperiments :: UTCTime -> Filters -> [Experiment] -> View ExView ()
viewExperiments now fs exs = do
  let sorted = sortOn (Down . (.experimentId)) exs
  el (pad 15 . gap 20 . big flexRow . small flexCol . grow) $ do
    row (big aside . gap 5) $ do
      viewFilters fs

    col (gap 40 . grow . collapse) $ do
      forM_ sorted viewExperiment
 where
  aside = width 250 . flexCol
  big = media (MinWidth 1000)
  small = media (MaxWidth 1000)

  viewExperiment :: Experiment -> View ExView ()
  viewExperiment e = do
    let shown = filter applyFilters $ G.toList e.programs
    experimentPrograms e shown

  experimentPrograms :: Experiment -> [InstrumentProgram] -> View ExView ()
  experimentPrograms _ [] = none
  experimentPrograms e ips = do
    col (Style.card . gap 15 . pad 15) $ do
      row id $ do
        el bold $ do
          text "Experiment "
          link (Route.Experiment e.experimentId) Style.link $ do
            text e.experimentId.fromId
        space
        el_ $ do
          text $ showDate e.startTime

      View.hr (color Gray)

      col (gap 10) $ do
        -- row (gap 5) $ do
        --   el bold "Start Time:"
        --   el_ $ text $ showDate ds1.startTime
        el truncate $ text e.description

        tableInstrumentPrograms now ips

        let ignored = length e.programs - length ips
        when (ignored > 0) $ do
          link (Route.Experiment e.experimentId) (fontSize 14 . color Black) $ do
            text $ cs (show ignored)
            text "Hidden Instrument Programs"

  applyFilters :: InstrumentProgram -> Bool
  applyFilters ip = checkInstrument ip && checkInvertible fs.inversionStatus ip.status

  checkInstrument :: InstrumentProgram -> Bool
  checkInstrument ip =
    case ip.instrument of
      VBI -> fs.isVBI
      VISP -> fs.isVISP

  checkInvertible :: InversionFilter -> ProgramStatus -> Bool
  checkInvertible Any _ = True
  checkInvertible Qualified StatusQualified = True
  checkInvertible Inverting (StatusInversion (StepPublished _)) = False
  checkInvertible Inverting (StatusInversion _) = True
  checkInvertible Complete (StatusInversion (StepPublished _)) = True
  checkInvertible _ _ = False


-- applyFilter :: Filter -> (InstrumentProgram -> Bool) -> (InstrumentProgram -> Bool)
-- applyFilter Invertible f ip = f ip && Qualify.isQualified ip
-- applyFilter (IsInstrument i) f ip = f ip && ip.instrument == i

viewFilters :: Filters -> View ExView ()
viewFilters fs = do
  el (item . bold) "Instrument"

  row (gap 5) $ do
    toggle (Filter fs{isVISP = not fs.isVISP}) fs.isVISP id "VISP"
    toggle (Filter fs{isVBI = not fs.isVBI}) fs.isVBI id "VBI"

  -- dropdown (\i -> Filter $ fs{isInstrument = i}) (== fs.isInstrument) $ do
  --   option Nothing id ""
  --   option (Just VISP) id "VISP"
  --   option (Just VBI) id "VBI"

  el (item . bold) "Status"
  dropdown (\i -> Filter $ fs{inversionStatus = i}) (== fs.inversionStatus) (item . pad 5) $ do
    option Any "Any"
    option Qualified "Qualified"
    option Inverting "Active"
    option Complete "Complete"
 where
  toggle action sel f =
    button action (f . item . pad (XY 10 5) . Style.btn (if sel then on else off))

  item = pad (XY 0 5)

  on = Primary
  off = Gray


tableInstrumentPrograms :: UTCTime -> [InstrumentProgram] -> View ExView ()
tableInstrumentPrograms now ips = do
  let sorted = ips
  col id $ do
    dataRows sorted $ \ip -> do
      rowInstrumentProgram now ip


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

rowInstrumentProgram :: UTCTime -> InstrumentProgram -> View c ()
rowInstrumentProgram now psm = do
  -- liveButton onClick id $ do
  link (Program psm.programId) id $ do
    viewProgramRow now psm

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

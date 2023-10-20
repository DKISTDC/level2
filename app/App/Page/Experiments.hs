module App.Page.Experiments where

import App.Colors
import App.Route as Route
import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 (Rel8)
import Effectful.Request
import Effectful.Time (Time)
import NSO.Data.Dataset as Dataset
import NSO.Data.Spectral as Spectral
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
  let exs = reverse $ sortWith (.startTime) $ toExperiments $ d :| ds'

  col (gap 40) $ do
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

        let visp = filter isVisp $ NE.toList e.instrumentPrograms

        tableInstrumentPrograms visp

        let ignored = length e.instrumentPrograms - length visp
        when (ignored > 0) $ do
          link (routeUrl $ Route.Experiment e.experimentId) (color GrayDark) $ do
            text $ cs (show ignored)
            text "Other Instruments Ignored"

  isVisp :: InstrumentProgram -> Bool
  isVisp ip = ip.instrument == VISP

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
 where

-- header = dataCell . bold

dataRows :: [a] -> (a -> View c ()) -> View c ()
dataRows as viewRow = forM_ (zip (cycle [True, False]) as) $ \(b, a) ->
  el (dataRow . alternateColor b) $ viewRow a
 where
  alternateColor b = if b then bg Light else id

dataCell :: Mod
dataCell = minWidth 100

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
    pure $ rowInstrumentProgram Expand ip

rowInstrumentProgram :: IPEvent -> InstrumentProgram -> View IPRow ()
rowInstrumentProgram onClick ip = el (transition Height 0.5 . height dataRowHeight) $ do
  liveButton onClick id $ row (gap 10) $ do
    el (dataCell . bg Warning) $ text "Status"
    el dataCell $ text $ showDate ip.createDate
    -- el dataCell $ text $ showDate ip.startTime
    el dataCell $ text $ cs $ show ip.instrument
    -- not worth showing Stokes in the row. They seem to be present for all VISP
    -- el dataCell $ text $ cs $ show ip.stokesParameters

    -- TODO: on disk
    row (dataCell . gap 5 . fontSize 14) $ do
      let (mids, lns) = partitionEithers $ map identify $ NE.toList ip.datasets
      mapM_ lineTag lns
      mapM_ midTag $ sortOn id mids
 where
  identify :: Dataset -> Either (Wavelength Nm) SpectralLine
  identify d =
    case Spectral.identifyLine d.wavelengthMin d.wavelengthMax of
      Nothing -> Left (midWave d)
      Just l -> Right l

  midWave :: Dataset -> Wavelength Nm
  midWave d = d.wavelengthMin + d.wavelengthMax / 2

  lineTag s = tag "pre" (dataTag . bg SecondaryLight) $ text $ cs $ show s

  midTag mid =
    tag "pre" (pad 2 . color GrayDark) $ text $ cs (showFFloat (Just 0) mid "nm")

  dataTag :: Mod
  dataTag = pad (XY 6 2) . rounded 3

viewInstrumentProgram :: InstrumentProgram -> View IPRow ()
viewInstrumentProgram ip = el (transition Height 0.5 . height 200) $ do
  col (height 100) $ do
    rowInstrumentProgram Collapse ip
    el_ $ text $ cs $ show ip.instrumentProgramId

-- liveButton Collapse (bg Primary . color White) "Collapse"

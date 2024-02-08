module App.Page.Program where

import App.Colors
import App.Route
import App.Style qualified as Style
import App.View.Common as View
import App.View.DatasetsTable as DatasetsTable
import App.View.ExperimentDetails
import App.View.Icons as Icons
import Data.Grouped as G
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Effectful.Dispatch.Dynamic
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs
import NSO.Prelude
import Web.Hyperbole


-- import Web.View.Style

page
  :: (Hyperbole :> es, Time :> es, Datasets :> es, Inversions :> es)
  => Id InstrumentProgram
  -> Page es ()
page ip = do
  hyper inversions
  hyper DatasetsTable.actionSort

  load $ do
    ds' <- send $ Datasets.Query (Datasets.ByProgram ip)
    ds <- expectFound ds'
    let d = head ds

    dse <- send $ Datasets.Query (ByExperiment d.primaryExperimentId)
    is <- send $ Inversions.ByProgram ip
    now <- currentTime

    pure $ appLayout Experiments $ do
      col (Style.page . gap 30) $ do
        col (gap 5) $ do
          el Style.header $ do
            text "Instrument Program: "
            text ip.fromId

          experimentLink d (numOtherIps dse)

        -- viewExperimentDescription d.experimentDescription

        viewId (InversionStatus ip) $ viewInversions is

        col Style.card $ do
          el (Style.cardHeader Secondary) "Instrument Program Details"
          col (gap 15 . pad 15) $ do
            viewDatasets now (NE.filter (.latest) ds) is
            viewId (ProgramDatasets ip) $ DatasetsTable.datasetsTable ByLatest (NE.toList ds)
 where
  instrumentProgramIds :: [Dataset] -> [Id InstrumentProgram]
  instrumentProgramIds ds = nub $ map (\d -> d.instrumentProgramId) ds

  expectFound :: (Hyperbole :> es) => [a] -> Eff es (NonEmpty a)
  expectFound [] = notFound
  expectFound (a : as) = pure $ a :| as

  numOtherIps :: [Dataset] -> Int
  numOtherIps dse = length (instrumentProgramIds dse) - 1

  experimentLink :: Dataset -> Int -> View c ()
  experimentLink d n = do
    el_ $ do
      text "Experiment: "
      link (Experiment d.primaryExperimentId) Style.link $ do
        text d.primaryExperimentId.fromId
      text $
        if n > 0
          then [i|(#{n} other Instrument Programs)|]
          else ""


viewDatasets :: UTCTime -> [Dataset] -> [Inversion] -> View c ()
viewDatasets _ [] _ = none
viewDatasets now (d : ds) is = do
  let gd = Grouped (d :| ds)
  let ip = instrumentProgram gd is

  row (textAlign Center) $ do
    viewProgramRow now ip

  View.hr (color Gray)

  viewCriteria ip gd


-- viewProvenanceEntry :: ProvenanceEntry -> View c ()
-- viewProvenanceEntry (WasInverted p) = do
--   row (gap 10) $ do
--     el_ "Inverted"
--     text $ showTimestamp p.completed
-- viewProvenanceEntry (WasQueued p) = do
--   row (gap 10) $ do
--     el_ "Queued"
--     text $ showTimestamp p.completed

-- Status -----------------------------------------------

newtype InversionStatus = InversionStatus (Id InstrumentProgram)
  deriving newtype (Show, Read, Param)


data InversionsAction
  = CreateInversion
  | Download (Id Inversion)
  | Cancel (Id Inversion)
  deriving (Show, Read, Param)


instance HyperView InversionStatus where
  type Action InversionStatus = InversionsAction


inversions :: (Hyperbole :> es, Inversions :> es) => InversionStatus -> InversionsAction -> Eff es (View InversionStatus ())
inversions (InversionStatus ip) = \case
  CreateInversion -> do
    inv <- send $ Inversions.Create ip
    pure $ viewInversion inv
  Download iid -> do
    send $ Inversions.SetDownloaded iid
    pure $ el_ "DOWNLOADING"
  Cancel iid -> do
    send $ Inversions.Remove iid
    pure $ el_ "CANCEL"


viewInversions :: [Inversion] -> View InversionStatus ()
viewInversions [] = do
  button CreateInversion (Style.btn Primary) "Create Inversion"
viewInversions is =
  col (gap 20) $ do
    mapM_ viewInversion is


viewInversion :: Inversion -> View InversionStatus ()
viewInversion inv = do
  col (Style.card . gap 15) $ do
    el (Style.cardHeader Info) "Inversion"
    col (gap 15 . pad 15) $ do
      invProgress
      viewStep
 where
  invProgress :: View InversionStatus ()
  invProgress = do
    row (gap 10) $ do
      step Success "DOWNLOAD" Icons.check
      line Success grow
      step Info "INVERSION" "2"
      line Gray grow
      step Gray "POST PROCESS" "3"
      line Gray grow
      step Gray "PUBLISH" "4"

  circle = rounded 50 . pad 5 . color White . textAlign Center . width 34 . height 34

  step clr t icon = col (color clr . gap 4) $ do
    row id $ do
      space
      el (circle . bg clr) icon
      space
    el (fontSize 12 . textAlign Center) (text t)

  line c f = col f $ do
    el (border (TRBL 0 0 2 0) . height 20 . borderColor c) ""

  viewStep :: View InversionStatus ()
  viewStep =
    case inv.step of
      StepStarted _ -> stepDownload
      StepDownloaded _ -> stepCalibrate
      StepCalibrated _ -> stepInvert
      StepInverted _ -> stepProcess
      StepProcessed _ -> stepPublish
      StepPublished _ -> stepDone

  stepDownload = do
    -- click that download button!
    el_ "Download"
    row (gap 10) $ do
      button (Download inv.inversionId) (Style.btn Primary . grow) "Download"
      button (Cancel inv.inversionId) (Style.btnOutline Secondary) $ do
        -- el (width 24) Icons.xCircle
        "Cancel"

  stepCalibrate = do
    el_ "Calibrate"

  stepInvert = do
    el_ "Invert"

  stepProcess = do
    el_ "Process"

  stepPublish = do
    el_ "Publish"

  stepDone = do
    el_ "Done"

-- moveDown =
--   addClass
--     $ cls "move-down"
--     & prop @Text "position" "relative"
--     & prop @Text "top" "50px"

-- statusView :: View Status ()
-- statusView = do
--   row (gap 10) $ do
--     button Queue btn "Queue"
--     button Complete btn "Complete"
--  where
--   btn = color White . pad (XY 15 10) . bg Secondary . hover (bg SecondaryLight)

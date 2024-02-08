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

-- ----------------------------------------------------------------
-- INVERSION STATUS -----------------------------------------------
-- ----------------------------------------------------------------

newtype InversionStatus = InversionStatus (Id InstrumentProgram)
  deriving newtype (Show, Read, Param)


data InversionsAction
  = CreateInversion
  | Update (Id Inversion) InversionAction
  deriving (Show, Read, Param)


data InversionAction
  = Download
  | Cancel
  | Calibrate
  | Invert
  | PostProcess
  | Publish
  deriving (Show, Read, Param)


data CalibrationForm a = CalibrationForm
  { calibrationUrl :: Field a Text
  }
  deriving (Generic, Form)


data InversionForm a = InversionForm
  { inversionSoftware :: Field a Text
  }
  deriving (Generic, Form)


instance HyperView InversionStatus where
  type Action InversionStatus = InversionsAction


inversions :: (Hyperbole :> es, Inversions :> es) => InversionStatus -> InversionsAction -> Eff es (View InversionStatus ())
inversions (InversionStatus ip) = \case
  CreateInversion -> do
    inv <- send $ Inversions.Create ip
    pure $ viewInversion inv
  (Update iid act) ->
    case act of
      Cancel -> do
        send $ Inversions.Remove iid
        pure none
      Download -> do
        send $ Inversions.SetDownloaded iid
        refresh iid
      Calibrate -> do
        f <- parseForm @CalibrationForm
        send $ Inversions.SetCalibrated iid f.calibrationUrl
        refresh iid
      Invert -> do
        f <- parseForm @InversionForm
        send $ Inversions.SetInverted iid (InversionSoftware f.inversionSoftware)
        refresh iid
      PostProcess -> do
        send $ Inversions.SetPostProcessed iid
        refresh iid
      Publish -> do
        send $ Inversions.SetPublished iid
        refresh iid
 where
  refresh iid = do
    inv <- send $ Inversions.ById iid
    pure $ mapM_ viewInversion inv


viewInversions :: [Inversion] -> View InversionStatus ()
viewInversions [] = do
  button CreateInversion (Style.btn Primary) "Create Inversion"
viewInversions is =
  col (gap 20) $ do
    mapM_ viewInversion is


viewInversion :: Inversion -> View InversionStatus ()
viewInversion inv = do
  let curr = currentStep inv.step
  col (Style.card . gap 15) $ do
    el (Style.cardHeader (headerColor curr)) "Inversion"
    col (gap 15 . pad 15) $ do
      invProgress curr
      viewStep curr
 where
  viewStep :: CurrentStep -> View InversionStatus ()
  viewStep Downloading = stepDownload
  viewStep Calibrating = stepCalibrate
  viewStep Inverting = stepInvert
  viewStep Processing = stepProcess
  viewStep Publishing = stepPublish
  viewStep Complete = stepDone

  headerColor Complete = Success
  headerColor _ = Info

  stepDownload = do
    -- click that download button!
    row (gap 10) $ do
      button (Update inv.inversionId Download) (Style.btn Primary . grow) "Download"
      button (Update inv.inversionId Cancel) (Style.btnOutline Secondary) $ do
        "Cancel"

  stepCalibrate = do
    form @CalibrationForm (Update inv.inversionId Calibrate) (gap 10) $ \f -> do
      field id $ do
        label "Calibration URL"
        input TextInput Style.input f.calibrationUrl
      submit (Style.btn Primary . grow) "Save Calibration"

  stepInvert = do
    form @InversionForm (Update inv.inversionId Invert) (gap 10) $ \f -> do
      field id $ do
        label "Inversion Software"
        input TextInput Style.input f.inversionSoftware
      submit (Style.btn Primary . grow) "Save Inversion"

  stepProcess = do
    button (Update inv.inversionId PostProcess) (Style.btn Primary . grow) "Save Post Processing"

  stepPublish = do
    button (Update inv.inversionId Publish) (Style.btn Primary . grow) "Save Publish"

  stepDone = do
    el_ "Done"


data CurrentStep
  = Downloading
  | Calibrating
  | Inverting
  | Processing
  | Publishing
  | Complete
  deriving (Eq, Ord, Bounded)


currentStep :: InversionStep -> CurrentStep
currentStep = \case
  StepStarted _ -> Downloading
  StepDownloaded _ -> Calibrating
  StepCalibrated _ -> Inverting
  StepInverted _ -> Processing
  StepProcessed _ -> Publishing
  StepPublished _ -> Complete


invProgress :: CurrentStep -> View InversionStatus ()
invProgress curr = do
  row (gap 10) $ do
    stat Downloading "DOWNLOAD" "1"
    line Downloading
    stat Calibrating "CALIBRATE" "2"
    line Calibrating
    stat Inverting "INVERT" "3"
    line Inverting
    stat Processing "POST PROCESS" "4"
    line Processing
    stat Publishing "PUBLISH" "5"
 where
  stat s t icon = col (color statColor . gap 4) $ do
    row id $ do
      space
      el (circle . bg statColor) statIcon
      space
    el (fontSize 12 . textAlign Center) (text t)
   where
    statIcon
      | s < curr = Icons.check
      | otherwise = icon

    statColor
      | s == curr = Info
      | s < curr = Success
      | otherwise = Gray

  line s = col grow $ do
    el (border (TRBL 0 0 2 0) . height 20 . borderColor lineColor) ""
   where
    lineColor
      | s == curr = Gray
      | s < curr = Success
      | otherwise = Gray

  circle = rounded 50 . pad 5 . color White . textAlign Center . width 34 . height 34

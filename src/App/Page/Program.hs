module App.Page.Program where

import App.Colors
import App.Error (expectFound)
import App.Globus as Globus
import App.Route
import App.Style qualified as Style
import App.Types (App (..))
import App.View.Common as View
import App.View.DatasetsTable as DatasetsTable
import App.View.ExperimentDetails
import App.View.Icons as Icons
import App.View.Layout
import Data.Diverse.Many
import Data.Grouped as G
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs
import NSO.Prelude
import Web.Hyperbole


page
  :: (Hyperbole :> es, Time :> es, Datasets :> es, Inversions :> es, Layout :> es, Reader App :> es)
  => Id InstrumentProgram
  -> Page es Response
page ip = do
  hyper inversions
  hyper DatasetsTable.actionSort

  load $ do
    ds' <- send $ Datasets.Query (Datasets.ByProgram ip)
    ds <- expectFound ds'
    let d = head ds

    dse <- send $ Datasets.Query (ByExperiment d.primaryExperimentId)
    invs <- send $ Inversions.ByProgram ip
    steps <- mapM (currentStep . (.step)) invs
    now <- currentTime

    appLayout Experiments $ do
      col (Style.page . gap 30) $ do
        col (gap 5) $ do
          el Style.header $ do
            text "Instrument Program: "
            text ip.fromId

          experimentLink d (numOtherIps dse)

        -- viewExperimentDescription d.experimentDescription

        viewId (InversionStatus ip) $ viewInversions invs steps

        col Style.card $ do
          el (Style.cardHeader Secondary) "Instrument Program Details"
          col (gap 15 . pad 15) $ do
            viewDatasets now (NE.filter (.latest) ds) invs
            viewId (ProgramDatasets ip) $ DatasetsTable.datasetsTable ByLatest (NE.toList ds)
 where
  instrumentProgramIds :: [Dataset] -> [Id InstrumentProgram]
  instrumentProgramIds ds = nub $ map (\d -> d.instrumentProgramId) ds

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


-- ----------------------------------------------------------------
-- INVERSION STATUS -----------------------------------------------
-- ----------------------------------------------------------------

newtype InversionStatus = InversionStatus (Id InstrumentProgram)
  deriving newtype (Show, Read, Param)


data InversionsAction
  = CreateInversion
  | Update (Id Inversion) InversionAction
  | CheckTask (Id Inversion) (Id Task)
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
  { calibrationSoftware :: Field a Text
  }
  deriving (Generic, Form)


data InversionForm a = InversionForm
  { inversionSoftware :: Field a Text
  }
  deriving (Generic, Form)


instance HyperView InversionStatus where
  type Action InversionStatus = InversionsAction


inversions :: (Hyperbole :> es, Inversions :> es, Globus :> es) => InversionStatus -> InversionsAction -> Eff es (View InversionStatus ())
inversions (InversionStatus ip) = \case
  CreateInversion -> do
    inv <- send $ Inversions.Create ip
    step <- currentStep inv.step
    pure $ viewInversion inv step
  (Update iid act) ->
    case act of
      Cancel -> do
        send $ Inversions.Remove iid
        pure $ viewInversions [] []
      Download -> do
        r <- request
        redirect $ Globus.fileManagerUrl iid ip r
      Calibrate -> do
        f <- parseForm @CalibrationForm
        send $ Inversions.SetCalibrated iid (GitCommit f.calibrationSoftware)
        refresh iid
      Invert -> do
        f <- parseForm @InversionForm
        send $ Inversions.SetInverted iid (GitCommit f.inversionSoftware)
        refresh iid
      PostProcess -> do
        send $ Inversions.SetPostProcessed iid
        refresh iid
      Publish -> do
        send $ Inversions.SetPublished iid
        refresh iid
  CheckTask vi ti -> do
    t <- Globus.transferStatus ti
    case t.status of
      Succeeded -> do
        inv <- send (Inversions.ById vi) >>= expectFound
        pure $ viewInversionContainer Calibrating $ do
          stepCalibrate $ head inv
      _ -> pure $ viewInversionContainer (Downloading ti) $ do
        stepDownloadProgress t
 where
  refresh iid = do
    inv <- send $ Inversions.ById iid
    steps <- mapM (currentStep . (.step)) inv
    pure $ viewInversions inv steps


viewInversions :: [Inversion] -> [CurrentStep] -> View InversionStatus ()
viewInversions [] [] = do
  button CreateInversion (Style.btn Primary) "Create Inversion"
viewInversions is ss =
  col (gap 20) $ do
    zipWithM_ viewInversion is ss


viewInversionContainer :: CurrentStep -> View InversionStatus () -> View InversionStatus ()
viewInversionContainer step cnt =
  col (Style.card . gap 15) $ do
    el (Style.cardHeader (headerColor step)) "Inversion"
    col (gap 15 . pad 15) $ do
      invProgress step
      cnt
 where
  headerColor Complete = Success
  headerColor _ = Info


viewInversion :: Inversion -> CurrentStep -> View InversionStatus ()
viewInversion inv step = do
  viewInversionContainer step $ do
    viewStep step
 where
  viewStep :: CurrentStep -> View InversionStatus ()
  viewStep SelectingDownload = stepDownload
  viewStep (Downloading ti) = stepCheckDownload ti
  viewStep Calibrating = stepCalibrate inv
  viewStep Inverting = stepInvert
  viewStep Processing = stepProcess
  viewStep Publishing = stepPublish
  viewStep Complete = stepDone

  stepDownload = do
    el_ "You will be redirected to Globus. Please select a destination for this instrument program"
    row (gap 10) $ do
      button (Update inv.inversionId Download) (Style.btn Primary . grow) "Choose Download Location"
      button (Update inv.inversionId Cancel) (Style.btnOutline Secondary) $ do
        "Cancel"

  stepCheckDownload ti = do
    onLoad (CheckTask inv.inversionId ti) $ do
      row (pad 20) $ do
        space
        el (width 200 . color (light Primary)) Icons.spinner
        space
  -- el_ $ text $ cs $ show $ taskPercentComplete t

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


stepDownloadProgress :: Task -> View InversionStatus ()
stepDownloadProgress t =
  el_ $ text $ cs $ show $ taskPercentComplete t


stepCalibrate :: Inversion -> View InversionStatus ()
stepCalibrate inv = do
  form @CalibrationForm (Update inv.inversionId Calibrate) (gap 10) $ \f -> do
    field id $ do
      label "Calibration URL"
      input TextInput Style.input f.calibrationSoftware
    submit (Style.btn Primary . grow) "Save Calibration"


data CurrentStep
  = SelectingDownload
  | Downloading (Id Task)
  | Calibrating
  | Inverting
  | Processing
  | Publishing
  | Complete
  deriving (Eq, Ord, Show)


newtype CurrentTask = CurrentTask Task
  deriving (Eq)


instance Ord CurrentTask where
  compare _ _ = EQ


-- TODO: Just show the step, and then update on a poll
-- TODO: put them in an instrument program sub-folder? Otherwise it's hard to know what is what...
currentStep :: (Hyperbole :> es, Globus :> es) => InversionStep -> Eff es CurrentStep
currentStep = \case
  StepCreated _ -> pure SelectingDownload
  StepDownloaded dwn -> do
    let d = grab @Downloaded dwn :: Downloaded
    pure $ Downloading (Id d.taskId)
  StepCalibrated _ -> pure Inverting
  StepInverted _ -> pure Processing
  StepProcessed _ -> pure Publishing
  StepPublished _ -> pure Complete


invProgress :: CurrentStep -> View InversionStatus ()
invProgress curr = do
  row (gap 10) $ do
    stat SelectingDownload "DOWNLOAD" "1"
    line SelectingDownload
    stat Calibrating "CALIBRATE" "2"
    line Calibrating
    stat Inverting "INVERT" "3"
    line Inverting
    stat Processing "POST PROCESS" "4"
    line Processing
    stat Publishing "PUBLISH" "5"
 where
  stat :: CurrentStep -> Text -> View InversionStatus () -> View InversionStatus ()
  stat s t icon = col (color (statColor curr) . gap 4) $ do
    row id $ do
      space
      el (circle . bg (statColor curr)) (statIcon curr)
      space
    el (fontSize 12 . textAlign Center) (text t)
   where
    statIcon (Downloading _)
      | s == SelectingDownload = icon
      | otherwise = statIcon'
    statIcon _ = statIcon'

    statIcon'
      | s < curr = Icons.check
      | otherwise = icon

    statColor (Downloading _)
      | s == SelectingDownload = Info
      | otherwise = statColor'
    statColor _ = statColor'

    statColor'
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

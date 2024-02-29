module App.Page.Inversion where

import App.Colors
import App.Error (expectFound)
import App.Globus as Globus
import App.Route
import App.Style qualified as Style
import App.View.Icons qualified as Icons
import App.View.Layout
import Data.Diverse.Many
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import Web.Hyperbole


page :: (Hyperbole :> es, Inversions :> es, Layout :> es) => Id Inversion -> Page es Response
page i = do
  hyper $ inversions redirectHome
  load $ do
    (inv :| _) <- send (Inversions.ById i) >>= expectFound
    step <- currentStep inv.step
    appLayout Inversions $ do
      col Style.page $ do
        col (gap 5) $ do
          el Style.header $ do
            text "Inversion: "
            text i.fromId

          el_ $ do
            text "Program: "
            link (Program inv.programId) Style.link $ do
              text inv.programId.fromId

        viewId (InversionStatus inv.programId) $ viewInversion inv step


redirectHome :: (Hyperbole :> es) => Eff es (View InversionStatus ())
redirectHome = do
  redirect $ pathUrl . routePath $ Inversions


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


inversions :: (Hyperbole :> es, Inversions :> es, Globus :> es) => Eff es (View InversionStatus ()) -> InversionStatus -> InversionsAction -> Eff es (View InversionStatus ())
inversions onCancel (InversionStatus ip) = \case
  CreateInversion -> do
    inv <- send $ Inversions.Create ip
    step <- currentStep inv.step
    pure $ viewInversion inv step
  (Update iid act) ->
    case act of
      Cancel -> do
        send $ Inversions.Remove iid
        -- then redirect to home.. but that won't refresh!
        onCancel
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
    (inv :| _) <- send (Inversions.ById iid) >>= expectFound
    step <- currentStep inv.step
    pure $ viewInversion inv step


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


currentStep :: (Globus :> es) => InversionStep -> Eff es CurrentStep
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

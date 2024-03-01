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
import Numeric (showFFloat)
import Web.Hyperbole
import Web.View qualified as WebView


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
  | CalibrateValid
  | Calibrate GitCommit
  | InvertValid
  | Invert GitCommit
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
      CalibrateValid -> do
        inv <- send (Inversions.ById iid) >>= expectFound
        f <- parseForm @CalibrationForm
        let commit = GitCommit f.calibrationSoftware
        pure $ loading (Update iid (Calibrate commit)) Calibrating $ stepCalibrate (head inv)
      Calibrate commit -> do
        valid <- send $ ValidateCalibrationCommit commit
        validate iid Calibrating stepCalibrate valid
        send $ Inversions.SetCalibrated iid commit
        refresh iid
      InvertValid -> do
        inv <- send (Inversions.ById iid) >>= expectFound
        f <- parseForm @InversionForm
        let commit = GitCommit f.inversionSoftware
        pure $ loading (Update iid (Invert commit)) Inverting $ stepInvert (head inv)
      Invert commit -> do
        valid <- send $ ValidateDesireCommit commit
        validate iid Inverting stepInvert valid
        send $ Inversions.SetInverted iid commit
        refresh iid
      PostProcess -> do
        send $ Inversions.SetPostProcessed iid
        refresh iid
      Publish -> do
        send $ Inversions.SetPublished iid
        refresh iid
  CheckTask vi ti -> do
    t <- Globus.transferStatus ti
    checkTask vi ti t
 where
  refresh iid = do
    (inv :| _) <- send (Inversions.ById iid) >>= expectFound
    step <- currentStep inv.step
    pure $ viewInversion inv step

  validate :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> CurrentStep -> (Inversion -> View InversionStatus ()) -> Bool -> Eff es ()
  validate _ _ _ True = pure ()
  validate iid step viewStep False = do
    inv <- send (Inversions.ById iid) >>= expectFound
    respondEarly (InversionStatus ip) $ do
      viewInversionContainer step $ do
        viewStep $ head inv
        validationError "Not a valid Git Commit"

  loading :: InversionsAction -> CurrentStep -> View InversionStatus () -> View InversionStatus ()
  loading act step cnt =
    viewInversionContainer step $ onLoad act $ do
      el disabled cnt

  disabled = opacity 0.5 . att "inert" ""


checkTask :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> Id Task -> Task -> Eff es (View InversionStatus ())
checkTask vi ti t' = do
  case t'.status of
    Succeeded -> do
      inv <- send (Inversions.ById vi) >>= expectFound
      pure $ viewInversionContainer Calibrating $ do
        stepCalibrate $ head inv
    Failed -> pure $ do
      viewInversionContainer (Downloading ti) $ do
        stepDownloadFailed t'
    _ -> pure $ do
      viewInversionContainer (Downloading ti) $ do
        stepDownloadProgress t'
 where
  stepDownloadProgress :: Task -> View InversionStatus ()
  stepDownloadProgress t = do
    -- el_ $ text $ cs $ show $ taskPercentComplete t
    row id $ do
      el_ $ text $ "Downloading... (" <> cs rate <> " Mb/s)"
      space
      activityLink t
    progress (taskPercentComplete t)
   where
    rate :: String
    rate = showFFloat (Just 2) (fromIntegral t.effective_bytes_per_second / (1000 * 1000) :: Float) ""

  stepDownloadFailed :: Task -> View InversionStatus ()
  stepDownloadFailed t =
    row id $ do
      el_ "Download Failed"
      space
      activityLink t

  activityLink :: Task -> View c ()
  activityLink t =
    WebView.link activityUrl Style.link "View Transfer on Globus"
   where
    activityUrl = Url $ "https://app.globus.org/activity/" <> t.task_id.unTagged

  progress :: Float -> View c ()
  progress p = do
    row (bg Gray . height 20) $ do
      el (width (Pct p) . bg (light Info)) $ do
        space


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
  viewStep Inverting = stepInvert inv
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
    -- don't show anything until load
    onLoad (CheckTask inv.inversionId ti) $ do
      el (height 60) ""

  stepProcess = do
    button (Update inv.inversionId PostProcess) (Style.btn Primary . grow) "Save Post Processing"

  stepPublish = do
    button (Update inv.inversionId Publish) (Style.btn Primary . grow) "Save Publish"

  stepDone = do
    el_ "Done"


stepCalibrate :: Inversion -> View InversionStatus ()
stepCalibrate inv = do
  form @CalibrationForm (Update inv.inversionId CalibrateValid) (gap 10) $ \f -> do
    field id $ do
      label "Proprocessing / Calibration Git Commit"
      input TextInput Style.input f.calibrationSoftware
    submit (Style.btn Primary . grow) "Save Calibration"


stepInvert :: Inversion -> View InversionStatus ()
stepInvert inv = do
  form @InversionForm (Update inv.inversionId InvertValid) (gap 10) $ \f -> do
    field id $ do
      label "DeSIRe Git Commit"
      input TextInput Style.input f.inversionSoftware
    -- TODO: upload files
    submit (Style.btn Primary . grow) "Save Inversion"


validationError :: Text -> View c ()
validationError msg = do
  el (border 1 . borderColor (dark Danger) . bg (light Danger) . pad 10 . rounded 4) $ do
    el (color White) $ text msg


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

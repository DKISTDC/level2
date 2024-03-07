module App.Page.Inversion where

import App.Colors
import App.Error (expectFound)
import App.Globus as Globus
import App.Route as Route
import App.Style qualified as Style
import App.View.Icons qualified as Icons
import App.View.Inversions
import App.View.Layout
import Data.Diverse.Many
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole


page :: (Hyperbole :> es, Inversions :> es, Datasets :> es, Layout :> es) => Id Inversion -> InversionRoute -> Page es Response
page i Inv = pageMain i
page i SubmitDownload = pageSubmitDownload i


pageMain :: (Hyperbole :> es, Inversions :> es, Layout :> es) => Id Inversion -> Page es Response
pageMain i = do
  hyper $ inversions redirectHome
  load $ do
    -- TODO: handle taskId query param, and pass forward!
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


pageSubmitDownload :: (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es) => Id Inversion -> Page es Response
pageSubmitDownload iv = do
  load $ do
    t <- parseTransferForm
    (inv :| _) <- send (Inversions.ById iv) >>= expectFound
    ds <- send $ Datasets.Query $ Datasets.ByProgram inv.programId
    it <- Globus.initDownload t ds

    -- don't mark as downloading... there is no downloading step
    -- send $ Inversions.SetDownloading iv it

    redirect $ addTaskId it $ routeUrl (Route.Inversion iv Inv)
 where
  setQuery query Url{path, scheme, domain} = Url{path, scheme, domain, query}
  addTaskId it = setQuery [("taskId", Just (cs it.fromId))]


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
  | CheckDownload (Id Inversion) (Id Task)
  | CheckUpload (Id Inversion) (Id Task)
  deriving (Show, Read, Param)


data InversionAction
  = Download
  | Cancel
  | CalibrateValid
  | Calibrate GitCommit
  | Upload
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
        redirect $ Globus.fileManagerUrl (Route.Inversion iid SubmitDownload) ("Transfer Instrument Program " <> ip.fromId) r
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
      Upload -> do
        pure $ el_ "Upload"
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
  CheckDownload vi ti -> do
    -- t <- Globus.transferStatus ti
    -- checkDownload vi ti t
    pure $ el_ "TODO"
  CheckUpload vi ti -> do
    -- t <- Globus.transferStatus ti
    -- checkUpload vi ti t
    pure $ el_ "TODO"
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
    viewInversionContainer step $ onLoad act 0 $ do
      el disabled cnt


-- checkDownload :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> Id Task -> Task -> Eff es (View InversionStatus ())
-- checkDownload vi ti t' = do
--   case t'.status of
--     Succeeded -> do
--       inv <- send (Inversions.ById vi) >>= expectFound
--       -- here is where we set that we have finished downloading
--       send (Inversions.SetDownloaded vi)
--       pure $ viewInversionContainer Calibrating $ do
--         stepCalibrate $ head inv
--     Failed -> pure $ do
--       viewInversionContainer (Downloading (Transferring ti)) $ do
--         viewTransferFailed t'
--     _ -> pure $ do
--       viewInversionContainer (Downloading (Transferring ti)) $ do
--         onLoad (CheckDownload vi ti) 5000 $ do
--           viewTransferProgress t'
--
--
-- checkUpload :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> Id Task -> Task -> Eff es (View InversionStatus ())
-- checkUpload vi ti t = do
--   case t.status of
--     Succeeded -> do
--       inv <- send (Inversions.ById vi) >>= expectFound
--       send (Inversions.SetUploaded vi)
--       pure $ viewInversionContainer Calibrating $ do
--         stepCalibrate $ head inv
--     Failed -> pure $ do
--       viewInversionContainer (Uploading (Transferring ti)) $ do
--         viewTransferFailed t
--     _ -> pure $ do
--       viewInversionContainer (Uploading (Transferring ti)) $ do
--         onLoad (CheckDownload vi ti) 5000 $ do
--           viewTransferProgress t

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
  viewStep Downloading = viewStepDownload inv.inversionId Nothing
  -- viewStep Downloading = stepCheckDownload ti
  viewStep Calibrating = stepCalibrate inv
  viewStep Inverting = stepInvert inv
  -- viewStep (Uploading (Transferring ti)) = stepCheckUpload ti
  viewStep Uploading = stepUpload
  viewStep Processing = stepProcess
  viewStep Publishing = stepPublish
  viewStep Complete = stepDone

  stepProcess = do
    button (Update inv.inversionId PostProcess) (Style.btn Primary . grow) "Save Post Processing"

  stepPublish = do
    button (Update inv.inversionId Publish) (Style.btn Primary . grow) "Save Publish"

  stepDone = do
    el_ "Done"

  stepUpload = do
    el bold "Upload Inversion Output"
    el id $ text $ cs $ show inv.step
    el_ "You will be redirected to Globus. Please select a source folder containing the FITS output of the inversion"

    row (gap 10) $ do
      button (Update inv.inversionId Upload) (Style.btn Primary . grow) "Choose Upload Source Location"


-- stepCheckUpload ti = do
--   -- don't show anything until load
--   onLoad (CheckDownload inv.inversionId ti) 0 $ do
--     el (height 60) ""

viewStepDownload :: Id Inversion -> Maybe (Id Task) -> View InversionStatus ()
viewStepDownload ii Nothing = do
  el_ "You will be redirected to Globus. Please select a destination for this instrument program"

  row (gap 10) $ do
    button (Update ii Download) (Style.btn Primary) "Choose Download Location"

  row (gap 10) $ do
    button (Update ii Cancel) (Style.btnOutline Secondary) "Cancel"
    el (grow . disabled . Style.btnOutline Secondary) "Submit Download"
viewStepDownload ii (Just it) = do
  -- don't show anything until load
  onLoad (CheckDownload ii it) 0 $ do
    el (height 60) ""


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

    el_ "You will be redirected to Globus. Please select a destination for this instrument program"

    -- 1. Choose the download location
    -- 2. Start the upload?
    -- 3. Two separate steps?
    -- you can't confirm the entire step until all steps are complete
    submit (Style.btn Primary . grow) "Save Inversion"
    button (Update inv.inversionId Download) (Style.btn Primary . grow) "Choose Download Location"


validationError :: Text -> View c ()
validationError msg = do
  el (border 1 . borderColor (dark Danger) . bg Danger . pad 10) $ do
    el (color White) $ text msg


data CurrentStep
  = Downloading
  | Calibrating
  | Uploading
  | Inverting
  | Processing
  | Publishing
  | Complete
  deriving (Eq, Ord, Show)


-- data TransferStep
--   = Select
--   | Transferring (Id Task)
--   deriving (Eq, Ord, Show)

newtype CurrentTask = CurrentTask Task
  deriving (Eq)


currentStep :: (Globus :> es) => InversionStep -> Eff es CurrentStep
currentStep = \case
  StepCreated _ -> pure Downloading
  -- StepDownloading dwn -> do
  --   let t = grab @Transfer dwn :: Transfer
  --   pure $ Downloading $ Transferring (Id t.taskId)
  StepDownloaded _ -> pure Calibrating
  StepCalibrated _ -> pure Uploading
  StepUploaded _ -> pure Inverting
  -- StepUploading up -> do
  --   let t = grab @Transfer up :: Transfer
  --   pure $ Uploading $ Transferring (Id t.taskId)
  StepInverted _ -> pure Processing
  StepProcessed _ -> pure Publishing
  StepPublished _ -> pure Complete


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
  stat :: CurrentStep -> Text -> View InversionStatus () -> View InversionStatus ()
  stat s t icon = col (color (statColor curr) . gap 4) $ do
    row id $ do
      space
      el (circle . bg (statColor curr)) (statIcon curr)
      space
    el (fontSize 12 . textAlign Center) (text t)
   where
    -- statIcon (Downloading _)
    --   | s == SelectingDownload = icon
    --   | otherwise = statIcon'
    statIcon _ = statIcon'

    statIcon'
      | s < curr = Icons.check
      | otherwise = icon

    -- statColor (Downloading _)
    --   | s == SelectingDownload = Info
    --   | otherwise = statColor'
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


disabled :: Mod
disabled = opacity 0.5 . att "inert" ""

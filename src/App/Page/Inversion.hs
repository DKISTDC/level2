module App.Page.Inversion where

import App.Colors
import App.Effect.Scratch (Scratch)
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Inversions.InvForm (CommitAction (..), TransferAction (..))
import App.Page.Inversions.InvForm qualified as InvForm
import App.Route as Route
import App.Style qualified as Style
import App.View.Icons qualified as Icons
import App.View.Layout
import Data.Diverse.Many
import Data.Maybe (isJust)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Worker
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole
import Web.Hyperbole.Forms (formFields)


page :: (Hyperbole :> es, Inversions :> es, Datasets :> es, Auth :> es, Globus :> es, Scratch :> es, Worker GenTask :> es) => Id Inversion -> InversionRoute -> Page es Response
page i Inv = pageMain i
page i SubmitDownload = pageSubmitDownload i
page i SubmitUpload = pageSubmitUpload i


pageMain :: (Hyperbole :> es, Inversions :> es, Datasets :> es, Auth :> es, Globus :> es, Worker GenTask :> es) => Id Inversion -> Page es Response
pageMain i = do
  handle $ inversions redirectHome
  handle generateTransfer
  handle inversionCommit
  handle preprocessCommit
  handle downloadTransfer
  handle uploadTransfer
  load $ do
    inv <- loadInversion i
    step <- currentStep i inv.step
    appLayout Inversions $ do
      col Style.page $ do
        col (gap 5) $ do
          el Style.header $ do
            text "Inversion: "
            text i.fromId

          el_ $ do
            text "Program: "
            route (Program inv.programId) Style.link $ do
              text inv.programId.fromId

        hyper (InversionStatus inv.programId inv.inversionId) $ viewInversion inv step


pageSubmitUpload :: forall es. (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es, Scratch :> es) => Id Inversion -> Page es Response
pageSubmitUpload ii = do
  load $ do
    tfrm <- formFields @TransferForm
    tup <- formFields @(UploadFiles Filename)
    it <- Globus.initUpload tfrm tup ii
    send $ Inversions.SetUploading ii it

    redirect $ routeUrl (Route.Inversion ii Inv)


pageSubmitDownload :: (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es) => Id Inversion -> Page es Response
pageSubmitDownload ii = do
  load $ do
    tfrm <- formFields @TransferForm
    tfls <- formFields @DownloadFolder
    inv <- loadInversion ii
    ds <- send $ Datasets.Query $ Datasets.ByProgram inv.programId
    it <- requireLogin $ Globus.initDownload tfrm tfls ds
    send $ Inversions.SetDownloading ii it

    redirect $ routeUrl (Route.Inversion ii Inv)


loadInversion :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> Eff es Inversion
loadInversion ii = do
  (inv :| _) <- send (Inversions.ById ii) >>= expectFound
  pure inv


markInverted :: (Inversions :> es) => Id Inversion -> GitCommit -> Eff es ()
markInverted ii gc =
  send $ SetInversion ii gc


redirectHome :: (Hyperbole :> es) => Eff es (View InversionStatus ())
redirectHome = do
  redirect $ pathUrl . routePath $ Inversions


-- ----------------------------------------------------------------
-- INVERSION STATUS -----------------------------------------------
-- ----------------------------------------------------------------

data InversionStatus = InversionStatus (Id InstrumentProgram) (Id Inversion)
  deriving (Generic, ViewId)
instance HyperView InversionStatus where
  type Action InversionStatus = InversionAction


data InversionAction
  = Download
  | Cancel
  | Upload
  | PostProcess
  | Publish
  | Reload
  | RestartGen
  | GoStepInv
  deriving (Generic, ViewAction)


data PreprocessSoftware = PreprocessSoftware Text
  deriving (Generic, FormField)


data InversionSoftware = InversionSoftware Text
  deriving (Generic, FormField)


inversions :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Worker GenTask :> es) => Eff es (View InversionStatus ()) -> InversionStatus -> InversionAction -> Eff es (View InversionStatus ())
inversions onCancel (InversionStatus ip ii) = \case
  Cancel -> do
    send $ Inversions.Remove ii
    onCancel
  Download -> do
    r <- request
    requireLogin $ do
      redirect $ Globus.fileManagerUrl (Folders 1) (Route.Inversion ii SubmitDownload) ("Transfer Instrument Program " <> ip.fromId) r
  Upload -> do
    r <- request
    requireLogin $ do
      redirect $ Globus.fileManagerUrl (Files 3) (Route.Inversion ii SubmitUpload) ("Transfer Inversion Results " <> ii.fromId) r
  PostProcess -> do
    send $ Inversions.SetGenerated ii
    refresh
  Publish -> do
    send $ Inversions.SetPublished ii
    refresh
  Reload -> do
    refresh
  RestartGen -> do
    send $ Inversions.ResetGenerating ii
    refresh
  GoStepInv -> do
    inv <- loadInversion ii
    let mi = findInverted inv.step
    case mi of
      Nothing -> do
        step <- currentStep ii inv.step
        pure $ viewInversion inv step
      Just i ->
        pure $ viewInversion inv (Inverting (InvertStep (Just i.inversionSoftware) (Just i.uploadedTaskId)))
 where
  refresh = do
    inv <- loadInversion ii
    step <- currentStep ii inv.step
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
  col (gap 10) $ do
    viewInversionContainer step $ do
      viewStep step
 where
  -- button NewInversion Style.link "HELLO"

  viewStep :: CurrentStep -> View InversionStatus ()
  viewStep (Downloading ts) = stepDownload inv ts
  viewStep Preprocessing = stepPreprocess inv
  viewStep (Inverting is) = stepInvert is inv
  viewStep (Generating g) = stepGenerate inv g
  viewStep Publishing = stepPublish
  viewStep Complete = stepDone

  stepPublish = do
    button Publish (Style.btn Primary . grow) "Publish to Portal"

  stepDone = do
    el_ "Done"


-- ----------------------------------------------------------------
-- STEP DOWNLOAD
-- ----------------------------------------------------------------

data DownloadTransfer = DownloadTransfer (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Generic, ViewId)
instance HyperView DownloadTransfer where
  type Action DownloadTransfer = TransferAction


downloadTransfer :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es) => DownloadTransfer -> TransferAction -> Eff es (View DownloadTransfer ())
downloadTransfer (DownloadTransfer ip ii ti) = \case
  TaskFailed -> do
    inv <- loadInversion ii
    pure $ do
      col (gap 10) $ do
        InvForm.viewTransferFailed ti
        hyper (InversionStatus ip ii) $ stepDownload inv Select
  TaskSucceeded -> do
    -- need to get the datasets used???
    ds <- send $ Datasets.Query $ Datasets.ByProgram ip
    send (Inversions.SetDownloaded ii (map (.datasetId) ds))
    pure $ hyper (InversionStatus ip ii) $ onLoad Reload 0 none
  CheckTransfer -> InvForm.checkTransfer ti


stepDownload :: Inversion -> TransferStep -> View InversionStatus ()
stepDownload _ Select = do
  el_ "Please select a destination folder for the instrument program's datasets. You will be redirected to Globus."

  row (gap 10) $ do
    button Download (Style.btn Primary . grow) "Choose Folder"
    button Cancel (Style.btnOutline Secondary) "Cancel"
stepDownload inv (Transferring it) = do
  hyper (DownloadTransfer inv.programId inv.inversionId it) InvForm.viewLoadTransfer


-- ----------------------------------------------------------------
-- STEP PREPROCESS
-- ----------------------------------------------------------------

data PreprocessCommit = PreprocessCommit (Id InstrumentProgram) (Id Inversion)
  deriving (Generic, ViewId)
instance HyperView PreprocessCommit where
  type Action PreprocessCommit = CommitAction


preprocessCommit :: (Hyperbole :> es, Inversions :> es) => PreprocessCommit -> CommitAction -> Eff es (View PreprocessCommit ())
preprocessCommit (PreprocessCommit ip ii) = action
 where
  action LoadValid = InvForm.loadValid preprocessCommitLabel
  action (CheckCommitValid gc) = do
    _ <- InvForm.validate (PreprocessCommit ip ii) preprocessRepo gc preprocessCommitLabel $ do
      send $ SetPreprocessed ii gc
    -- We can reload the parent like this!
    pure $ target (InversionStatus ip ii) $ onLoad Reload 0 $ el_ "Loading.."


preprocessCommitLabel :: Text
preprocessCommitLabel = "Preprocess Git Commit"


stepPreprocess :: Inversion -> View InversionStatus ()
stepPreprocess inv = do
  hyper (PreprocessCommit inv.programId inv.inversionId) $ InvForm.commitForm Nothing NotInvalid preprocessCommitLabel


-- ----------------------------------------------------------------
-- STEP INVERT
-- ----------------------------------------------------------------

data InversionCommit = InversionCommit (Id Inversion)
  deriving (Generic, ViewId)
instance HyperView InversionCommit where
  type Action InversionCommit = CommitAction


inversionCommit :: (Hyperbole :> es, Inversions :> es, Globus :> es, Worker GenTask :> es) => InversionCommit -> CommitAction -> Eff es (View InversionCommit ())
inversionCommit (InversionCommit ii) = action
 where
  action LoadValid = InvForm.loadValid inversionCommitLabel
  action (CheckCommitValid gc) = do
    vg <- InvForm.validate (InversionCommit ii) desireRepo gc inversionCommitLabel $ do
      send $ SetInversion ii gc

    checkInvertReload ii $ InvForm.commitForm (Just gc) vg inversionCommitLabel


inversionCommitLabel :: Text
inversionCommitLabel = "DeSIRe Git Commit"


data UploadTransfer = UploadTransfer (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Generic, ViewId)
instance HyperView UploadTransfer where
  type Action UploadTransfer = TransferAction


uploadTransfer :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Worker GenTask :> es) => UploadTransfer -> TransferAction -> Eff es (View UploadTransfer ())
uploadTransfer (UploadTransfer ip ii ti) = \case
  TaskFailed -> do
    pure $ do
      col (gap 10) $ do
        InvForm.viewTransferFailed ti
        hyper (InversionStatus ip ii) $ uploadSelect (Invalid "Upload Task Failed")
  TaskSucceeded -> do
    send (Inversions.SetUploaded ii)
    checkInvertReload ii $ hyper (InversionStatus ip ii) $ uploadSelect Valid
  CheckTransfer -> InvForm.checkTransfer ti


checkInvertReload :: (Hyperbole :> es, Inversions :> es, Globus :> es, Worker GenTask :> es) => Id Inversion -> View a () -> Eff es (View a ())
checkInvertReload ii vw = do
  inv <- loadInversion ii
  curr <- currentStep ii inv.step
  pure $ case curr of
    Generating _ -> hyper (InversionStatus inv.programId inv.inversionId) $ onLoad Reload 0 none
    _ -> vw


stepInvert :: InvertStep -> Inversion -> View InversionStatus ()
stepInvert (InvertStep mc mt) inv = do
  hyper (InversionCommit inv.inversionId) $ InvForm.commitForm mc (InvForm.fromExistingCommit mc) "DeSIRe GIt Commit"
  viewUploadTransfer mt
 where
  viewUploadTransfer (Just it) = do
    hyper (UploadTransfer inv.programId inv.inversionId it) InvForm.viewLoadTransfer
  viewUploadTransfer Nothing = do
    uploadSelect NotInvalid


uploadSelect :: Validated (Id Task) -> View InversionStatus ()
uploadSelect val = do
  col (gap 5 . file val) $ do
    el bold "Upload Inversion Results"
    instructions val
    li id "inv_res_pre.fits"
    li id "per_ori.fits"
    li id "inv_res_mod.fits"
    case val of
      Valid -> button Upload (Style.btnOutline Success . grow) "Select New Files"
      _ -> button Upload (Style.btn Primary . grow) "Select Files"
 where
  file Valid = color Success
  file _ = color Black

  li = tag "li"

  instructions Valid = none
  instructions _ = el_ "Please select the following files for upload. You will be redirected to Globus"


-- ----------------------------------------------------------------
-- STEP GENERATE
-- ----------------------------------------------------------------

data GenerateTransfer = GenerateTransfer (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Generic, ViewId)
instance HyperView GenerateTransfer where
  type Action GenerateTransfer = TransferAction


generateTransfer :: (Worker GenTask :> es, Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es) => GenerateTransfer -> TransferAction -> Eff es (View GenerateTransfer ())
generateTransfer (GenerateTransfer ip ii ti) = \case
  TaskFailed -> do
    pure $ viewGenerateDownload $ do
      InvForm.viewTransferFailed ti
      target (InversionStatus ip ii) $ do
        button RestartGen (Style.btn Primary) "Restart Transfer"
  TaskSucceeded -> do
    -- reload immediately... it should be marked as uploaded and go somewhere else
    s <- send $ TaskGetStatus $ GenTask ii
    pure $ do
      target (InversionStatus ip ii) $
        viewGenerateWait s
  CheckTransfer -> do
    vw <- InvForm.checkTransfer ti
    pure $ viewGenerateDownload vw


stepGenerate :: Inversion -> GenerateStep -> View InversionStatus ()
stepGenerate inv = \case
  GenWaitStart -> do
    onLoad Reload 1000 $ do
      el bold "Generate"
      el_ "Waiting for job to start"
  GenTransfering taskId -> do
    hyper (GenerateTransfer inv.programId inv.inversionId taskId) $ do
      viewGenerateDownload InvForm.viewLoadTransfer
  GenConverting s -> do
    viewGenerateWait s
  GenConvError e -> do
    el bold "Generate Error!"
    el_ $ text $ cs $ show e
    button RestartGen (Style.btn Primary) "Restart Generation"
    button GoStepInv (Style.btnOutline Secondary) "Go Back to Inversion"


viewGenerateDownload :: View GenerateTransfer () -> View GenerateTransfer ()
viewGenerateDownload vw = col (gap 5) $ do
  el bold "Downloading L1 Files"
  vw


viewGenerateWait :: GenStatus -> View InversionStatus ()
viewGenerateWait s =
  onLoad Reload 1000 $ do
    col (gap 5) $ do
      el bold "Generate Frames"
      el_ $ text $ cs $ show s


data GenerateStep
  = GenWaitStart
  | GenTransfering (Id Task)
  | GenConverting GenStatus
  | GenConvError Text
  deriving (Eq, Show, Ord)


-- ----------------------------------------------------------------
-- CURRENT STEP / PROGRESS
-- ----------------------------------------------------------------

data CurrentStep
  = Downloading TransferStep
  | Preprocessing
  | Inverting InvertStep
  | Generating GenerateStep
  | Publishing
  | Complete
  deriving (Eq, Ord, Show)


data TransferStep
  = Select
  | Transferring (Id Task)
  deriving (Eq, Ord, Show)


data InvertStep = InvertStep (Maybe GitCommit) (Maybe (Id Task))
  deriving (Show, Eq, Ord)
instance Semigroup InvertStep where
  InvertStep gc1 ti1 <> InvertStep gc2 ti2 = InvertStep (gc1 <|> gc2) (ti1 <|> ti2)
instance Monoid InvertStep where
  mempty = InvertStep Nothing Nothing


newtype CurrentTask = CurrentTask Task
  deriving (Eq)


currentStep :: (Globus :> es, Worker GenTask :> es) => Id Inversion -> InversionStep -> Eff es CurrentStep
currentStep ii = \case
  StepCreated _ -> pure (Downloading Select)
  StepDownloading dwn -> do
    let t = grab @Transfer dwn :: Transfer
    pure $ Downloading $ Transferring t.taskId
  StepDownloaded _ -> pure Preprocessing
  StepPreprocessed _ -> pure (Inverting mempty)
  StepInverting inv -> do
    let i = grab @Invert inv
    pure $ Inverting $ InvertStep i.commit i.taskId
  StepInverted _ -> pure $ Generating GenWaitStart
  StepGenerated _ -> pure Publishing
  StepGenTransfer inv -> do
    let t = grab @GenTransfer inv
    pure $ Generating $ GenTransfering t.taskId
  StepGenerating inv -> do
    let g = grab @Generate inv
    case g.genError of
      Just e -> pure $ Generating $ GenConvError e
      Nothing -> do
        s <- send $ TaskGetStatus (GenTask ii)
        pure $ Generating (GenConverting s)
  StepPublished _ -> pure Complete


invProgress :: CurrentStep -> View InversionStatus ()
invProgress curr = do
  row (gap 10) $ do
    prgDown curr "1" "DOWNLOAD"
    prgStep Preprocessing "2" "PREPROCESS"
    prgInv curr "3" "INVERT"
    prgGen curr "4" "GENERATE"
    prgStep Publishing "5" "PUBLISH"
 where
  stat :: AppColor -> View InversionStatus () -> Text -> View InversionStatus ()
  stat clr icn lbl = col (color clr . gap 4) $ do
    row id $ do
      space
      el (circle . bg clr) icn
      space
    el (fontSize 12 . textAlign Center) (text lbl)

  prgDown (Downloading Select) icn lbl = do
    stat Info icn lbl
    line Gray
  prgDown (Downloading _) icn lbl = do
    stat Info icn lbl
    line Info
  prgDown _ _ lbl = do
    stat Success Icons.check lbl
    line Success

  prgInv (Inverting (InvertStep mc mt)) icn lbl = do
    stat Info icn lbl
    line $
      if isJust mc || isJust mt
        then Info
        else Gray
  prgInv _ icn lbl = do
    let clr = statColor (Inverting mempty)
    stat clr (statIcon (Inverting mempty) icn) lbl
    line clr

  prgGen (Generating _) icn lbl = do
    stat Info icn lbl
    line Info
  prgGen _ icn lbl = do
    let clr = statColor (Generating GenWaitStart)
    stat clr (statIcon (Generating GenWaitStart) icn) lbl
    line clr

  -- prgGen (Generating _) icn lbl = do
  --   stat Info icn lbl
  --   line Gray
  -- prgGen _ icn lbl = do
  --   let clr = statColor (Generating (Id mempty))
  --   stat clr (statIcon (Generating (Id mempty)) icn) lbl
  --   line clr

  prgStep s icn lbl = do
    stat (statColor s) (statIcon s icn) lbl
    line (lineColor s)

  statIcon s icon
    | s < curr = Icons.check
    | otherwise = icon

  statColor s
    | s == curr = Info
    | s < curr = Success
    | otherwise = Gray

  line clr = col grow $ do
    el (border (TRBL 0 0 2 0) . height 20 . borderColor clr) ""

  lineColor s
    | s == curr = Gray
    | s < curr = Success
    | otherwise = Gray

  circle = rounded 50 . pad 5 . color White . textAlign Center . width 34 . height 34

-- isInverting (Inverting _) = True
-- isInverting _ = False

module App.Page.Inversion where

import App.Colors
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Inversions.InvForm
import App.Route as Route
import App.Style qualified as Style
import App.View.Icons qualified as Icons
import App.View.Layout
import Data.Diverse.Many
import Data.Maybe (isJust)
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole


page :: (Hyperbole :> es, Inversions :> es, Datasets :> es, Auth :> es, Globus :> es) => Id Inversion -> InversionRoute -> Page es Response
page i Inv = pageMain i
page i SubmitDownload = pageSubmitDownload i
page i SubmitUpload = pageSubmitUpload i


pageMain :: (Hyperbole :> es, Inversions :> es, Auth :> es, Globus :> es) => Id Inversion -> Page es Response
pageMain i = do
  hyper $ inversions redirectHome
  hyper inversionCommit
  hyper preprocessCommit
  hyper downloadTransfer
  hyper uploadTransfer
  load $ do
    inv <- loadInversion i
    step <- currentStep inv.step
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

        viewId (InversionStatus inv.programId inv.inversionId) $ viewInversion inv step


pageSubmitUpload :: forall es. (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es) => Id Inversion -> Page es Response
pageSubmitUpload ii = do
  load $ do
    tfrm <- parseForm @TransferForm
    tup <- parseForm @UploadFiles
    it <- Globus.initUpload tfrm tup ii
    send $ Inversions.SetUploading ii it

    redirect $ routeUrl (Route.Inversion ii Inv)


pageSubmitDownload :: (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es) => Id Inversion -> Page es Response
pageSubmitDownload ii = do
  load $ do
    tfrm <- parseForm @TransferForm
    tfls <- parseForm @DownloadFolder
    inv <- loadInversion ii
    ds <- send $ Datasets.Query $ Datasets.ByProgram inv.programId
    it <- Globus.initDownload tfrm tfls ds
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
  deriving (Show, Read, Param)
instance HyperView InversionStatus where
  type Action InversionStatus = InversionAction


data InversionAction
  = Download
  | Cancel
  | Upload
  | PostProcess
  | Publish
  | Reload
  deriving (Show, Read, Param)


data PreprocessForm a = PreprocessForm
  { preprocessSoftware :: Field a Text
  }
  deriving (Generic, Form)


data InversionForm a = InversionForm
  { inversionSoftware :: Field a Text
  }
  deriving (Generic, Form)


inversions :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es) => Eff es (View InversionStatus ()) -> InversionStatus -> InversionAction -> Eff es (View InversionStatus ())
inversions onCancel (InversionStatus ip ii) = \case
  Cancel -> do
    send $ Inversions.Remove ii
    onCancel
  Download -> do
    r <- request
    requireLogin
    redirect $ Globus.fileManagerUrl (Folders 1) (Route.Inversion ii SubmitDownload) ("Transfer Instrument Program " <> ip.fromId) r
  Upload -> do
    r <- request
    requireLogin
    redirect $ Globus.fileManagerUrl (Files 3) (Route.Inversion ii SubmitUpload) ("Transfer Inversion Results " <> ii.fromId) r
  PostProcess -> do
    send $ Inversions.SetGenerated ii
    refresh
  Publish -> do
    send $ Inversions.SetPublished ii
    refresh
  Reload -> do
    refresh
 where
  refresh = do
    inv <- loadInversion ii
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
  viewStep (Downloading ts) = stepDownload inv ts
  viewStep Preprocessing = stepPreprocess inv
  viewStep (Inverting is) = stepInvert is inv
  viewStep Generating = stepGenerate
  viewStep Publishing = stepPublish
  viewStep Complete = stepDone

  stepGenerate = do
    button PostProcess (Style.btn Primary . grow) "Save FITS Generation"

  stepPublish = do
    button Publish (Style.btn Primary . grow) "Save Publish"

  stepDone = do
    el_ "Done"


-- ----------------------------------------------------------------
-- STEP DOWNLOAD
-- ----------------------------------------------------------------

data DownloadTransfer = DownloadTransfer (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Show, Read, Param)
instance HyperView DownloadTransfer where
  type Action DownloadTransfer = TransferAction


downloadTransfer :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es) => DownloadTransfer -> TransferAction -> Eff es (View DownloadTransfer ())
downloadTransfer (DownloadTransfer ip ii ti) = \case
  TaskFailed -> do
    inv <- loadInversion ii
    pure $ do
      col (gap 10) $ do
        viewTransferFailed ti
        viewId (InversionStatus ip ii) $ stepDownload inv Select
  TaskSucceeded -> do
    send (Inversions.SetDownloaded ii)
    pure $ viewId (InversionStatus ip ii) $ onLoad Reload 0 none
  CheckTransfer -> checkTransfer ti


stepDownload :: Inversion -> TransferStep -> View InversionStatus ()
stepDownload _ Select = do
  el_ "Please select a destination folder for the instrument program's datasets. You will be redirected to Globus."

  row (gap 10) $ do
    button Download (Style.btn Primary . grow) "Choose Folder"
    button Cancel (Style.btnOutline Secondary) "Cancel"
stepDownload inv (Transferring it) = do
  viewId (DownloadTransfer inv.programId inv.inversionId it) viewLoadTransfer


-- ----------------------------------------------------------------
-- STEP PREPROCESS
-- ----------------------------------------------------------------

data PreprocessCommit = PreprocessCommit (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, Param)
instance HyperView PreprocessCommit where
  type Action PreprocessCommit = CommitAction


preprocessCommit :: (Hyperbole :> es, Inversions :> es) => PreprocessCommit -> CommitAction -> Eff es (View PreprocessCommit ())
preprocessCommit (PreprocessCommit ip ii) = action
 where
  action LoadValid = loadValid preprocessCommitLabel
  action (CheckCommitValid gc) = do
    _ <- validate (PreprocessCommit ip ii) preprocessRepo gc preprocessCommitLabel $ do
      send $ SetPreprocessed ii gc
    -- We can reload the parent like this!
    pure $ target (InversionStatus ip ii) $ onLoad Reload 0 $ el_ "Loading.."


preprocessCommitLabel :: Text
preprocessCommitLabel = "Preprocess Git Commit"


stepPreprocess :: Inversion -> View InversionStatus ()
stepPreprocess inv = do
  viewId (PreprocessCommit inv.programId inv.inversionId) $ commitForm Empty preprocessCommitLabel


-- ----------------------------------------------------------------
-- STEP INVERT
-- ----------------------------------------------------------------

data InversionCommit = InversionCommit (Id Inversion)
  deriving (Show, Read, Param)
instance HyperView InversionCommit where
  type Action InversionCommit = CommitAction


inversionCommit :: (Hyperbole :> es, Inversions :> es) => InversionCommit -> CommitAction -> Eff es (View InversionCommit ())
inversionCommit (InversionCommit ii) = action
 where
  action LoadValid = loadValid inversionCommitLabel
  action (CheckCommitValid gc) = do
    vg <- validate (InversionCommit ii) desireRepo gc inversionCommitLabel $ do
      send $ SetInversion ii gc
    pure $ commitForm vg inversionCommitLabel


inversionCommitLabel :: Text
inversionCommitLabel = "DeSIRe Git Commit"


data UploadTransfer = UploadTransfer (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Show, Read, Param)
instance HyperView UploadTransfer where
  type Action UploadTransfer = TransferAction


uploadTransfer :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es) => UploadTransfer -> TransferAction -> Eff es (View UploadTransfer ())
uploadTransfer (UploadTransfer ip ii ti) = \case
  TaskFailed -> do
    inv <- loadInversion ii
    pure $ do
      col (gap 10) $ do
        viewTransferFailed ti
        viewId (InversionStatus ip ii) $ stepDownload inv Select
  TaskSucceeded -> do
    send (Inversions.SetUploaded ii)
    pure $ viewId (InversionStatus ip ii) $ onLoad Reload 0 none
  CheckTransfer -> checkTransfer ti


stepInvert :: InvertStep -> Inversion -> View InversionStatus ()
stepInvert (InvertStep mc mt) inv = do
  viewId (InversionCommit inv.inversionId) $ commitForm (fromExistingCommit mc) "DeSIRe GIt Commit"

  maybe selectUpload viewUploadTransfer mt
 where
  selectUpload = do
    col (gap 5) $ do
      el bold "Upload Inversion Results"
      el_ "Please select the following files for upload. You will be redirected to Globus"
      tag "li" id "inv_res_pre.fits"
      tag "li" id "per_ori.fits"
      tag "li" id "inv_res_mod.fits"
    button Upload (Style.btn Primary . grow) "Select Files"

  viewUploadTransfer it = do
    viewId (UploadTransfer inv.programId inv.inversionId it) viewLoadTransfer


-- ----------------------------------------------------------------
-- CURRENT STEP / PROGRESS
-- ----------------------------------------------------------------

data CurrentStep
  = Downloading TransferStep
  | Preprocessing
  | Inverting InvertStep
  | Generating
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


currentStep :: (Globus :> es) => InversionStep -> Eff es CurrentStep
currentStep = \case
  StepCreated _ -> pure (Downloading Select)
  StepDownloading dwn -> do
    let t = grab @Transfer dwn :: Transfer
    pure $ Downloading $ Transferring (Id t.taskId)
  StepDownloaded _ -> pure Preprocessing
  StepPreprocessed _ -> pure (Inverting mempty)
  StepInverting inv -> do
    let i = grab @Invert inv
    pure $ Inverting $ InvertStep i.commit (Id <$> i.taskId)
  StepInverted _ -> pure Generating
  StepGenerated _ -> pure Publishing
  StepPublished _ -> pure Complete


invProgress :: CurrentStep -> View InversionStatus ()
invProgress curr = do
  row (gap 10) $ do
    prgDown curr "1" "DOWNLOAD"
    prgStep Preprocessing "2" "PREPROCESS"
    prgInv curr "3" "INVERT"
    prgStep Generating "4" "GENERATE"
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

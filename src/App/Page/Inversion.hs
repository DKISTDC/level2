module App.Page.Inversion where

import App.Colors
import App.Effect.Auth
import App.Effect.Scratch (Scratch)
import App.Effect.Scratch qualified as Scratch
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Inversions.InvForm (CommitAction (..), CommitForm (..), TransferAction (..))
import App.Page.Inversions.InvForm qualified as InvForm
import App.Route as Route
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons qualified as Icons
import App.View.Layout
import App.Worker.GenWorker as Gen (GenFits (..), GenFitsStatus (..), GenFitsStep (..))
import Data.Maybe (isJust)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log hiding (Info)
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole
import Web.Hyperbole.Forms (Input)


page
  :: (Hyperbole :> es, Log :> es, Inversions :> es, Datasets :> es, Auth :> es, Globus :> es, Tasks GenFits :> es, Time :> es)
  => Id Proposal
  -> Id Inversion
  -> Page es (InversionStatus, GenerateTransfer, InversionCommit, DownloadTransfer, UploadTransfer)
page _ i = do
  handle (inversions redirectHome, generateTransfer, inversionCommit, downloadTransfer, uploadTransfer) $ do
    inv <- loadInversion i
    appLayout Inversions $ do
      col Style.page $ do
        col (gap 5) $ do
          el Style.header $ do
            text "Inversion - "
            text i.fromId

          el_ $ do
            text "Program - "
            route (Route.Proposal inv.proposalId $ Program inv.programId) Style.link $ do
              text inv.programId.fromId

          el_ $ do
            text "Proposal - "
            route (Route.Proposal inv.proposalId PropRoot) Style.link $ do
              text inv.proposalId.fromId

        hyper (InversionStatus inv.proposalId inv.programId inv.inversionId) $ viewInversion inv


submitUpload
  :: forall es
   . (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es, Scratch :> es)
  => Id Proposal
  -> Id Inversion
  -> Eff es Response
submitUpload ip ii = do
  tfrm <- formData @TransferForm
  tup <- formData @(UploadFiles Filename)
  it <- requireLogin $ Globus.initUpload tfrm tup ip ii
  Inversions.setUploading ii it

  redirect $ routeUrl (Route.Proposal ip $ Route.Inversion ii Inv)


submitDownload :: (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es) => Id Proposal -> Id Inversion -> Eff es Response
submitDownload ip ii = do
  tfrm <- formData @TransferForm
  tfls <- formData @DownloadFolder
  inv <- loadInversion ii
  ds <- send $ Datasets.Query $ Datasets.ByProgram inv.programId
  it <- requireLogin $ Globus.initDownloadL1Inputs tfrm tfls ds
  Inversions.setDownloading ii it
  redirect $ routeUrl (Route.Proposal ip $ Route.Inversion ii Inv)


loadInversion :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> Eff es Inversion
loadInversion ii = do
  (inv :| _) <- send (Inversions.ById ii) >>= expectFound
  pure inv


redirectHome :: (Hyperbole :> es) => Eff es (View InversionStatus ())
redirectHome = do
  redirect $ pathUrl . routePath $ Inversions


-- ----------------------------------------------------------------
-- INVERSION STATUS -----------------------------------------------
-- ----------------------------------------------------------------

type InversionViews = '[DownloadTransfer, UploadTransfer, InversionCommit, GenerateTransfer]


data InversionStatus = InversionStatus (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)
instance HyperView InversionStatus where
  type Action InversionStatus = InversionAction
  type Require InversionStatus = InversionViews


data InversionAction
  = Download
  | Cancel
  | Upload
  | Publish
  | Reload
  | RegenError
  | RegenFits
  | RegenAsdf
  deriving (Show, Read, ViewAction)


inversions :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Tasks GenFits :> es, Time :> es) => Eff es (View InversionStatus ()) -> InversionStatus -> InversionAction -> Eff es (View InversionStatus ())
inversions onCancel (InversionStatus ip iip ii) = \case
  Cancel -> do
    send $ Inversions.Remove ii
    onCancel
  Download -> do
    r <- request
    requireLogin $ do
      redirect $ Globus.fileManagerSelectUrl (Folders 1) (Route.Proposal ip $ Route.Inversion ii SubmitDownload) ("Transfer Instrument Program " <> iip.fromId) r
  Upload -> do
    r <- request
    requireLogin $ do
      redirect $ Globus.fileManagerSelectUrl (Files 4) (Route.Proposal ip $ Route.Inversion ii SubmitUpload) ("Transfer Inversion Results " <> ii.fromId) r
  Publish -> do
    Inversions.setPublished ii
    refresh
  Reload -> do
    refresh
  RegenError -> do
    Inversions.clearError ii
    refresh
  RegenFits -> do
    Inversions.resetGenerating ii
    refresh
  RegenAsdf -> do
    Inversions.resetGeneratingAsdf ii
    refresh
 where
  refresh = do
    inv <- loadInversion ii
    pure $ viewInversion inv


viewInversionContainer :: InversionStep -> View InversionStatus () -> View InversionStatus ()
viewInversionContainer step cnt =
  col (Style.card . gap 15) $ do
    el (Style.cardHeader (headerColor step)) "Inversion"
    col (gap 15 . pad 15) $ do
      invProgress step
      cnt
 where
  headerColor (StepPublish (StepPublished _)) = Success
  headerColor (StepGenerate (StepGenerateError _)) = Danger
  headerColor _ = Info


viewInversion :: Inversion -> View InversionStatus ()
viewInversion inv = do
  let step = inversionStep inv
  col (gap 10) $ do
    viewInversionContainer step $ do
      el bold "Download"
      viewDownload inv inv.download

      el bold "Invert"
      viewInvert inv (inverting inv.invert)

      el bold "Generate"

      el bold "Publish"
      viewPublish

      stepDone
 where
  viewPublish = do
    button Publish (Style.btn Primary . grow) "Mark as Published"
    row (gap 10) $ do
      viewFiles
      button RegenFits (Style.btnOutline Danger) "Regen Fits"
      button RegenAsdf (Style.btnOutline Danger) "Regen Asdf"

  stepDone = do
    el_ "Inversion Complete"
    viewFiles

  viewFiles =
    link (Globus.fileManagerOpenInv $ Scratch.outputL2Dir inv.proposalId inv.inversionId) (Style.btnOutline Secondary . grow . att "target" "_blank") "View Generated Files"


-- ----------------------------------------------------------------
-- STEP DOWNLOAD
-- ----------------------------------------------------------------

data DownloadTransfer = DownloadTransfer (Id Proposal) (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Show, Read, ViewId)
instance HyperView DownloadTransfer where
  type Action DownloadTransfer = TransferAction
  type Require DownloadTransfer = '[InversionStatus]


downloadTransfer
  :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es, Time :> es)
  => DownloadTransfer
  -> TransferAction
  -> Eff es (View DownloadTransfer ())
downloadTransfer (DownloadTransfer ip iip ii ti) = \case
  TaskFailed -> do
    -- inv <- loadInversion ii
    pure $ do
      col (gap 10) $ do
        InvForm.viewTransferFailed ti
  -- hyper (InversionStatus ip iip ii) $ stepDownload inv Select
  TaskSucceeded -> do
    -- need to get the datasets used???
    ds <- send $ Datasets.Query $ Datasets.ByProgram iip
    Inversions.setDownloaded ii (map (.datasetId) ds)
    pure $ target (InversionStatus ip iip ii) $ onLoad Reload 0 none
  CheckTransfer -> InvForm.checkTransfer ti


viewDownload :: Inversion -> StepDownload -> View InversionStatus ()
viewDownload _ StepDownloadNone = do
  el_ "Please select a destination folder for the instrument program's datasets. You will be redirected to Globus."
  row (gap 10) $ do
    button Download (Style.btn Primary . grow) "Choose Folder"
    button Cancel (Style.btnOutline Secondary) "Cancel"
viewDownload inv (StepDownloading dwn) = do
  hyper (DownloadTransfer inv.proposalId inv.programId inv.inversionId dwn.transfer) InvForm.viewLoadTransfer
viewDownload _ (StepDownloaded _) = do
  el_ "Complete"
  row (gap 10) $ do
    button Download (Style.btn Primary . grow) "Choose Folder"
    button Cancel (Style.btnOutline Secondary) "Cancel"


-- ----------------------------------------------------------------
-- STEP INVERT
-- ----------------------------------------------------------------

data InversionCommit = InversionCommit (Id Proposal) (Id Inversion)
  deriving (Show, Read, ViewId)
instance HyperView InversionCommit where
  type Action InversionCommit = CommitAction
  type Require InversionCommit = '[InversionStatus]


inversionCommit
  :: (Hyperbole :> es, Log :> es, Inversions :> es, Globus :> es, Tasks GenFits :> es, Time :> es)
  => InversionCommit
  -> CommitAction
  -> Eff es (View InversionCommit ())
inversionCommit (InversionCommit ip ii) = action
 where
  action LoadValid = InvForm.loadValid inversionCommitLabel
  action (CheckCommitValid gc) = do
    log Debug "CheckCommitValid"
    vg <- InvForm.validate (InversionCommit ip ii) desireRepo gc inversionCommitLabel $ do
      Inversions.setInverted ii gc

    pure $ InvForm.commitForm (Just gc) (CommitForm vg) inversionCommitLabel


inversionCommitLabel :: View (Input id Validated GitCommit) ()
inversionCommitLabel =
  link "https://github.com/han-uitenbroek/RH" (att "target" "_blank") $ label "DeSIRe Git Commit"


data UploadTransfer = UploadTransfer (Id Proposal) (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Show, Read, ViewId)
instance HyperView UploadTransfer where
  type Action UploadTransfer = TransferAction
  type Require UploadTransfer = '[InversionStatus]


uploadTransfer :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Tasks GenFits :> es, Time :> es) => UploadTransfer -> TransferAction -> Eff es (View UploadTransfer ())
uploadTransfer (UploadTransfer ip iip ii ti) = \case
  TaskFailed -> do
    pure $ do
      col (gap 10) $ do
        InvForm.viewTransferFailed ti
        target (InversionStatus ip iip ii) $ do
          uploadSelect (Invalid "Upload Task Failed")
  TaskSucceeded -> do
    Inversions.setUploaded ii
    pure $ target (InversionStatus ip iip ii) $ do
      uploadSelect Valid
  CheckTransfer -> InvForm.checkTransfer ti


-- invertReload :: (Hyperbole :> es, Inversions :> es, Globus :> es, Tasks GenFits :> es) => Id Proposal -> Id Inversion -> View id () -> Eff es (View id ())
-- invertReload ip ii vw = do
--   inv <- loadInversion ii
--   pure $ target (InversionStatus inv.proposalId inv.inversionId) $ onLoad Reload 0 none

-- -- | Check to see if we are have all the inversion fields filled out and need to reload
-- checkInvertReload :: (HyperViewHandled InversionStatus id, Hyperbole :> es, Inversions :> es, Globus :> es, Tasks GenFits :> es) => Id Proposal -> Id Inversion -> View id () -> Eff es (View id ())
-- checkInvertReload ip ii vw = do
--   inv <- loadInversion ii
--   pure $ case inv.step of
--     StepGenerate _ -> hyper (InversionStatus inv.proposalId inv.programId inv.inversionId) $ onLoad Reload 0 none
--     _ -> vw

viewInvert :: Inversion -> Inverting -> View InversionStatus ()
viewInvert inv (Inverting mt mc) = do
  hyper (InversionCommit inv.proposalId inv.inversionId) $ InvForm.commitForm mc (InvForm.fromExistingCommit mc) inversionCommitLabel
  do
    viewUploadTransfer mt
 where
  viewUploadTransfer (Just it) = do
    hyper (UploadTransfer inv.proposalId inv.programId inv.inversionId it) InvForm.viewLoadTransfer
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
    -- li id "timestamps.tsv"
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

data GenerateTransfer = GenerateTransfer (Id Proposal) (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Show, Read, ViewId)
instance HyperView GenerateTransfer where
  type Action GenerateTransfer = TransferAction


generateTransfer :: (Tasks GenFits :> es, Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es) => GenerateTransfer -> TransferAction -> Eff es (View GenerateTransfer ())
generateTransfer (GenerateTransfer ip iip ii ti) = \case
  TaskFailed -> do
    pure $ viewGenerateDownload $ do
      InvForm.viewTransferFailed ti
      target (InversionStatus ip iip ii) $ do
        button RegenFits (Style.btn Primary) "Restart Transfer"
  TaskSucceeded -> do
    -- reload immediately... it should be marked as uploaded and go somewhere else
    s <- send $ TaskGetStatus $ GenFits ip ii
    pure $ do
      target (InversionStatus ip iip ii) $
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
      el (height 45) Icons.spinner
  GenTransfering taskId -> do
    hyper (GenerateTransfer inv.proposalId inv.programId inv.inversionId taskId) $ do
      viewGenerateDownload InvForm.viewLoadTransfer
  GenConvert s -> do
    viewGenerateWait s
  GenAsdf -> do
    onLoad Reload 1000 $ do
      col (gap 5) $ do
        el bold "Generate Asdf"
        el_ "Generating..."
  GenConvError e -> do
    el bold "Generate Error!"
    el_ $ text $ cs e
    button RegenError (Style.btn Primary) "Restart"


viewGenerateDownload :: View GenerateTransfer () -> View GenerateTransfer ()
viewGenerateDownload vw = col (gap 5) $ do
  el bold "Downloading L1 Files"
  vw


viewGenerateWait :: GenFitsStatus -> View InversionStatus ()
viewGenerateWait s =
  onLoad Reload 1000 $ do
    col (gap 5) $ do
      el bold "Generate Frames"
      viewStatus s.step
 where
  viewStatus = \case
    GenCreating -> do
      row (gap 5) $ do
        el_ "Creating"
        space
        el_ $ text $ cs $ show s.complete
        el_ " / "
        el_ $ text $ cs $ show s.total
      View.progress (fromIntegral s.complete / fromIntegral s.total)
    GenWaiting ->
      el_ "Waiting for job to start"
    GenStarted ->
      el_ "Started"
    GenTransferring ->
      el_ "Transferring L1 Files"


data GenerateStep
  = GenWaitStart
  | GenTransfering (Id Task)
  | GenConvert GenFitsStatus
  | GenConvError Text
  | GenAsdf
  deriving (Eq, Show, Ord)


-- ----------------------------------------------------------------
-- CURRENT STEP / PROGRESS
-- ----------------------------------------------------------------

-- data CurrentStep
--   = Downloading TransferStep
--   | Preprocessing
--   | Inverting InvertStep
--   | Generating GenerateStep
--   | Publishing
--   | Complete
--   deriving (Eq, Ord, Show)

-- data TransferStep
--   = TransferSelect
--   | Transferring (Id Task)
--   | TransferComplete
--   deriving (Eq, Ord, Show)

-- data InvertStep = InvertStep (Maybe GitCommit) (Maybe (Id Task))
--   deriving (Show, Eq, Ord)
-- instance Semigroup InvertStep where
--   InvertStep gc1 ti1 <> InvertStep gc2 ti2 = InvertStep (gc1 <|> gc2) (ti1 <|> ti2)
-- instance Monoid InvertStep where
--   mempty = InvertStep Nothing Nothing

-- newtype CurrentTask = CurrentTask Task
--   deriving (Eq)

-- downloadStep :: InversionStep -> TransferStep
-- downloadStep = \case
--   StepDownloading dwn ->
--     let t = grab @Transfer dwn :: Transfer
--      in Transferring t.taskId
--   StepCreated _ -> Select
--   _ -> TransferComplete
--
--
-- invertStep :: InversionStep -> InvertStep
-- invertStep = \case
--   StepInverting inv -> do
--     let i = grab @Invert inv
--     pure $ Inverting $ InvertStep i.commit i.taskId
--
--
-- currentStep :: (Globus :> es, Tasks GenFits :> es) => Id Proposal -> Id Inversion -> InversionStep -> Eff es CurrentStep
-- currentStep ip ii = \case
--   StepCreated _ -> pure (Downloading Select)
--   StepDownloading dwn -> do
--     let t = grab @Transfer dwn :: Transfer
--     pure $ Downloading $ Transferring t.taskId
--   StepDownloaded _ -> pure Preprocessing
--   StepPreprocessed _ -> pure (Inverting mempty)
--   StepInverting inv -> do
--     let i = grab @Invert inv
--     pure $ Inverting $ InvertStep i.commit i.taskId
--   StepInverted _ -> pure $ Generating GenWaitStart
--   StepGenerated _ -> pure Publishing
--   StepGenTransfer inv -> do
--     let t = grab @Transfer inv
--     pure $ Generating $ GenTransfering t.taskId
--   StepGenerating inv -> do
--     let g = grab @Generate inv
--     checkGenError g $ do
--       s <- send $ TaskGetStatus (GenFits ip ii)
--       pure $ Generating (GenConvert s)
--   StepGeneratedFits inv -> do
--     let g = grab @Generate inv
--     checkGenError g $ do
--       pure $ Generating GenAsdf
--   StepPublished _ -> pure Complete
--  where
--   checkGenError g act =
--     case g.genError of
--       Just e -> pure $ Generating $ GenConvError e
--       Nothing -> act

invProgress :: InversionStep -> View InversionStatus ()
invProgress curr = do
  row (gap 10) $ do
    prgDown curr "1" "DOWNLOAD"
    prgInv curr "2" "INVERT"
    prgGen curr "3" "GENERATE"
    prgStep (StepPublish StepPublishNone) "4" "PUBLISH"
 where
  stat :: AppColor -> View InversionStatus () -> Text -> View InversionStatus ()
  stat clr icn lbl = col (color clr . gap 4) $ do
    row id $ do
      space
      el (circle . bg clr) icn
      space
    el (fontSize 12 . textAlign Center) (text lbl)

  prgDown (StepDownload StepDownloadNone) icn lbl = do
    stat Info icn lbl
    line Gray
  prgDown (StepDownload (StepDownloading _)) icn lbl = do
    stat Info icn lbl
    line Info
  prgDown _ _ lbl = do
    stat Success Icons.check lbl
    line Success

  prgInv (StepInvert (StepInverting inv)) icn lbl = do
    stat Info icn lbl
    line $
      if isJust inv.commit || isJust inv.transfer
        then Info
        else Gray
  prgInv _ icn lbl = do
    -- let clr = statColor (StepInverting mempty)
    -- stat clr (statIcon (StepInverting mempty) icn) lbl
    -- line clr
    none

  prgGen (StepGenerate _) icn lbl = do
    stat Info icn lbl
    line Info
  prgGen _ icn lbl = do
    -- let clr = statColor (Generating GenWaitStart)
    -- stat clr (statIcon (Generating GenWaitStart) icn) lbl
    -- line clr
    none

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

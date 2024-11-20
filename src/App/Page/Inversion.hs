module App.Page.Inversion where

import App.Colors
import App.Effect.Auth
import App.Effect.Scratch (Scratch)
import App.Effect.Scratch qualified as Scratch
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Dashboard (AdminLogin (..))
import App.Page.Inversions.CommitForm as CommitForm
import App.Page.Inversions.InvForm (TransferAction (..))
import App.Page.Inversions.InvForm qualified as InvForm
import App.Route as Route
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons qualified as Icons
import App.View.Layout
import App.Worker.GenWorker as Gen (GenFits (..), GenFitsStatus (..), GenFitsStep (..))
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


page
  :: (Hyperbole :> es, Auth :> es, Log :> es, Inversions :> es, Datasets :> es, Auth :> es, Globus :> es, Tasks GenFits :> es, Time :> es, IOE :> es)
  => Id Proposal
  -> Id Inversion
  -> Page es (InversionStatus, Generate, GenerateTransfer, InversionCommit, DownloadTransfer, UploadTransfer)
page ip i = do
  handle (inversions redirectHome, generate, generateTransfer, inversionCommit, downloadTransfer, uploadTransfer) $ do
    inv <- loadInversion i
    mtok <- send AdminToken
    login <- send LoginUrl
    let admin = AdminLogin mtok login
    status <- send $ TaskGetStatus $ GenFits ip i
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

        hyper (InversionStatus inv.proposalId inv.programId inv.inversionId) $ viewInversion inv admin status


submitUpload
  :: forall es
   . (Hyperbole :> es, Log :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es, Scratch :> es)
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

type InversionViews = '[DownloadTransfer, UploadTransfer, InversionCommit, Generate, GenerateTransfer]


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
 where
  refresh = do
    mtok <- send AdminToken
    login <- send LoginUrl
    inv <- loadInversion ii
    status <- send $ TaskGetStatus $ GenFits ip ii
    pure $ viewInversion inv (AdminLogin mtok login) status


viewInversionContainer :: InversionStep -> View c () -> View c ()
viewInversionContainer step cnt =
  col (Style.card . gap 15) $ do
    el (Style.cardHeader (headerColor step)) "Inversion"
    col (gap 0 . pad 15) $ do
      cnt
 where
  headerColor (StepPublish (StepPublished _)) = Success
  headerColor (StepGenerate (StepGenerateError _)) = Danger
  headerColor _ = Info


viewInversion :: Inversion -> AdminLogin -> GenFitsStatus -> View InversionStatus ()
viewInversion inv admin status = do
  let step = inversionStep inv
  col (gap 10) $ do
    viewInversionContainer step $ do
      downloadStep step $ do
        viewDownload inv inv.download

      invertStep step $ do
        viewInvert inv inv.invert

      generateStep step $ do
        hyper (Generate inv.proposalId inv.programId inv.inversionId) $
          viewGenerate inv admin status inv.generate

      publishStep step $ do
        viewPublish inv inv.publish


-- stepDone = do
--   el_ "Inversion Complete"
--   viewFiles

viewStep :: Step -> Int -> Text -> View c () -> View c ()
viewStep step num stepName = viewStep' step num stepName (stepLine step)


viewStep' :: Step -> Int -> Text -> View c () -> View c () -> View c ()
viewStep' step num stepName line cnt =
  row (gap 10 . stepEnabled) $ do
    col id $ do
      stepCircle step num
      line
    col (gap 10 . grow . pad (TRBL 0 0 40 0)) $ do
      el (bold . color (stepColor step) . fontSize 22) (text stepName)
      cnt
 where
  stepEnabled =
    case step of
      StepNext -> Style.disabled
      _ -> id


stepLine :: Step -> View c ()
stepLine = \case
  StepActive -> line Info
  StepNext -> line Secondary
  StepComplete -> line Success
  StepError -> line Danger
 where
  line clr = el (grow . border (TRBL 0 2 0 0) . width 18 . borderColor clr) ""


stepCircle :: Step -> Int -> View c ()
stepCircle step num =
  el (circle . bg (stepColor step)) stepIcon
 where
  circle = rounded 50 . pad 5 . color White . textAlign Center . width 34 . height 34
  stepIcon =
    case step of
      StepComplete -> Icons.check
      StepError -> "!"
      _ -> text $ cs $ show num


stepColor :: Step -> AppColor
stepColor = \case
  StepActive -> Info
  StepNext -> Secondary
  StepComplete -> Success
  StepError -> Danger


-- stepHeader :: AppColor -> View c () -> Text -> View c ()
-- stepHeader clr icon lbl = do
--   row (gap 10 . color clr) $ do
--     el (circle . bg clr) icon
--     col id $ do
--       space
--       el (bold) (text lbl)
--       space

-- stat :: AppColor -> View c () -> Text -> View c ()
-- stat clr icn lbl =
--   col (color clr . gap 4) $ do
--     row id $ do
--       space
--       el (circle . bg clr) icn
--       space
--     el (fontSize 12 . textAlign Center) (text lbl)

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


downloadStep :: InversionStep -> View c () -> View c ()
downloadStep current content =
  case current of
    StepDownload StepDownloadNone -> step StepActive "Download"
    StepDownload (StepDownloading _) -> step StepActive "Downloading"
    _ -> step StepComplete "Downloaded"
 where
  step s title = viewStep s 1 title content


viewDownload :: Inversion -> StepDownload -> View InversionStatus ()
viewDownload _ StepDownloadNone = do
  el_ "Please select a destination folder for the instrument program's datasets. You will be redirected to Globus."
  row (gap 10) $ do
    button Download (Style.btn Primary . grow) "Choose Folder"
-- button Cancel (Style.btnOutline Secondary) "Cancel"
viewDownload inv (StepDownloading dwn) = do
  hyper (DownloadTransfer inv.proposalId inv.programId inv.inversionId dwn.transfer) InvForm.viewLoadTransfer
viewDownload _ (StepDownloaded _) = do
  row (gap 10) $ do
    button Download (Style.btnOutline Secondary . grow) "Download Again"


-- button Cancel (Style.btnOutline Secondary) "Cancel"

-- ----------------------------------------------------------------
-- STEP INVERT
-- ----------------------------------------------------------------

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
    -- reload the parent
    pure $ target (InversionStatus ip iip ii) $ onLoad Reload 200 $ do
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

data Step
  = StepActive
  | StepNext
  | StepComplete
  | StepError


invertStep :: InversionStep -> View c () -> View c ()
invertStep current content =
  case current of
    StepDownload _ -> step StepNext "Invert"
    StepInvert StepInvertNone -> step StepActive "Invert"
    StepInvert (StepInverting _) -> step StepActive "Inverting"
    _ -> step StepComplete "Inverted"
 where
  step s t = viewStep s 2 t content


viewInvert :: Inversion -> StepInvert -> View InversionStatus ()
viewInvert inv = \case
  StepInvertNone -> none
  StepInverting (Inverting mt mc) -> do
    hyper (InversionCommit inv.proposalId inv.inversionId) $ do
      CommitForm.commitForm mc (CommitForm.fromExistingCommit mc)
    viewUploadTransfer mt
  StepInverted inverted -> do
    let mc = Just inverted.commit
    -- let mt = Just inverted.transfer
    hyper (InversionCommit inv.proposalId inv.inversionId) $ do
      CommitForm.commitForm mc (CommitForm.fromExistingCommit mc)
    el_ "Upload Inversion Results"
    button Upload (Style.btnOutline Success . grow) "Select New Files"
 where
  viewUploadTransfer (Just it) = do
    hyper (UploadTransfer inv.proposalId inv.programId inv.inversionId it) InvForm.viewLoadTransfer
  viewUploadTransfer Nothing = do
    uploadSelect NotInvalid


uploadSelect :: Validated (Id Task) -> View InversionStatus ()
uploadSelect val = do
  col (gap 5 . file val) $ do
    el id "Upload Inversion Results"
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

-- we don't have the task here.... have to load anc check
data Generate = Generate (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)
instance HyperView Generate where
  type Action Generate = GenerateAction
  type Require Generate = '[GenerateTransfer]


generate :: (Tasks GenFits :> es, Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es) => Generate -> GenerateAction -> Eff es (View Generate ())
generate (Generate ip _ ii) = \case
  ReloadGen ->
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
    status <- send $ TaskGetStatus $ GenFits ip ii
    inv <- loadInversion ii
    mtok <- send AdminToken
    login <- send LoginUrl
    pure $ viewGenerate inv (AdminLogin mtok login) status inv.generate


data GenerateAction
  = ReloadGen
  | RegenError
  | RegenFits
  | RegenAsdf
  deriving (Show, Read, ViewAction)


generateStep :: InversionStep -> View c () -> View c ()
generateStep current content =
  case current of
    StepDownload _ -> step StepNext "Generate"
    StepInvert _ -> step StepNext "Generate"
    StepGenerate (StepGenerateError _) -> step StepError "Generating"
    StepGenerate _ -> step StepActive "Generating"
    StepPublish _ -> step StepComplete "Generated"
 where
  step s t = viewStep s 3 t content


viewGenerate :: Inversion -> AdminLogin -> GenFitsStatus -> StepGenerate -> View Generate ()
viewGenerate inv admin status = \case
  StepGenerateNone -> do
    none
  StepGenerateWaiting -> do
    onLoad ReloadGen 1000 $ do
      row id $ do
        loadingMessage "Waiting for job to start"
        space
        case admin.token of
          Nothing -> link admin.loginUrl (Style.btnOutline Danger) "Needs Globus Login"
          Just _ -> pure ()
  StepGenerateError e -> do
    el (bold . color Danger) "Error!"
    row collapse $ View.systemError $ cs e
    button RegenError (Style.btn Primary) "Restart"
  StepGenerateTransferring taskId -> do
    loadingMessage "Generating Fits - Transferring L1 Files"
    hyper (GenerateTransfer inv.proposalId inv.programId inv.inversionId taskId) $ do
      InvForm.viewLoadTransfer
  StepGeneratingFits _ -> do
    col (gap 10) $ do
      loadingMessage "Generating Fits"
      onLoad ReloadGen 1000 $ do
        viewStatus status.step
  StepGeneratingAsdf _ -> do
    loadingMessage "Generating Asdf"
    onLoad ReloadGen 1000 none
  StepGenerated _ -> do
    row (gap 10) $ do
      viewGeneratedFiles inv
      button RegenFits (Style.btnOutline Secondary) "Regen Fits"
      button RegenAsdf (Style.btnOutline Secondary) "Regen Asdf"
 where
  loadingMessage msg =
    row (gap 5) $ do
      el (width 20) Icons.spinnerCircle
      el_ $ text msg

  viewStatus = \case
    GenCreating -> do
      row (gap 5) $ do
        space
        el_ $ text $ cs $ show status.complete
        el_ " / "
        el_ $ text $ cs $ show status.total
      View.progress (fromIntegral status.complete / fromIntegral status.total)
    GenWaiting ->
      el_ "Waiting for job to start"
    GenStarted ->
      el_ "Started"
    GenTransferring ->
      el_ "..."


-- GenerateTransfer ---------------------------------------------

data GenerateTransfer = GenerateTransfer (Id Proposal) (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Show, Read, ViewId)


instance HyperView GenerateTransfer where
  type Action GenerateTransfer = TransferAction


generateTransfer :: (Tasks GenFits :> es, Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es) => GenerateTransfer -> TransferAction -> Eff es (View GenerateTransfer ())
generateTransfer (GenerateTransfer ip iip ii ti) = \case
  TaskFailed -> do
    pure $ do
      InvForm.viewTransferFailed ti
      target (Generate ip iip ii) $ do
        button RegenFits (Style.btn Primary) "Restart Transfer"
  TaskSucceeded -> do
    pure $ target (Generate ip iip ii) $ onLoad ReloadGen 1000 (el_ "SUCCEEDED")
  CheckTransfer -> do
    InvForm.checkTransfer ti


viewGeneratedFiles :: Inversion -> View c ()
viewGeneratedFiles inv =
  link (Globus.fileManagerOpenInv $ Scratch.outputL2Dir inv.proposalId inv.inversionId) (Style.btnOutline Success . grow . att "target" "_blank") "View Generated Files"


-- GenWaitStart -> do
-- GenConvert s -> do
--   viewGenerateWait s
-- GenAsdf -> do
--   onLoad Reload 1000 $ do
--     col (gap 5) $ do
--       el bold "Generate Asdf"
--       el_ "Generating..."

-- ----------------------------------------------------------------
-- STEP PUBLISH
-- ----------------------------------------------------------------

publishStep :: InversionStep -> View c () -> View c ()
publishStep current content =
  case current of
    StepPublish (StepPublished _) -> step StepComplete "Published"
    StepPublish _ -> step StepActive "Publishing"
    _ -> step StepNext "Publish"
 where
  step s t = viewStep' s 4 t none content


viewPublish :: Inversion -> StepPublish -> View InversionStatus ()
viewPublish _ = \case
  StepPublishNone -> none
  StepPublishing -> do
    button Publish (Style.btn Primary . grow) "TODO: Mark as Published"
  StepPublished _ -> el_ "TODO: Link to files in portal"

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

-- invProgress :: InversionStep -> View InversionStatus ()
-- invProgress curr = do
--   row (gap 10) $ do
--     prgDown curr "1" "DOWNLOAD"
--     prgInv curr "2" "INVERT"
--     prgGen curr "3" "GENERATE"
--     prgStep (StepPublish StepPublishNone) "4" "PUBLISH"
--  where
--   stat :: AppColor -> View InversionStatus () -> Text -> View InversionStatus ()
--   stat clr icn lbl = col (color clr . gap 4) $ do
--     row id $ do
--       space
--       el (circle . bg clr) icn
--       space
--     el (fontSize 12 . textAlign Center) (text lbl)
--
--   prgDown (StepDownload StepDownloadNone) icn lbl = do
--     stat Info icn lbl
--     line Gray
--   prgDown (StepDownload (StepDownloading _)) icn lbl = do
--     stat Info icn lbl
--     line Info
--   prgDown _ _ lbl = do
--     stat Success Icons.check lbl
--     line Success
--
--   prgInv (StepInvert (StepInverting inv)) icn lbl = do
--     stat Info icn lbl
--     line $
--       if isJust inv.commit || isJust inv.transfer
--         then Info
--         else Gray
--   prgInv _ icn lbl = do
--     -- let clr = statColor (StepInverting mempty)
--     -- stat clr (statIcon (StepInverting mempty) icn) lbl
--     -- line clr
--     none
--
--   prgGen (StepGenerate _) icn lbl = do
--     stat Info icn lbl
--     line Info
--   prgGen _ icn lbl = do
--     -- let clr = statColor (Generating GenWaitStart)
--     -- stat clr (statIcon (Generating GenWaitStart) icn) lbl
--     -- line clr
--     none
--
--   prgStep s icn lbl = do
--     stat (statColor s) (statIcon s icn) lbl
--     line (lineColor s)
--
--   statIcon s icon
--     | s < curr = Icons.check
--     | otherwise = icon
--
--   statColor s
--     | s == curr = Info
--     | s < curr = Success
--     | otherwise = Gray
--
--   stat :: AppColor -> View InversionStatus () -> Text -> View InversionStatus ()
--   stat clr icn lbl = col (color clr . gap 4) $ do
--     row id $ do
--       space
--       el (circle . bg clr) icn
--       space
--     el (fontSize 12 . textAlign Center) (text lbl)
--
--   line clr = col grow $ do
--     el (border (TRBL 0 0 2 0) . height 20 . borderColor clr) ""
--
--   lineColor s
--     | s == curr = Gray
--     | s < curr = Success
--     | otherwise = Gray
--
--   circle = rounded 50 . pad 5 . color White . textAlign Center . width 34 . height 34

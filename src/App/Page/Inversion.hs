{-# LANGUAGE UndecidableInstances #-}

module App.Page.Inversion where

import App.Colors
import App.Effect.Auth
import App.Effect.Publish qualified as Publish
import App.Effect.Scratch (Scratch)
import App.Effect.Scratch qualified as Scratch
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Dashboard (AdminLogin (..))
import App.Page.Inversions.CommitForm as CommitForm
import App.Page.Inversions.Transfer (TransferAction (..))
import App.Page.Inversions.Transfer qualified as Transfer
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
  :: (Hyperbole :> es, Auth :> es, Log :> es, Inversions :> es, Datasets :> es, Auth :> es, Globus :> es, Tasks GenFits :> es, Time :> es)
  => Id Proposal
  -> Id Inversion
  -> Eff es (Page '[InversionStatus, Generate, GenerateTransfer, InversionCommit, DownloadTransfer, UploadTransfer, Publish])
page ip i = do
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
  ds <- Datasets.find $ Datasets.ByProgram inv.programId
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

type InversionViews = '[DownloadTransfer, UploadTransfer, InversionCommit, Generate, GenerateTransfer, Publish]


data InversionStatus = InversionStatus (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Globus :> es, Auth :> es, Tasks GenFits :> es, Time :> es, Scratch :> es) => HyperView InversionStatus es where
  data Action InversionStatus
    = Download
    | Upload
    | Reload
    deriving (Show, Read, ViewAction)
  type Require InversionStatus = InversionViews


  update action = do
    InversionStatus ip iip ii <- viewId
    case action of
      Download -> do
        r <- request
        requireLogin $ do
          redirect $ Globus.fileManagerSelectUrl (Folders 1) (Route.Proposal ip $ Route.Inversion ii SubmitDownload) ("Transfer Instrument Program " <> iip.fromId) r
      Upload -> do
        r <- request
        requireLogin $ do
          redirect $ Globus.fileManagerSelectUrl (Files 4) (Route.Proposal ip $ Route.Inversion ii SubmitUpload) ("Transfer Inversion Results " <> ii.fromId) r
      Reload -> do
        refresh
   where
    refresh = do
      InversionStatus ip _ ii <- viewId
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
        hyper (Publish inv.proposalId inv.programId inv.inversionId) $
          viewPublish inv inv.publish


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
  circle = rounded 50 . pad 5 . color White . textAlign AlignCenter . width 34 . height 34
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


-- ----------------------------------------------------------------
-- STEP DOWNLOAD
-- ----------------------------------------------------------------

data DownloadTransfer = DownloadTransfer (Id Proposal) (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Show, Read, ViewId)
instance (Inversions :> es, Globus :> es, Auth :> es, Datasets :> es, Time :> es) => HyperView DownloadTransfer es where
  data Action DownloadTransfer
    = DwnTransfer TransferAction
    deriving (Show, Read, ViewAction)


  type Require DownloadTransfer = '[InversionStatus]


  update (DwnTransfer action) = do
    DownloadTransfer ip iip ii ti <- viewId
    case action of
      TaskFailed -> do
        -- inv <- loadInversion ii
        pure $ do
          col (gap 10) $ do
            Transfer.viewTransferFailed ti
      -- hyper (InversionStatus ip iip ii) $ stepDownload inv Select
      TaskSucceeded -> do
        -- need to get the datasets used???
        ds <- Datasets.find $ Datasets.ByProgram iip
        Inversions.setDownloaded ii (map (.datasetId) ds)
        pure $ target (InversionStatus ip iip ii) $ el (onLoad Reload 0) none
      CheckTransfer -> Transfer.checkTransfer DwnTransfer ti


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
  hyper (DownloadTransfer inv.proposalId inv.programId inv.inversionId dwn.transfer) (Transfer.viewLoadTransfer DwnTransfer)
viewDownload _ (StepDownloaded _) = do
  row (gap 10) $ do
    button Download (Style.btnOutline Secondary . grow) "Download Again"


-- button Cancel (Style.btnOutline Secondary) "Cancel"

-- ----------------------------------------------------------------
-- STEP INVERT
-- ----------------------------------------------------------------

data UploadTransfer = UploadTransfer (Id Proposal) (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Globus :> es, Auth :> es, Tasks GenFits :> es, Time :> es) => HyperView UploadTransfer es where
  data Action UploadTransfer
    = UpTransfer TransferAction
    deriving (Show, Read, ViewAction)
  type Require UploadTransfer = '[InversionStatus]


  update (UpTransfer action) = do
    UploadTransfer ip iip ii ti <- viewId
    case action of
      TaskFailed -> do
        pure $ do
          col (gap 10) $ do
            Transfer.viewTransferFailed ti
            target (InversionStatus ip iip ii) $ do
              uploadSelect (Invalid "Upload Task Failed")
      TaskSucceeded -> do
        Inversions.setUploaded ii
        -- reload the parent
        pure $ target (InversionStatus ip iip ii) $ do
          el (onLoad Reload 200) $ do
            uploadSelect Valid
      CheckTransfer -> Transfer.checkTransfer UpTransfer ti


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
viewInvert inv step =
  col (gap 10) $ do
    case step of
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
        col (gap 5) $ do
          el_ "Upload Inversion Results"
          button Upload (Style.btnOutline Success . grow) "Select New Files"
 where
  viewUploadTransfer (Just it) = do
    hyper (UploadTransfer inv.proposalId inv.programId inv.inversionId it) (Transfer.viewLoadTransfer UpTransfer)
  viewUploadTransfer Nothing = do
    uploadSelect NotInvalid


uploadSelect :: Validated (Id Task) -> View InversionStatus ()
uploadSelect val = do
  col (gap 5 . file val) $ do
    el id "Upload Inversion Results"
    instructions val
    ul id $ do
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

  instructions Valid = none
  instructions _ = el_ "Please select the following files for upload. You will be redirected to Globus"


-- ----------------------------------------------------------------
-- STEP GENERATE
-- ----------------------------------------------------------------

data Generate = Generate (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Tasks GenFits :> es, Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es) => HyperView Generate es where
  data Action Generate
    = ReloadGen
    | RegenError
    | RegenFits
    | RegenAsdf
    deriving (Show, Read, ViewAction)


  type Require Generate = '[GenerateTransfer, InversionStatus]


  update action = do
    Generate _ _ ii <- viewId
    case action of
      ReloadGen ->
        refresh
      RegenError -> do
        Inversions.clearError ii
        refreshInversion
      RegenFits -> do
        Inversions.resetGenerating ii
        refreshInversion
      RegenAsdf -> do
        Inversions.resetGeneratingAsdf ii
        refreshInversion
   where
    refresh = do
      Generate ip _ ii <- viewId
      inv <- loadInversion ii
      case inv.generate of
        StepGenerated _ -> refreshInversion
        step -> do
          status <- send $ TaskGetStatus $ GenFits ip ii
          mtok <- send AdminToken
          login <- send LoginUrl
          pure $ do
            viewGenerate inv (AdminLogin mtok login) status step

    refreshInversion = do
      Generate ip iip ii <- viewId
      pure $ target (InversionStatus ip iip ii) $ el (onLoad Reload 0) "RELOAD?"


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
viewGenerate inv admin status step =
  col (gap 10) $ viewGenerateStep step
 where
  viewGenerateStep = \case
    StepGenerateNone -> do
      none
    StepGenerateWaiting -> do
      row (onLoad ReloadGen 1000) $ do
        loadingMessage "Waiting for job to start"
        space
        case admin.token of
          Nothing -> link admin.loginUrl (Style.btnOutline Danger) "Needs Globus Login"
          Just _ -> pure ()
    StepGenerateError e -> do
      row truncate $ View.systemError $ cs e
      button RegenError (Style.btn Primary) "Restart"
    StepGenerateTransferring taskId -> do
      el_ "Generating FITS - Transferring L1 Files"
      hyper (GenerateTransfer inv.proposalId inv.programId inv.inversionId taskId) $ do
        Transfer.viewLoadTransfer GenTransfer
    StepGeneratingFits _ -> do
      loadingMessage "Generating FITS"
      col (onLoad ReloadGen 1000) $ do
        viewStatus status.step
    StepGeneratingAsdf _ -> do
      loadingMessage "Generating ASDF"
      el (onLoad ReloadGen 1000) none
    StepGenerated _ -> do
      row (gap 10) $ do
        viewGeneratedFiles inv
        button RegenFits (Style.btnOutline Secondary) "Regen FITS"
        button RegenAsdf (Style.btnOutline Secondary) "Regen ASDF"

  loadingMessage msg =
    row (gap 5) $ do
      el (width 20) Icons.spinnerCircle
      col id $ do
        space
        el_ $ text msg
        space

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


instance (Tasks GenFits :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es) => HyperView GenerateTransfer es where
  type Require GenerateTransfer = '[Generate]
  data Action GenerateTransfer
    = GenTransfer TransferAction
    deriving (Show, Read, ViewAction)


  update (GenTransfer action) = do
    GenerateTransfer ip iip ii ti <- viewId
    case action of
      TaskFailed -> do
        pure $ do
          Transfer.viewTransferFailed ti
          target (Generate ip iip ii) $ do
            button RegenFits (Style.btn Primary) "Restart Transfer"
      TaskSucceeded ->
        pure $ do
          target (Generate ip iip ii) $ do
            el (onLoad ReloadGen 1000) "SUCCEEDED"
      CheckTransfer -> do
        Transfer.checkTransfer GenTransfer ti


viewGeneratedFiles :: Inversion -> View c ()
viewGeneratedFiles inv =
  link (Globus.fileManagerOpenInv $ Scratch.outputL2Dir inv.proposalId inv.inversionId) (Style.btnOutline Success . grow . att "target" "_blank") "View Generated Files"


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


data Publish = Publish (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Globus :> es, Auth :> es, IOE :> es, Scratch :> es, Time :> es) => HyperView Publish es where
  type Require Publish = '[InversionStatus]


  data Action Publish
    = StartSoftPublish
    | PublishTransfer (Id Task) TransferAction
    deriving (Show, Read, ViewAction)


  update action = do
    Publish propId _ invId <- viewId
    case action of
      StartSoftPublish -> do
        requireLogin $ do
          taskId <- Publish.transferSoftPublish propId invId
          Inversions.setPublishing invId taskId
          pure $ viewPublishing taskId
      PublishTransfer taskId TaskFailed -> do
        pure $ do
          Transfer.viewTransferFailed taskId
          button StartSoftPublish (Style.btn Primary . grow) "Restart Transfer"
      PublishTransfer _ TaskSucceeded -> do
        Inversions.setPublished invId
        refreshInversion
      PublishTransfer taskId CheckTransfer -> do
        Transfer.checkTransfer (PublishTransfer taskId) taskId
   where
    refreshInversion = do
      Publish propId progId invId <- viewId
      pure $ target (InversionStatus propId progId invId) $ do
        el (onLoad Reload 0) none


viewPublish :: Inversion -> StepPublish -> View Publish ()
viewPublish inv = \case
  StepPublishNone -> none
  StepPublishing Nothing -> do
    button StartSoftPublish (Style.btn Primary . grow) "Soft Publish"
  StepPublishing (Just taskId) -> viewPublishing taskId
  StepPublished _ -> viewPublished inv.proposalId inv.inversionId


viewPublishing :: Id Task -> View Publish ()
viewPublishing taskId = do
  el_ "Publishing..."
  Transfer.viewLoadTransfer (PublishTransfer taskId)


viewPublished :: Id Proposal -> Id Inversion -> View Publish ()
viewPublished propId invId = do
  link (Publish.fileManagerOpenPublish $ Publish.publishedDir propId invId) (Style.btnOutline Success . grow . att "target" "_blank") "View Published Files"

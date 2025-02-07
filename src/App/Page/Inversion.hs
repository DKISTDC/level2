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
import App.Page.Inversions.Transfer (TransferAction (..), activeTransfer, saveActiveTransfer)
import App.Page.Inversions.Transfer qualified as Transfer
import App.Route as Route
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons qualified as Icons
import App.View.Inversions (inversionStepColor)
import App.View.Layout
import App.Worker.GenWorker as Gen (GenFits (..), GenFitsStatus (..))
import App.Worker.Publish as Publish
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
  :: (Hyperbole :> es, Auth :> es, Log :> es, Inversions :> es, Datasets :> es, Auth :> es, Globus :> es, Tasks GenFits :> es, Time :> es, Tasks PublishTask :> es)
  => Id Proposal
  -> Id Inversion
  -> Eff es (Page '[InversionStatus, GenerateStep, GenerateTransfer, InversionCommit, UploadTransfer, PublishStep])
page propId invId = do
  inv <- loadInversion invId
  mtok <- send AdminToken
  login <- send LoginUrl
  let admin = AdminLogin mtok login
  up <- activeTransfer invId
  gen <- send $ TaskLookupStatus $ GenFits propId invId
  pub <- send $ TaskLookupStatus $ PublishTask propId invId
  appLayout Inversions $ do
    col Style.page $ do
      col (gap 5) $ do
        el Style.header $ do
          text "Inversion - "
          text invId.fromId

        el_ $ do
          text "Program - "
          route (Route.Proposal inv.proposalId $ Program inv.programId Prog) Style.link $ do
            text inv.programId.fromId

        el_ $ do
          text "Proposal - "
          route (Route.Proposal inv.proposalId PropRoot) Style.link $ do
            text inv.proposalId.fromId

      hyper (InversionStatus inv.proposalId inv.programId inv.inversionId) $ viewInversion inv admin up gen pub


submitUpload
  :: forall es
   . (Hyperbole :> es, Log :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es, Scratch :> es)
  => Id Proposal
  -> Id Inversion
  -> Eff es Response
submitUpload propId invId = do
  tfrm <- formData @TransferForm
  tup <- formData @(UploadFiles Filename)
  taskId <- requireLogin $ Globus.initUpload tfrm tup propId invId
  saveActiveTransfer invId taskId
  redirect $ routeUrl (Route.Proposal propId $ Route.Inversion invId Inv)


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

type InversionViews = '[UploadTransfer, InversionCommit, GenerateStep, GenerateTransfer, PublishStep]


data InversionStatus = InversionStatus (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Globus :> es, Auth :> es, Tasks GenFits :> es, Time :> es, Scratch :> es, Tasks PublishTask :> es) => HyperView InversionStatus es where
  data Action InversionStatus
    = Upload
    | Reload
    deriving (Show, Read, ViewAction)
  type Require InversionStatus = InversionViews


  update action = do
    InversionStatus propId _ invId <- viewId
    case action of
      Upload -> do
        r <- request
        requireLogin $ do
          redirect $ Globus.fileManagerSelectUrl (Files 4) (Route.Proposal propId $ Route.Inversion invId SubmitUpload) ("Transfer Inversion Results " <> invId.fromId) r
      Reload -> do
        refresh
   where
    refresh = do
      InversionStatus ip _ ii <- viewId
      mtok <- send AdminToken
      login <- send LoginUrl
      inv <- loadInversion ii
      up <- activeTransfer ii
      gen <- send $ TaskLookupStatus $ GenFits ip ii
      pub <- send $ TaskLookupStatus $ PublishTask ip ii
      pure $ viewInversion inv (AdminLogin mtok login) up gen pub


viewInversionContainer :: Inversion -> View c () -> View c ()
viewInversionContainer inv cnt =
  col (Style.card . gap 15) $ do
    el (Style.cardHeader (inversionStepColor inv)) "Inversion"
    col (gap 0 . pad 15) $ do
      cnt


viewInversion :: Inversion -> AdminLogin -> Maybe (Id Task) -> Maybe GenFitsStatus -> Maybe PublishStatus -> View InversionStatus ()
viewInversion inv admin up gen pub = do
  col (gap 10) $ do
    viewInversionContainer inv $ do
      datasetStep inv $ do
        viewDatasets inv

      invertStep inv $ do
        viewInvert inv up

      generateStep inv $ do
        hyper (GenerateStep inv.proposalId inv.programId inv.inversionId) $
          viewGenerate inv admin gen

      publishStep inv $ do
        hyper (PublishStep inv.proposalId inv.programId inv.inversionId) $
          viewPublish inv pub


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
  StepDone -> line Success
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
      StepDone -> Icons.check
      StepError -> "!"
      _ -> text $ cs $ show num


stepColor :: Step -> AppColor
stepColor = \case
  StepActive -> Info
  StepNext -> Secondary
  StepDone -> Success
  StepError -> Danger


datasetStep :: Inversion -> View c () -> View c ()
datasetStep _inv =
  viewStep StepActive 1 "Datasets"


viewDatasets :: Inversion -> View InversionStatus ()
-- viewDatasets _ StepDownloadNone = do
--   el_ "Please select a destination folder for the instrument program's datasets. You will be redirected to Globus."
--   row (gap 10) $ do
--     button Download (Style.btn Primary . grow) "Choose Folder"
-- -- button Cancel (Style.btnOutline Secondary) "Cancel"
-- viewDownload inv (StepDownloading dwn) = do
--   hyper (DownloadTransfer inv.proposalId inv.programId inv.inversionId dwn.transfer) (Transfer.viewLoadTransfer DwnTransfer)
-- viewDownload _ (StepDownloaded _) = do
--   row (gap 10) $ do
--     button Download (Style.btnOutline Secondary . grow) "Download Again"
viewDatasets _inv = el_ "Datasets here"


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
    UploadTransfer ip iip invId ti <- viewId
    case action of
      TaskFailed -> do
        pure $ do
          col (gap 10) $ do
            Transfer.viewTransferFailed ti
            target (InversionStatus ip iip invId) $ do
              uploadSelect (Invalid "Upload Task Failed")
      TaskSucceeded -> do
        Inversions.setUploaded invId
        -- reload the parent
        pure $ target (InversionStatus ip iip invId) $ do
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
  | StepDone
  | StepError


invertStep :: Inversion -> View c () -> View c ()
invertStep inv =
  viewStep status 2 "Invert"
 where
  status
    | isInverted inv = StepDone
    | otherwise = StepActive


viewInvert :: Inversion -> Maybe (Id Task) -> View InversionStatus ()
viewInvert inv mtfer = do
  col (gap 10) $ do
    if isInverted inv
      then viewComplete
      else viewInverting
 where
  viewInverting = do
    hyper (InversionCommit inv.proposalId inv.inversionId) $ do
      CommitForm.commitForm inv.invert.commit (CommitForm.fromExistingCommit inv.invert.commit)

    viewUploadTransfer mtfer
  viewComplete = do
    let mc = inv.invert.commit
    -- let mt = Just inverted.transfer
    hyper (InversionCommit inv.proposalId inv.inversionId) $ do
      CommitForm.commitForm mc (CommitForm.fromExistingCommit mc)
    col (gap 5) $ do
      el_ "Upload Inversion Results"
      button Upload (Style.btnOutline Success . grow) "Select New Files"

  viewUploadTransfer = \case
    Just it -> do
      hyper (UploadTransfer inv.proposalId inv.programId inv.inversionId it) (Transfer.viewLoadTransfer UpTransfer)
    Nothing -> do
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

data GenerateStep = GenerateStep (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Tasks GenFits :> es, Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es) => HyperView GenerateStep es where
  data Action GenerateStep
    = ReloadGen
    | RegenError
    | RegenFits
    | RegenAsdf
    deriving (Show, Read, ViewAction)


  type Require GenerateStep = '[GenerateTransfer, InversionStatus]


  update action = do
    GenerateStep _ _ ii <- viewId
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
      GenerateStep _ _ invId <- viewId
      inv <- loadInversion invId
      if isGenerated inv
        then refreshInversion
        else loadGenerate

    loadGenerate = do
      GenerateStep propId _ invId <- viewId
      inv <- loadInversion invId
      status <- send $ TaskLookupStatus $ GenFits propId invId
      mtok <- send AdminToken
      login <- send LoginUrl
      pure $ do
        viewGenerate inv (AdminLogin mtok login) status

    refreshInversion = do
      GenerateStep ip iip ii <- viewId
      pure $ target (InversionStatus ip iip ii) $ el (onLoad Reload 0) "RELOAD?"


generateStep :: Inversion -> View c () -> View c ()
generateStep inv =
  viewStep status 3 "Generate"
 where
  status
    | isGenerated inv = StepDone
    | isInverted inv = StepActive
    | otherwise = StepNext


viewGenerate :: Inversion -> AdminLogin -> Maybe GenFitsStatus -> View GenerateStep ()
viewGenerate inv admin status =
  col (gap 10) viewGen
 where
  viewGen =
    case (inv.invError, inv.generate.fits, inv.generate.asdf) of
      (Just e, _, _) -> viewGenError e
      (_, Just f, Just a) -> viewGenComplete f a
      (_, Just f, _) -> viewGenAsdf f
      (_, _, _) -> viewGenerateStep status

  viewGenError e = do
    row truncate $ View.systemError $ cs e
    button RegenError (Style.btn Primary) "Restart"

  viewGenComplete :: UTCTime -> UTCTime -> View GenerateStep ()
  viewGenComplete _fits _asdf = do
    row (gap 10) $ do
      viewGeneratedFiles inv
      button RegenFits (Style.btnOutline Secondary) "Regen FITS"
      button RegenAsdf (Style.btnOutline Secondary) "Regen ASDF"

  viewGenAsdf :: UTCTime -> View GenerateStep ()
  viewGenAsdf _fits = do
    loadingMessage "Generating ASDF"
    el (onLoad ReloadGen 1000) none

  viewGenerateStep Nothing = none
  viewGenerateStep (Just gen) =
    case gen of
      GenStarted -> el_ "..."
      GenWaiting -> do
        row (onLoad ReloadGen 1000) $ do
          loadingMessage "Waiting for job to start"
          space
          case admin.token of
            Nothing -> link admin.loginUrl (Style.btnOutline Danger) "Needs Globus Login"
            Just _ -> pure ()
      GenTransferring taskId -> do
        el_ "Generating FITS - Transferring L1 Files"
        hyper (GenerateTransfer inv.proposalId inv.programId inv.inversionId taskId) $ do
          Transfer.viewLoadTransfer GenTransfer
      GenFrames _ _ -> do
        loadingMessage "Generating FITS"
        col (onLoad ReloadGen 1000) $ do
          row (gap 5) $ do
            space
            el_ $ text $ cs $ show gen.complete
            el_ " / "
            el_ $ text $ cs $ show gen.total
          View.progress (fromIntegral gen.complete / fromIntegral gen.total)

  loadingMessage msg =
    row (gap 5) $ do
      el (width 20) Icons.spinnerCircle
      col id $ do
        space
        el_ $ text msg
        space


-- GenerateTransfer ---------------------------------------------

data GenerateTransfer = GenerateTransfer (Id Proposal) (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Show, Read, ViewId)


instance (Tasks GenFits :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es) => HyperView GenerateTransfer es where
  type Require GenerateTransfer = '[GenerateStep]
  data Action GenerateTransfer
    = GenTransfer TransferAction
    deriving (Show, Read, ViewAction)


  update (GenTransfer action) = do
    GenerateTransfer ip iip ii ti <- viewId
    case action of
      TaskFailed -> do
        pure $ do
          Transfer.viewTransferFailed ti
          target (GenerateStep ip iip ii) $ do
            button RegenFits (Style.btn Primary) "Restart Transfer"
      TaskSucceeded ->
        pure $ do
          target (GenerateStep ip iip ii) $ do
            el (onLoad ReloadGen 1000) "SUCCEEDED"
      CheckTransfer -> do
        Transfer.checkTransfer GenTransfer ti


viewGeneratedFiles :: Inversion -> View c ()
viewGeneratedFiles inv =
  link (Globus.fileManagerOpenInv $ Scratch.outputL2Dir inv.proposalId inv.inversionId) (Style.btnOutline Success . grow . att "target" "_blank") "View Generated Files"


-- ----------------------------------------------------------------
-- STEP PUBLISH
-- ----------------------------------------------------------------

publishStep :: Inversion -> View c () -> View c ()
publishStep inv =
  viewStep' status 4 "Publish" none
 where
  status
    | isPublished inv = StepDone
    | isGenerated inv = StepActive
    | otherwise = StepNext


data PublishStep = PublishStep (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Globus :> es, Auth :> es, IOE :> es, Scratch :> es, Time :> es, Tasks PublishTask :> es) => HyperView PublishStep es where
  type Require PublishStep = '[InversionStatus]


  data Action PublishStep
    = StartSoftPublish
    | CheckPublish
    | PublishTransfer (Id Task) TransferAction
    deriving (Show, Read, ViewAction)


  update action = do
    PublishStep propId _ invId <- viewId
    case action of
      StartSoftPublish -> do
        requireLogin $ do
          Publish.startSoftPublish propId invId
          pure viewPublishWait
      CheckPublish -> do
        status <- send $ TaskGetStatus $ PublishTask propId invId
        pure $ do
          case status of
            PublishWaiting -> viewPublishWait
            PublishTransferring taskId -> viewPublishTransfer taskId
      PublishTransfer taskId TaskFailed -> do
        pure $ do
          Transfer.viewTransferFailed taskId
          button StartSoftPublish (Style.btn Primary . grow) "Restart Transfer"
      PublishTransfer _ TaskSucceeded -> do
        refreshInversion
      PublishTransfer taskId CheckTransfer -> do
        Transfer.checkTransfer (PublishTransfer taskId) taskId
   where
    refreshInversion = do
      PublishStep propId progId invId <- viewId
      pure $ target (InversionStatus propId progId invId) $ do
        el (onLoad Reload 0) none


-- what if it is actively being published?
viewPublish :: Inversion -> Maybe PublishStatus -> View PublishStep ()
viewPublish inv mstatus
  | isPublished inv = viewPublished inv.proposalId inv.inversionId
  | isInverted inv = viewCheckStatus
  | otherwise = none
 where
  viewNeedsPublish =
    button StartSoftPublish (Style.btn Primary . grow) "Soft Publish"

  viewCheckStatus = do
    case mstatus of
      Just _ -> viewPublishWait
      Nothing -> viewNeedsPublish


viewPublishWait :: View PublishStep ()
viewPublishWait = do
  el (onLoad CheckPublish 1000) "Publishing..."


viewPublishTransfer :: Id Task -> View PublishStep ()
viewPublishTransfer taskId = do
  el_ "Publishing..."
  Transfer.viewLoadTransfer (PublishTransfer taskId)


viewPublished :: Id Proposal -> Id Inversion -> View PublishStep ()
viewPublished propId invId = do
  link (Publish.fileManagerOpenPublish $ Publish.publishedDir propId invId) (Style.btnOutline Success . grow . att "target" "_blank") "View Published Files"

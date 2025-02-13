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
import App.Route as Route
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons qualified as Icons
import App.View.Inversion (viewInversionContainer)
import App.View.Layout
import App.View.LiveInput (liveTextArea)
import App.View.Transfer (TransferAction (..), activeTransfer)
import App.View.Transfer qualified as Transfer
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
  -> Eff es (Page (InversionStatus : MoreInversions : InversionViews))
page propId invId = do
  inv <- loadInversion invId
  ds <- Datasets.find (Datasets.ByProgram inv.programId)
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

      hyper (InversionStatus inv.proposalId inv.programId inv.inversionId) $ viewInversion inv ds admin up gen pub
      hyper (MoreInversions inv.proposalId inv.programId) viewMoreInversions


loadInversion :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> Eff es Inversion
loadInversion invId = do
  (inv :| _) <- send (Inversions.ById invId) >>= expectFound
  pure inv


redirectHome :: (Hyperbole :> es) => Eff es (View InversionStatus ())
redirectHome = do
  redirect $ pathUrl . routePath $ Inversions


-------------------------------------------------------------------
--- INVERSIONS
-------------------------------------------------------------------

data MoreInversions = MoreInversions (Id Proposal) (Id InstrumentProgram)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Globus :> es, Auth :> es, Tasks GenFits :> es) => HyperView MoreInversions es where
  data Action MoreInversions
    = CreateInversion
    deriving (Show, Read, ViewAction)


  update = \case
    CreateInversion -> do
      MoreInversions propId progId <- viewId
      redirect $ routeUrl $ Route.Proposal propId $ Program progId Prog


viewMoreInversions :: View MoreInversions ()
viewMoreInversions = do
  button CreateInversion (Style.btnOutline Primary) "Start Over With New Inversion"


-------------------------------------------------------------------
--- INVERSION STATUS
-------------------------------------------------------------------

type InversionViews = '[InversionCommit, GenerateStep, GenerateTransfer, PublishStep, InversionMeta]


data InversionStatus = InversionStatus (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Datasets :> es, Globus :> es, Auth :> es, Tasks GenFits :> es, Time :> es, Scratch :> es, Tasks PublishTask :> es) => HyperView InversionStatus es where
  data Action InversionStatus
    = Reload
    | SetDataset (Id Dataset) Bool
    deriving (Show, Read, ViewAction)


  type Require InversionStatus = InversionViews


  update action = do
    InversionStatus _ _ invId <- viewId
    case action of
      Reload -> do
        refresh
      SetDataset dsetId sel -> do
        inv <- loadInversion invId
        Inversions.setDatasetUsed inv dsetId sel
        refresh
   where
    refresh = do
      InversionStatus propId progId invId <- viewId
      ds <- Datasets.find (Datasets.ByProgram progId)
      mtok <- send AdminToken
      login <- send LoginUrl
      inv <- loadInversion invId
      up <- activeTransfer invId
      gen <- send $ TaskLookupStatus $ GenFits propId invId
      pub <- send $ TaskLookupStatus $ PublishTask propId invId
      pure $ viewInversion inv ds (AdminLogin mtok login) up gen pub


viewInversion :: Inversion -> [Dataset] -> AdminLogin -> Maybe (Id Task) -> Maybe GenFitsStatus -> Maybe PublishStatus -> View InversionStatus ()
viewInversion inv ds admin up gen pub = do
  col (gap 10) $ do
    viewInversionContainer inv $ do
      invertStep inv $ do
        viewInvert inv ds up

      generateStep inv $ do
        hyper (GenerateStep inv.proposalId inv.programId inv.inversionId) $
          viewGenerate inv admin gen

      publishStep inv $ do
        hyper (PublishStep inv.proposalId inv.programId inv.inversionId) $
          viewPublish inv pub

      hyper (InversionMeta inv.proposalId inv.programId inv.inversionId) $ viewInversionMeta inv


-------------------------------------------------------------------
--- INVERSION META
-------------------------------------------------------------------

data InversionMeta = InversionMeta (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Inversions :> es) => HyperView InversionMeta es where
  data Action InversionMeta
    = AreYouSure
    | Delete
    | SetNotes Text
    | Cancel
    deriving (Show, Read, ViewAction)


  update action = do
    InversionMeta propId progId invId <- viewId
    case action of
      AreYouSure -> do
        inv <- loadInversion invId
        pure $ viewInversionMeta' inv viewAreYouSure
      Delete -> do
        send $ Inversions.Remove invId
        redirect $ routeUrl $ Route.Proposal propId $ Route.Program progId Route.Prog
      SetNotes notes -> do
        Inversions.setNotes invId notes
        inv <- loadInversion invId
        pure $ viewInversionMeta' inv viewDeleteBtn
      Cancel -> do
        inv <- loadInversion invId
        pure $ viewInversionMeta inv


viewInversionMeta :: Inversion -> View InversionMeta ()
viewInversionMeta inv = do
  viewInversionMeta' inv $ do
    if isInverted inv
      then viewCannotDelete
      else viewDeleteBtn


viewInversionMeta' :: Inversion -> View InversionMeta () -> View InversionMeta ()
viewInversionMeta' inv delContent = do
  col (gap 20) $ do
    col (gap 5) $ do
      el bold "Notes"
      liveTextArea SetNotes (att "rows" "3") inv.notes
    delContent


viewDeleteBtn :: View InversionMeta ()
viewDeleteBtn = do
  button AreYouSure (Style.btnOutline Danger) "Delete Inversion"


viewCannotDelete :: View InversionMeta ()
viewCannotDelete = do
  el (italic . color Secondary) "This Inversion has uploaded results, it can no longer be deleted. Please create new one instead"


viewAreYouSure :: View InversionMeta ()
viewAreYouSure = do
  col (gap 15) $ do
    el (bold . color Danger) "Are you sure? This cannot be undone"
    row (gap 10) $ do
      button Cancel (Style.btnOutline Secondary . grow) "Cancel"
      button Delete (Style.btn Danger) "Delete This Inverion"


-------------------------------------------------------------------
-- STEP INVERT
-------------------------------------------------------------------

-- data UploadTransfer = UploadTransfer (Id Proposal) (Id InstrumentProgram) (Id Inversion) (Id Task)
--   deriving (Show, Read, ViewId)
--
--
-- instance (Inversions :> es, Globus :> es, Auth :> es, Tasks GenFits :> es, Time :> es, Log :> es) => HyperView UploadTransfer es where
--   data Action UploadTransfer
--     = UpTransfer TransferAction
--     deriving (Show, Read, ViewAction)
--   type Require UploadTransfer = '[InversionStatus]
--
--
--   update (UpTransfer action) = do
--     UploadTransfer propId progId invId ti <- viewId
--     case action of
--       TaskFailed -> do
--         pure $ do
--           col (gap 10) $ do
--             Transfer.viewTransferFailed ti
--             target (InversionStatus propId progId invId) $ do
--               uploadSelect (Invalid "Upload Task Failed")
--       TaskSucceeded -> do
--         Inversions.setUploaded invId
--         -- reload the parent
--         pure $ target (InversionStatus propId progId invId) $ do
--           el (onLoad Reload 200) $ do
--             uploadSelect Valid
--       CheckTransfer -> Transfer.checkTransfer UpTransfer ti

-- invertReload :: (Hyperbole :> es, Inversions :> es, Globus :> es, Tasks GenFits :> es) => Id Proposal -> Id Inversion -> View id () -> Eff es (View id ())
-- invertReload propId invId vw = do
--   inv <- loadInversion invId
--   pure $ target (InversionStatus inv.proposalId inv.inversionId) $ onLoad Reload 0 none

-- -- | Check to see if we are have all the inversion fields filled out and need to reload
-- checkInvertReload :: (HyperViewHandled InversionStatus id, Hyperbole :> es, Inversions :> es, Globus :> es, Tasks GenFits :> es) => Id Proposal -> Id Inversion -> View id () -> Eff es (View id ())
-- checkInvertReload propId invId vw = do
--   inv <- loadInversion invId
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
  viewStep status 1 "Invert"
 where
  status
    | isInverted inv = StepDone
    | otherwise = StepActive


viewDatasets :: Inversion -> [Dataset] -> View InversionStatus ()
viewDatasets inv ds = do
  col (gap 5) $ do
    el bold "Datasets Used"
    forM_ ds $ \d -> do
      row (gap 10) $ do
        View.checkBtn (SetDataset d.datasetId) (d.datasetId `elem` inv.datasets)
        route (Route.Datasets $ Dataset d.datasetId) Style.link $ text d.datasetId.fromId


viewInvert :: Inversion -> [Dataset] -> Maybe (Id Task) -> View InversionStatus ()
viewInvert inv ds _mtfer = do
  col (gap 15) $ do
    viewDatasets inv ds

    hyper (InversionCommit inv.proposalId inv.inversionId) $ do
      CommitForm.commitForm inv.invert.commit (CommitForm.fromExistingCommit inv.invert.commit)


-- viewUploadTransfer mtfer
-- where
--  viewUploadTransfer = \case
--    Just it -> do
--      hyper (UploadTransfer inv.proposalId inv.programId inv.inversionId it) (Transfer.viewLoadTransfer UpTransfer)
--    Nothing -> do
--      uploadSelect NotInvalid

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
    GenerateStep _ _ invId <- viewId
    case action of
      ReloadGen ->
        refresh
      RegenError -> do
        Inversions.clearError invId
        refreshInversion
      RegenFits -> do
        Inversions.resetGenerating invId
        refreshInversion
      RegenAsdf -> do
        Inversions.resetGeneratingAsdf invId
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
      GenerateStep propId progId invId <- viewId
      pure $ target (InversionStatus propId progId invId) $ el (onLoad Reload 0) "RELOAD?"


generateStep :: Inversion -> View c () -> View c ()
generateStep inv =
  viewStep status 2 "Generate"
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


instance (Tasks GenFits :> es, Inversions :> es, Globus :> es, Auth :> es, Datasets :> es, Log :> es) => HyperView GenerateTransfer es where
  type Require GenerateTransfer = '[GenerateStep]
  data Action GenerateTransfer
    = GenTransfer TransferAction
    deriving (Show, Read, ViewAction)


  update (GenTransfer action) = do
    GenerateTransfer propId progId invId taskId <- viewId
    case action of
      TaskFailed -> do
        pure $ do
          Transfer.viewTransferFailed taskId
          target (GenerateStep propId progId invId) $ do
            button RegenFits (Style.btn Primary) "Restart Transfer"
      TaskSucceeded ->
        pure $ do
          target (GenerateStep propId progId invId) $ do
            el (onLoad ReloadGen 1000) "SUCCEEDED"
      CheckTransfer -> do
        Transfer.checkTransfer GenTransfer taskId


viewGeneratedFiles :: Inversion -> View c ()
viewGeneratedFiles inv =
  link (Globus.fileManagerOpenInv $ Scratch.outputL2Dir inv.proposalId inv.inversionId) (Style.btnOutline Success . grow . att "target" "_blank") "View Generated Files"


-- ----------------------------------------------------------------
-- STEP PUBLISH
-- ----------------------------------------------------------------

publishStep :: Inversion -> View c () -> View c ()
publishStep inv =
  viewStep' status 3 "Publish" none
 where
  status
    | isPublished inv = StepDone
    | isGenerated inv = StepActive
    | otherwise = StepNext


data PublishStep = PublishStep (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Globus :> es, Auth :> es, IOE :> es, Scratch :> es, Time :> es, Tasks PublishTask :> es, Log :> es) => HyperView PublishStep es where
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


-------------------------------------------------------------------
-- VERTICAL STEP INDICATORS
-------------------------------------------------------------------

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

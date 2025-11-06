{-# LANGUAGE UndecidableInstances #-}

module App.Page.Inversion where

import App.Colors
import App.Effect.Auth
import App.Effect.FileManager qualified as FileManager
import App.Effect.Transfer (Transfer (..))
import App.Error (expectFound)
import App.Page.Inversions.CommitForm as CommitForm
import App.Route as Route
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons qualified as Icons
import App.View.Inversion (Step (..), stepGenerate, stepMetadata, stepPublish, stepUpload, viewInversionContainer)
import App.View.Layout
import App.View.Loading (inputLoader)
import App.View.ProposalDetails (ionTag)
import App.View.Transfer (TransferAction (..))
import App.View.Transfer qualified as Transfer
import App.Worker.Generate as Gen (GenStatus (..), GenTask (..))
import App.Worker.Publish as Publish
import Data.Text qualified as T
import Effectful
import Effectful.Debug (Debug, delay)
import Effectful.Dispatch.Dynamic
import Effectful.Globus (Task)
import Effectful.Log hiding (Info)
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Spectra qualified as Spectra
import NSO.Files (Publish)
import NSO.Files.DKIST qualified as DKIST
import NSO.Files.Image qualified as Files
import NSO.Files.RemoteFolder (Remote)
import NSO.Files.Scratch (Scratch (..))
import NSO.Image.Fits.Frame qualified as Fits
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength (SpectralLine (..))
import Numeric (showFFloat)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI as URI (pathUri)


page
  :: (Hyperbole :> es, Transfer :> es, Auth :> es, Log :> es, Inversions :> es, Datasets :> es, Auth :> es, Tasks GenTask :> es, Time :> es, Tasks PublishTask :> es)
  => Id Proposal
  -> Id Inversion
  -> Page es (InversionStatus : MoreInversions : InversionViews)
page propId invId = do
  inv <- loadInversion invId
  ds <- loadDatasets inv.programId
  gen <- send $ TaskLookupStatus $ GenTask propId invId
  pub <- send $ TaskLookupStatus $ PublishTask propId invId
  scratch <- send RemoteScratch
  publish <- send RemotePublish
  appLayout Inversions $ do
    col ~ Style.page $ do
      col ~ gap 5 $ do
        el ~ Style.header $ do
          text "Inversion - "
          text invId.fromId

        el $ do
          text "Program - "
          appRoute (Route.Proposal inv.proposalId $ Program inv.programId Prog) ~ Style.link $ do
            text inv.programId.fromId

        el $ do
          text "Proposal - "
          appRoute (Route.Proposal inv.proposalId PropRoot) ~ Style.link $ do
            text inv.proposalId.fromId

      hyper (InversionStatus inv.proposalId inv.programId inv.inversionId) $ viewInversion publish scratch inv ds gen pub
      hyper (MoreInversions inv.proposalId inv.programId) viewMoreInversions


loadInversion :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> Eff es Inversion
loadInversion invId = do
  (inv :| _) <- send (Inversions.ById invId) >>= expectFound
  pure inv


loadDatasets :: (Hyperbole :> es, Datasets :> es) => Id InstrumentProgram -> Eff es (NonEmpty Dataset)
loadDatasets progId = do
  Datasets.find (Datasets.ByProgram progId) >>= expectFound


redirectHome :: (Hyperbole :> es) => Eff es (View InversionStatus ())
redirectHome = do
  redirect $ pathUri $ routePath Inversions


-------------------------------------------------------------------
--- INVERSIONS
-------------------------------------------------------------------

data MoreInversions = MoreInversions (Id Proposal) (Id InstrumentProgram)
  deriving (Generic, ViewId)


instance (Inversions :> es, Auth :> es, Tasks GenTask :> es) => HyperView MoreInversions es where
  data Action MoreInversions
    = CreateInversion
    deriving (Generic, ViewAction)


  update = \case
    CreateInversion -> do
      MoreInversions propId progId <- viewId
      redirect $ routeUri $ Route.Proposal propId $ Program progId Prog


viewMoreInversions :: View MoreInversions ()
viewMoreInversions = do
  button CreateInversion ~ Style.btnOutline Primary $ "Start Over With New Inversion"


-------------------------------------------------------------------
--- INVERSION STATUS
-------------------------------------------------------------------

type InversionViews = '[Metadata, GenerateStep, GenerateTransfer, PublishStep, InversionMeta]


data InversionStatus = InversionStatus (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Generic, ViewId)


data Generated = Generated
  { genFits :: UTCTime
  , genAsdf :: UTCTime
  , genTransfer :: UTCTime
  }


generated :: Inversion -> Maybe Generated
generated inv = do
  genFits <- inv.generate.fits
  genAsdf <- inv.generate.asdf
  genTransfer <- inv.generate.transfer
  pure Generated{genFits, genAsdf, genTransfer}


instance (Inversions :> es, Transfer :> es, Datasets :> es, Auth :> es, Tasks GenTask :> es, Time :> es, Scratch :> es, Tasks PublishTask :> es) => HyperView InversionStatus es where
  data Action InversionStatus
    = Reload
    | SetDataset (Id Dataset) Bool
    | Restore
    deriving (Generic, ViewAction)


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
      Restore -> do
        send $ Inversions.Deleted invId False
        refresh
   where
    refresh = do
      InversionStatus propId progId invId <- viewId
      ds <- loadDatasets progId
      inv <- loadInversion invId
      publish <- send RemotePublish
      scratch <- send RemoteScratch
      gen <- send $ TaskLookupStatus $ GenTask propId invId
      pub <- send $ TaskLookupStatus $ PublishTask propId invId
      pure $ viewInversion publish scratch inv ds gen pub


viewInversion :: Remote Publish -> Remote Scratch -> Inversion -> NonEmpty Dataset -> Maybe GenStatus -> Maybe PublishStatus -> View InversionStatus ()
viewInversion publish scratch inv ds gen pub = do
  col ~ gap 30 $ do
    if inv.deleted
      then restoreButton
      else none
    col ~ disableIfDeleted $ do
      viewInversionContainer inv $ do
        stepUpload (uploadStep inv) $ do
          viewUpload inv

        stepMetadata (metadataStep inv) $ do
          viewMetadata inv ds

        stepGenerate (generateStep inv) $ do
          hyper (GenerateStep inv.proposalId inv.programId inv.inversionId) $
            viewGenerate scratch inv gen

        stepPublish (publishStep inv) $ do
          hyper (PublishStep (head ds).bucket inv.proposalId inv.programId inv.inversionId) $
            viewPublish publish (head ds).bucket inv pub

        hyper (InversionMeta inv.proposalId inv.programId inv.inversionId) $ viewInversionMeta inv
 where
  disableIfDeleted = if inv.deleted then Style.disabled else id
  restoreButton =
    col ~ gap 10 $ do
      el ~ color Danger . italic $ "Inversion has been archived"
      button Restore ~ Style.btn Primary $ "Restore"


uploadStep :: Inversion -> Step
uploadStep _ = StepDone


viewUpload :: Inversion -> View c ()
viewUpload _ = none


-------------------------------------------------------------------
--- INVERSION META
-------------------------------------------------------------------

data InversionMeta = InversionMeta (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Generic, ViewId)


instance (Inversions :> es, Debug :> es) => HyperView InversionMeta es where
  data Action InversionMeta
    = SetNotes Text
    | Delete
    deriving (Generic, ViewAction)


  type Require InversionMeta = '[InversionStatus]


  update action = do
    InversionMeta propId progId invId <- viewId
    case action of
      SetNotes notes -> do
        delay 1000
        Inversions.setNotes invId notes
        inv <- loadInversion invId
        pure $ viewInversionMeta inv
      Delete -> do
        send $ Inversions.Deleted invId True
        pure $ target (InversionStatus propId progId invId) $ do
          el @ onLoad Reload 100 $ ""


viewInversionMeta :: Inversion -> View InversionMeta ()
viewInversionMeta inv =
  viewInversionMeta' inv deleteContents
 where
  deleteContents
    | inv.deleted = none
    | isPublished inv = viewCannotDelete
    | otherwise = viewDeleteBtn


viewInversionMeta' :: Inversion -> View InversionMeta () -> View InversionMeta ()
viewInversionMeta' inv delContent = do
  col ~ gap 20 $ do
    col ~ gap 5 $ do
      el ~ bold $ "Notes"
      inputLoader $ do
        tag "textarea" @ onInput SetNotes 500 . att "rows" "3" ~ Style.input $ text inv.notes
    delContent


viewDeleteBtn :: View InversionMeta ()
viewDeleteBtn = do
  button Delete ~ Style.btnOutline Danger $ "Archive Inversion"


viewCannotDelete :: View InversionMeta ()
viewCannotDelete = do
  el ~ italic . color Secondary $ "This Inversion has been published, it can no longer be deleted. Please create new one instead"


--
-- viewAreYouSure :: View InversionMeta ()
-- viewAreYouSure = do
--   col (gap 15) $ do
--     el (bold . color Danger) "Are you sure? This cannot be undone"
--     row (gap 10) $ do
--       button DeleteCancel (Style.btnOutline Secondary . grow) "Cancel"
--       button Delete (Style.btn Danger) "Delete This Inverion"

-------------------------------------------------------------------
-- STEP INVERT
-------------------------------------------------------------------

data Metadata = Metadata (Id Proposal) (Id Inversion)
  deriving (Generic, ViewId)


instance (Log :> es, Inversions :> es, Time :> es) => HyperView Metadata es where
  data Action Metadata
    = SaveCommit GitCommit
    deriving (Generic, ViewAction)


  update (SaveCommit commit) = do
    Metadata _ invId <- viewId
    log Debug $ dump "SaveCommit" commit

    isValid <- CommitForm.validate commit
    if isValid
      then do
        Inversions.setSoftwareCommit invId commit
        pure $ commitForm SaveCommit (Just commit) Valid
      else do
        pure $ commitForm SaveCommit (Just commit) invalidCommit


-- invertReload :: (Hyperbole :> es, Inversions :> es, Globus :> es, Tasks GenTask :> es) => Id Proposal -> Id Inversion -> View id () -> Eff es (View id ())
-- invertReload propId invId vw = do
--   inv <- loadInversion invId
--   pure $ target (InversionStatus inv.proposalId inv.inversionId) $ onLoad Reload 0 none

-- -- | Check to see if we are have all the inversion fields filled out and need to reload
-- checkInvertReload :: (HyperViewHandled InversionStatus id, Hyperbole :> es, Inversions :> es, Globus :> es, Tasks GenTask :> es) => Id Proposal -> Id Inversion -> View id () -> Eff es (View id ())
-- checkInvertReload propId invId vw = do
--   inv <- loadInversion invId
--   pure $ case inv.step of
--     StepGenerate _ -> hyper (InversionStatus inv.proposalId inv.programId inv.inversionId) $ onLoad Reload 0 none
--     _ -> vw

metadataStep :: Inversion -> Step
metadataStep inv
  | isInverted inv = StepDone
  | otherwise = StepActive


viewDatasets :: Inversion -> NonEmpty Dataset -> View InversionStatus ()
viewDatasets inv ds = do
  col ~ gap 5 $ do
    el ~ bold $ "Datasets Used"
    forM_ ds $ \d -> do
      row ~ gap 10 $ do
        View.checkBtn (SetDataset d.datasetId) (d.datasetId `elem` inv.datasets)
        appRoute (Route.Datasets $ Dataset d.datasetId) ~ Style.link $ text d.datasetId.fromId
        -- maybe none (\l -> text $ "(" <> cs (show l) <> ")") $ Spectra.identifyLine d
        el ~ fontSize 12 $ maybe none ionTag $ listToMaybe $ fmap (.ion) d.spectralLines


viewMetadata :: Inversion -> NonEmpty Dataset -> View InversionStatus ()
viewMetadata inv ds = do
  col ~ gap 15 $ do
    viewDatasets inv ds

    hyper (Metadata inv.proposalId inv.inversionId) $ do
      CommitForm.commitForm SaveCommit inv.invert.commit (CommitForm.existingCommit inv.invert.commit)


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
  deriving (Generic, ViewId)


instance (Transfer :> es, Tasks GenTask :> es, Hyperbole :> es, Inversions :> es, Auth :> es, Datasets :> es, Scratch :> es, Time :> es) => HyperView GenerateStep es where
  data Action GenerateStep
    = ReloadGen
    | RegenError
    | RegenFits
    | RegenAsdf
    deriving (Generic, ViewAction)


  type Require GenerateStep = '[GenerateTransfer, InversionStatus]


  update action = do
    GenerateStep propId _ invId <- viewId
    case action of
      ReloadGen ->
        refresh
      RegenError -> do
        Inversions.clearError invId
        refreshInversion
      RegenFits -> do
        Fits.deleteL2FramesFits propId invId
        Inversions.resetGeneratingFits invId
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
      scratch <- send RemoteScratch
      status <- send $ TaskLookupStatus $ GenTask propId invId
      pure $ do
        viewGenerate scratch inv status

    refreshInversion = do
      GenerateStep propId progId invId <- viewId
      pure $ target (InversionStatus propId progId invId) $ el @ onLoad Reload 0 $ "RELOAD?"


generateStep :: Inversion -> Step
generateStep inv
  | isGenerated inv = StepDone
  | isInverted inv = StepActive
  | otherwise = StepNext


viewGenerate :: Remote Scratch -> Inversion -> Maybe GenStatus -> View GenerateStep ()
viewGenerate scratch inv status
  | inv.deleted = none
  | isInverted inv = viewGenerate' scratch inv status
  | otherwise = none


viewGenerate' :: Remote Scratch -> Inversion -> Maybe GenStatus -> View GenerateStep ()
viewGenerate' scratch inv status =
  col ~ gap 10 $ viewGen
 where
  viewGen =
    case generated inv of
      Just gen -> viewGenComplete gen
      Nothing -> maybe (viewGenerateStep status) viewGenError inv.invError

  viewGenError e = do
    col ~ gap 15 $ do
      row $ View.systemError $ cs e
      row ~ gap 10 $ do
        button RegenError ~ Style.btn Primary $ "Retry"
        button RegenFits ~ Style.btnOutline Secondary $ "Start Over"
        when ("GlobusError" `T.isPrefixOf` e) $ do
          route Logout ~ Style.btnOutline Secondary $ "Reauthenticate"

  viewGenComplete :: Generated -> View GenerateStep ()
  viewGenComplete _generated = do
    row ~ gap 10 $ do
      viewGeneratedFiles scratch inv
      button RegenFits ~ Style.btnOutline Secondary $ "Regen FITS"
      button RegenAsdf ~ Style.btnOutline Secondary $ "Regen ASDF"

  viewGenerateStep Nothing =
    row @ onLoad ReloadGen 1000 $ do
      loadingMessage "Adding to Queue"
  viewGenerateStep (Just gen) =
    case gen of
      GenWaiting -> do
        row @ onLoad ReloadGen 1000 $ do
          loadingMessage "Waiting for job to start"
          space
      GenStarted ->
        row @ onLoad ReloadGen 1000 $ do
          loadingMessage "Started"
      GenTransferring taskId -> do
        el "Generating FITS - Transferring L1 Files"
        hyper (GenerateTransfer inv.proposalId inv.programId inv.inversionId taskId) $ do
          Transfer.viewLoadTransfer GenTransfer
      GenTransferComplete -> do
        el "Generating FITS - Transfer Complete"
        row @ onLoad ReloadGen 1000 $ do
          loadingMessage "Waiting for frame generation to start"
          space
      GenFrames{complete, total, throughput, skipped} -> do
        loadingMessage "Generating FITS"
        col @ onLoad ReloadGen 1000 $ do
          let done = complete + skipped
          row ~ gap 5 $ do
            speedMessage throughput
            space
            text $ cs $ show complete
            case skipped of
              0 -> pure ()
              _ -> do
                text " + "
                text $ cs $ show skipped
                text " skipped"
            el " / "
            el $ text $ cs $ show gen.total
          View.progress (fromIntegral done / fromIntegral total)
      GenAsdf -> do
        loadingMessage "Generating ASDF"
        el @ onLoad ReloadGen 1000 $ none

  speedMessage throughput = do
    case throughput of
      0 -> none
      _ ->
        el $ do
          text $ cs $ showFFloat (Just 2) (throughput * 60) ""
          text " frames per minute"

  loadingMessage msg =
    row ~ gap 5 $ do
      el ~ width 20 $ Icons.spinnerCircle
      col $ do
        space
        el $ text msg
        space


-- GenerateTransfer ---------------------------------------------

data GenerateTransfer = GenerateTransfer (Id Proposal) (Id InstrumentProgram) (Id Inversion) (Id Task)
  deriving (Generic, ViewId)


instance (Tasks GenTask :> es, Inversions :> es, Datasets :> es, Log :> es, Transfer :> es) => HyperView GenerateTransfer es where
  type Require GenerateTransfer = '[GenerateStep]
  data Action GenerateTransfer
    = GenTransfer TransferAction
    deriving (Generic, ViewAction)


  update (GenTransfer action) = do
    GenerateTransfer propId progId invId taskId <- viewId
    case action of
      TaskFailed -> do
        pure $ do
          Transfer.viewTransferFailed taskId
          target (GenerateStep propId progId invId) $ do
            button RegenFits ~ Style.btn Primary $ "Restart Transfer"
      TaskSucceeded ->
        pure $ do
          target (GenerateStep propId progId invId) $ do
            el @ onLoad ReloadGen 1000 $ "SUCCEEDED"
      CheckTransfer -> do
        Transfer.checkTransfer GenTransfer taskId


viewGeneratedFiles :: Remote Scratch -> Inversion -> View c ()
viewGeneratedFiles scratch inv = do
  link (FileManager.openInversion scratch $ Files.outputL2Dir inv.proposalId inv.inversionId) ~ Style.btnOutline Success . grow @ att "target" "_blank" $ "View Generated Files"


-- ----------------------------------------------------------------
-- STEP PUBLISH
-- ----------------------------------------------------------------

publishStep :: Inversion -> Step
publishStep inv
  | isPublished inv = StepDone
  | isGenerated inv = StepActive
  | otherwise = StepNext


data PublishStep = PublishStep Bucket (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Generic, ViewId)


instance (Inversions :> es, Scratch :> es, Time :> es, Tasks PublishTask :> es, Log :> es, Transfer :> es) => HyperView PublishStep es where
  type Require PublishStep = '[InversionStatus]


  data Action PublishStep
    = StartPublish
    | CheckPublish
    | PublishTransfer (Id Task) TransferAction
    deriving (Generic, ViewAction)


  update action = do
    PublishStep bucket propId _ invId <- viewId
    case action of
      StartPublish -> do
        Inversions.resetPublished invId
        Inversions.clearError invId
        Publish.startPublish propId invId
        refreshInversion
      CheckPublish -> do
        inv <- loadInversion invId
        mstatus <- send $ TaskLookupStatus $ PublishTask propId invId
        pub <- send RemotePublish
        pure $ viewPublish pub bucket inv mstatus
      PublishTransfer taskId TaskFailed -> do
        pure $ do
          Transfer.viewTransferFailed taskId
          button StartPublish ~ Style.btn Primary . grow $ "Restart Publish"
      PublishTransfer _ TaskSucceeded -> do
        refreshInversion
      PublishTransfer taskId CheckTransfer -> do
        Transfer.checkTransfer (PublishTransfer taskId) taskId
   where
    refreshInversion = do
      PublishStep _ propId progId invId <- viewId
      pure $ target (InversionStatus propId progId invId) $ do
        el @ onLoad Reload 0 $ none


-- what if it is actively being published?
viewPublish :: Remote Publish -> Bucket -> Inversion -> Maybe PublishStatus -> View PublishStep ()
viewPublish pub bucket inv mstatus
  | isPublished inv = viewPublished pub bucket inv.proposalId inv.inversionId
  | Just _ <- generated inv = viewPublishStep inv mstatus
  | otherwise = none


viewPublishStep :: Inversion -> Maybe PublishStatus -> View PublishStep ()
viewPublishStep inv mstatus =
  -- has the transfer started?
  case mstatus of
    Nothing -> maybe viewNeedsPublish viewPublishError inv.invError
    Just ps -> viewPublishStatus ps
 where
  viewNeedsPublish =
    button StartPublish ~ Style.btn Primary . grow $ "Publish Inversion "


viewPublishError :: Text -> View PublishStep ()
viewPublishError e = do
  col ~ gap 15 $ do
    row $ View.systemError $ cs e
    row ~ gap 10 $ do
      button StartPublish ~ Style.btn Primary . grow $ "Retry Publish"
      when ("GlobusError" `T.isPrefixOf` e) $ do
        route Logout ~ Style.btnOutline Secondary $ "Reauthenticate"


viewPublishStatus :: PublishStatus -> View PublishStep ()
viewPublishStatus = \case
  PublishWaiting ->
    el @ onLoad CheckPublish 1000 $ "Waiting to start..."
  PublishStarted ->
    el @ onLoad CheckPublish 1000 $ "Starting Transfer"
  PublishTransferring taskId -> do
    el "Publishing..."
    Transfer.viewLoadTransfer (PublishTransfer taskId)


viewPublished :: Remote Publish -> Bucket -> Id Proposal -> Id Inversion -> View PublishStep ()
viewPublished remote bucket propId invId = do
  row ~ gap 10 $ do
    link (FileManager.openPublish remote $ DKIST.publishDir bucket propId invId) ~ Style.btnOutline Success . grow @ att "target" "_blank" $ do
      "View Published Files"
    button StartPublish ~ Style.btnOutline Secondary $ "Republish"

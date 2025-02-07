{-# LANGUAGE UndecidableInstances #-}

module App.Page.Program where

import App.Colors
import App.Effect.Auth
import App.Error (expectFound)
import App.Globus (DownloadFolder, FileLimit (Folders), Globus, TransferForm)
import App.Globus qualified as Globus
import App.Page.Inversion qualified as Inversion
import App.Page.Inversions (rowInversion)
import App.Page.Inversions.Transfer (TransferAction (..), activeTransfer, saveActiveTransfer)
import App.Page.Inversions.Transfer qualified as Transfer
import App.Route qualified as Route
import App.Style qualified as Style
import App.View.Common as View
import App.View.DataRow (dataRows)
import App.View.DatasetsTable as DatasetsTable
import App.View.Icons qualified as Icons
import App.View.Inversions (inversionStepLabel)
import App.View.Layout
import App.View.ProposalDetails
import App.Worker.GenWorker
import Data.Grouped (Grouped (..))
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log hiding (Info)
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs hiding (programInversions)
import NSO.Data.Programs qualified as Programs
import NSO.Data.Qualify (qualify)
import NSO.Prelude
import NSO.Types.InstrumentProgram (Proposal)
import Web.Hyperbole


page
  :: (Hyperbole :> es, Time :> es, Datasets :> es, Inversions :> es, Auth :> es, Globus :> es, Tasks GenFits :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Eff es (Page '[ProgramInversions, ProgramDatasets, DownloadTransfer])
page propId progId = do
  ds <- Datasets.find (Datasets.ByProgram progId) >>= expectFound
  progs <- Programs.loadProgram progId >>= expectFound
  let prog = head progs
  now <- currentTime
  download <- activeTransfer progId

  appLayout Route.Proposals $ do
    col (Style.page . gap 30) $ do
      viewPageHeader (head ds)

      hyper (ProgramInversions propId progId) $ viewProgramInversions prog

      col Style.card $ do
        el (Style.cardHeader Secondary) $ text "Program"
        viewProgramDetails' (viewProgramStats now) prog prog.datasets

        hyper (ProgramDatasets propId progId) $ viewDatasets (NE.toList prog.datasets.items) ByLatest download

      col (gap 10) $ do
        el bold "Experiment"
        viewExperimentDescription (head ds).experimentDescription
 where
  viewPageHeader :: Dataset -> View c ()
  viewPageHeader ds = do
    col (gap 5) $ do
      el Style.header $ do
        text "Instrument Program - "
        text progId.fromId
      experimentLink ds

  experimentLink :: Dataset -> View c ()
  experimentLink d = do
    el_ $ do
      text "Proposal - "
      route (Route.Proposal d.primaryProposalId Route.PropRoot) Style.link $ do
        text d.primaryProposalId.fromId


-- ----------------------------------------------------------------
-- SUBMIT DOWNLOAD
-- ----------------------------------------------------------------

submitDownload :: (Log :> es, Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es) => Id Proposal -> Id InstrumentProgram -> Eff es Response
submitDownload propId progId = do
  tfrm <- formData @TransferForm
  tfls <- formData @DownloadFolder
  ds <- Datasets.find $ Datasets.ByProgram progId
  taskId <- requireLogin $ Globus.initDownloadL1Inputs tfrm tfls ds

  saveActiveTransfer progId taskId

  redirect $ routeUrl (Route.Proposal propId $ Route.Program progId Route.Prog)


----------------------------------------------------
-- ProgramInversions
----------------------------------------------------

data ProgramInversions = ProgramInversions (Id Proposal) (Id InstrumentProgram)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Globus :> es, Auth :> es, Tasks GenFits :> es) => HyperView ProgramInversions es where
  data Action ProgramInversions
    = CreateInversion
    deriving (Show, Read, ViewAction)


  type Require ProgramInversions = '[]


  update = \case
    CreateInversion -> do
      ProgramInversions propId progId <- viewId
      inv <- send $ Inversions.Create propId progId
      redirect $ inversionUrl propId inv.inversionId


inversionUrl :: Id Proposal -> Id Inversion -> Url
inversionUrl ip ii = routeUrl $ Route.Proposal ip $ Route.Inversion ii Route.Inv


viewProgramInversions :: ProgramFamily -> View ProgramInversions ()
viewProgramInversions prog =
  case prog.inversions of
    (_ : _) -> viewInversions
    [] -> firstInversion
 where
  viewInversions = do
    col Style.card $ do
      el (Style.cardHeader invHeaderColor) $ text "Inversions"
      col (gap 10 . pad 15) $ do
        col id $ do
          dataRows prog.inversions rowInversion
        button CreateInversion (Style.btnOutline Primary) "Create New Inversion"

  firstInversion = do
    case qualify prog.datasets of
      Left _ -> none
      Right _ -> iconButton CreateInversion (Style.btn Primary) Icons.plus "Create Inversion"

  invHeaderColor
    | any isPublished prog.inversions = Success
    | otherwise = Info


viewCurrentInversion :: Inversion -> View c ()
viewCurrentInversion inv = do
  let step = inversionStep inv
  Inversion.viewInversionContainer inv $ do
    Inversion.invertStep inv $ do
      viewInvert step

    Inversion.generateStep inv $ do
      viewGenerate step

    Inversion.publishStep inv $ do
      viewPublish step
 where
  viewInvert = \case
    StepInvert -> continueButton
    _ -> none

  viewGenerate = \case
    StepGenerate -> continueButton
    _ -> none

  viewPublish = \case
    StepPublish ->
      link (inversionUrl inv.proposalId inv.inversionId) (Style.btn Primary) "View Inversion"
    _ -> none

  continueButton =
    link (inversionUrl inv.proposalId inv.inversionId) (Style.btn Primary) "Continue Inversion"


viewOldInversion :: Inversion -> View c ()
viewOldInversion inv = row (gap 4) $ do
  el_ "•"
  link (inversionUrl inv.proposalId inv.inversionId) Style.link $ do
    text inv.inversionId.fromId
  el_ $ text $ cs $ showDate inv.created
  el_ $ text $ inversionStepLabel inv


-- clearInversion :: Id Proposal -> Id InstrumentProgram -> Eff es (View InversionStatus ())
-- clearInversion ip iip = pure $ do
--   target (ProgramInversions ip iip) $ onLoad ReloadAll 0 emptyButtonSpace
--  where
--   emptyButtonSpace = el (height 44) none

-- ----------------------------------------------------------------
-- DATASETS
-- ----------------------------------------------------------------

data ProgramDatasets = ProgramDatasets (Id Proposal) (Id InstrumentProgram)
  deriving (Show, Read, ViewId)


instance (Inversions :> es, Globus :> es, Auth :> es, Datasets :> es, Time :> es) => HyperView ProgramDatasets es where
  data Action ProgramDatasets
    = GoDownload
    | SortDatasets SortField
    deriving (Show, Read, ViewAction)


  type Require ProgramDatasets = '[DownloadTransfer]


  update GoDownload = do
    ProgramDatasets propId progId <- viewId
    r <- request
    requireLogin $ do
      redirect $ Globus.fileManagerSelectUrl (Folders 1) (Route.Proposal propId $ Route.Program progId Route.SubmitDownload) ("Transfer Instrument Program " <> progId.fromId) r
  update (SortDatasets srt) = do
    ProgramDatasets _ progId <- viewId
    progs <- Programs.loadProgram progId
    download <- activeTransfer progId
    pure $ do
      forM_ progs $ \prog -> do
        viewDatasets (NE.toList prog.datasets.items) srt download


viewDatasets :: [Dataset] -> SortField -> Maybe (Id Globus.Task) -> View ProgramDatasets ()
viewDatasets ds srt xfer = do
  ProgramDatasets propId progId <- viewId
  col (gap 15 . pad 15) $ do
    DatasetsTable.datasetsTable SortDatasets srt ds
    case xfer of
      Nothing -> iconButton GoDownload (Style.btn Primary) Icons.downTray "Download Datasets"
      Just taskId -> hyper (DownloadTransfer propId progId taskId) (Transfer.viewLoadTransfer DwnTransfer)


-- ----------------------------------------------------------------
-- DOWNLOAD
-- ----------------------------------------------------------------

data DownloadTransfer = DownloadTransfer (Id Proposal) (Id InstrumentProgram) (Id Globus.Task)
  deriving (Show, Read, ViewId)


instance (Globus :> es, Auth :> es, Datasets :> es) => HyperView DownloadTransfer es where
  data Action DownloadTransfer
    = DwnTransfer TransferAction
    deriving (Show, Read, ViewAction)


  type Require DownloadTransfer = '[ProgramDatasets]


  update (DwnTransfer action) = do
    DownloadTransfer _ progId taskId <- viewId
    case action of
      TaskFailed -> do
        pure $ col (gap 10) $ do
          redownloadBtn (Style.btn Primary) "Download Datasets"
          Transfer.viewTransferFailed taskId
      TaskSucceeded -> do
        ds <- Datasets.find (Datasets.ByProgram progId)
        pure $ col (gap 10) $ do
          redownloadBtn (Style.btn Primary) "Download Datasets"
          row (gap 10 . color Success) $ do
            el_ "Successfully Downloaded: "
            forM_ ds $ \d -> do
              el_ $ text d.datasetId.fromId
      CheckTransfer -> do
        vw <- Transfer.checkTransfer DwnTransfer taskId
        pure $ col (gap 10) $ do
          redownloadBtn (Style.btnLoading Secondary) "Downloading"
          vw


redownloadBtn :: Mod ProgramDatasets -> Text -> View DownloadTransfer ()
redownloadBtn f lbl = do
  DownloadTransfer propId progId _ <- viewId
  target (ProgramDatasets propId progId) $ do
    iconButton GoDownload f Icons.downTray lbl

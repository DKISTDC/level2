{-# LANGUAGE UndecidableInstances #-}

module App.Page.Program where

import App.Colors
import App.Effect.Auth as Auth
import App.Effect.FileManager (FileLimit (Folders))
import App.Effect.Transfer (DownloadFolder, TransferForm)
import App.Effect.Transfer qualified as Transfer
import App.Error (expectFound)
import App.Route as Route
import App.Style qualified as Style
import App.Types (App)
import App.View.Common as View
import App.View.DataRow (dataRows)
import App.View.DatasetsTable as DatasetsTable
import App.View.Error
import App.View.Icons qualified as Icons
import App.View.Inversion (rowInversion)
import App.View.Layout
import App.View.ProposalDetails
import App.View.Transfer (TransferAction (..))
import App.View.Transfer qualified as Transfer
import App.Worker.GenWorker
import Data.Grouped (Group (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Globus (Globus, GlobusError)
import Effectful.Log hiding (Info)
import Effectful.Reader.Dynamic (Reader)
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs hiding (programInversions)
import NSO.Data.Programs qualified as Programs
import NSO.Data.Qualify (qualify)
import NSO.Prelude
import NSO.Types.InstrumentProgram (Proposal)
import Network.Globus (Task)
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData (fromQueryData)


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
  ActiveDownload download <- query

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
      appRoute (Route.Proposal d.primaryProposalId Route.PropRoot) Style.link $ do
        text d.primaryProposalId.fromId


-- ----------------------------------------------------------------
-- SUBMIT DOWNLOAD
-- ----------------------------------------------------------------

data ActiveDownload = ActiveDownload
  { downloadTaskId :: Maybe (Id Task)
  }
  deriving (Generic, ToQuery, FromQuery)


submitDownload :: (Log :> es, Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es) => Id Proposal -> Id InstrumentProgram -> Eff es Response
submitDownload propId progId = do
  log Debug $ dump "Submit Download" (propId, progId)
  tfrm <- formData @TransferForm
  tfls <- formData @DownloadFolder
  ds <- Datasets.find $ Datasets.ByProgram progId
  taskId <- requireLogin $ userFacingError @GlobusError $ Transfer.initDownloadL1Inputs tfrm tfls ds

  let dwn = ActiveDownload (Just taskId)

  redirect $ activeDownloadQuery dwn $ routeUrl (Route.Proposal propId $ Route.Program progId Route.Prog)
 where
  setUrlQuery q Url{scheme, domain, path} =
    Url{scheme, domain, path, query = q}

  activeDownloadQuery :: ActiveDownload -> Url -> Url
  activeDownloadQuery ad =
    setUrlQuery (fromQueryData $ toQuery ad)


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
      invId <- send Inversions.NewId
      redirect $ routeUrl $ Route.inversionUpload propId progId invId


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
        iconButton CreateInversion (Style.btnOutline Primary) Icons.plus "Create New Inversion"

  firstInversion = do
    let res = qualify prog.datasets
    iconButton CreateInversion (Style.btn Primary . qualified res) Icons.plus "Create Inversion"
    qualifyMessage res

  qualifyMessage = \case
    Left _ -> el italic "This Instrument Program failed to qualify for inversion. See below."
    Right _ -> none

  qualified = \case
    Left _ -> Style.disabled
    Right _ -> id

  invHeaderColor
    | any isPublished prog.inversions = Success
    | otherwise = Info


-- viewCurrentInversion :: Inversion -> View c ()
-- viewCurrentInversion inv = do
--   let step = inversionStep inv
--   viewInversionContainer inv $ do
--     Inversion.invertStep inv $ do
--       viewInvert step
--
--     Inversion.generateStep inv $ do
--       viewGenerate step
--
--     Inversion.publishStep inv $ do
--       viewPublish step
--  where
--   viewInvert = \case
--     StepInvert -> continueButton
--     _ -> none
--
--   viewGenerate = \case
--     StepGenerate -> continueButton
--     _ -> none
--
--   viewPublish = \case
--     StepPublish ->
--       link (Route.inversionUrl inv.proposalId inv.inversionId) (Style.btn Primary) "View Inversion"
--     _ -> none
--
--   continueButton =
--     link (Route.inversionUrl inv.proposalId inv.inversionId) (Style.btn Primary) "Continue Inversion"

-- viewOldInversion :: Inversion -> View c ()
-- viewOldInversion inv = row (gap 4) $ do
--   el_ "•"
--   link (Route.inversionUrl inv.proposalId inv.inversionId) Style.link $ do
--     text inv.inversionId.fromId
--   el_ $ text $ cs $ showDate inv.created
--   el_ $ text $ inversionStepLabel inv

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


instance (Inversions :> es, Globus :> es, Auth :> es, Datasets :> es, Time :> es, Reader App :> es) => HyperView ProgramDatasets es where
  data Action ProgramDatasets
    = GoDownload
    | SortDatasets SortField
    deriving (Show, Read, ViewAction)


  type Require ProgramDatasets = '[DownloadTransfer]


  update GoDownload = do
    ProgramDatasets propId progId <- viewId
    -- r <- request
    let submitUrl = routeUrl $ Route.Proposal propId $ Route.Program progId Route.SubmitDownload
    Auth.openFileManager (Folders 1) ("Transfer Instrument Program " <> progId.fromId) submitUrl
  update (SortDatasets srt) = do
    ProgramDatasets _ progId <- viewId
    progs <- Programs.loadProgram progId
    ActiveDownload download <- query
    pure $ do
      forM_ progs $ \prog -> do
        viewDatasets (NE.toList prog.datasets.items) srt download


viewDatasets :: [Dataset] -> SortField -> Maybe (Id Task) -> View ProgramDatasets ()
viewDatasets ds srt xfer = do
  ProgramDatasets propId progId <- viewId
  col (gap 15 . pad 15) $ do
    DatasetsTable.datasetsTable SortDatasets srt ds
    case xfer of
      Nothing -> iconButton GoDownload (Style.btn Primary) Icons.downTray "Download Datasets"
      Just taskId -> hyper (DownloadTransfer propId progId taskId) viewDownloadLoad


-- ----------------------------------------------------------------
-- DOWNLOAD
-- ----------------------------------------------------------------

data DownloadTransfer = DownloadTransfer (Id Proposal) (Id InstrumentProgram) (Id Task)
  deriving (Show, Read, ViewId)


instance (Globus :> es, Auth :> es, Datasets :> es, Log :> es) => HyperView DownloadTransfer es where
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
            el_ $ text $ T.intercalate ", " $ fmap (\d -> d.datasetId.fromId) ds
      CheckTransfer -> do
        res <- runErrorNoCallStack @GlobusError $ Transfer.checkTransfer DwnTransfer taskId
        pure $ col (gap 10) $ do
          redownloadBtn (Style.btnLoading Secondary) "Downloading"
          either (Transfer.viewTransferError taskId) id res


viewDownloadLoad :: View DownloadTransfer ()
viewDownloadLoad = do
  col (gap 10) $ do
    redownloadBtn (Style.btn Secondary . att "disabled" "") "Download Datasets"
    Transfer.viewLoadTransfer DwnTransfer


redownloadBtn :: Mod ProgramDatasets -> Text -> View DownloadTransfer ()
redownloadBtn f lbl = do
  DownloadTransfer propId progId _ <- viewId
  target (ProgramDatasets propId progId) $ do
    iconButton GoDownload f Icons.downTray lbl

{-# LANGUAGE UndecidableInstances #-}

module App.Page.Program where

import App.Colors
import App.Effect.Auth as Auth
import App.Effect.Transfer (Transfer (RemoteLevel1))
import App.Error (expectFound)
import App.Route as Route
import App.Style qualified as Style
import App.Types (App)
import App.View.Common as View
import App.View.DataRow (dataRows)
import App.View.Datasets as DatasetsTable
import App.View.Icons qualified as Icons
import App.View.Inversion (rowInversion)
import App.View.Layout
import App.View.ProposalDetails
import App.Worker.Generate
import Data.Grouped (Group (..), sample)
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus (Globus)
import Effectful.Log hiding (Info)
import Effectful.Reader.Dynamic (Reader)
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs hiding (programInversions)
import NSO.Data.Programs qualified as Programs
import NSO.Data.Qualify (qualify)
import NSO.Files.DKIST (Level1)
import NSO.Files.RemoteFolder (Remote)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (Proposal)
import Web.Atomic.CSS
import Web.Hyperbole


-- import App.Page.Datasets.Download (ActiveDownload (..))

page
  :: (Hyperbole :> es, Time :> es, Datasets :> es, Inversions :> es, Auth :> es, Globus :> es, Tasks GenTask :> es, Transfer :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Page es '[ProgramInversions, ProgramDatasets, ProgramDetails]
page propId progId = do
  ds <- Datasets.find (Datasets.ByProgram progId) >>= expectFound
  progs <- Programs.loadProgram progId >>= expectFound
  let prog = head progs
  now <- currentTime
  l1 <- send RemoteLevel1
  -- ActiveDownload download <- query

  appLayout Route.Proposals $ do
    col ~ Style.page . gap 30 $ do
      viewPageHeader (head ds)

      hyper (ProgramInversions propId progId) $ viewProgramInversions prog

      col ~ Style.card $ do
        el ~ Style.cardHeader Secondary $ text "Program"
        hyper (ProgramDetails propId progId) $ viewProgramDetails prog now
        hyper (ProgramDatasets propId progId) $ viewDatasets l1 (NE.toList prog.datasets.items) ByLatest

      col ~ gap 10 $ do
        el ~ bold $ "Experiment"
        viewExperimentDescription (head ds).experimentDescription
 where
  viewPageHeader :: Dataset -> View c ()
  viewPageHeader ds = do
    col ~ gap 5 $ do
      el ~ Style.header $ do
        text "Instrument Program - "
        text progId.fromId
      experimentLink ds

  experimentLink :: Dataset -> View c ()
  experimentLink d = do
    el $ do
      text "Proposal - "
      appRoute (Route.Proposal d.primaryProposalId Route.PropRoot) ~ Style.link $ do
        text d.primaryProposalId.fromId


data ProgramDetails = ProgramDetails (Id Proposal) (Id InstrumentProgram)
  deriving (Generic, ViewId)


instance HyperView ProgramDetails es where
  data Action ProgramDetails = GenIronImage
    deriving (Generic, ViewAction)


  update _ = pure none


viewProgramDetails :: ProgramFamily -> UTCTime -> View c ()
viewProgramDetails prog now = do
  let ds = prog.datasets
  viewProgramRowLink now prog

  View.hr ~ color Gray

  col ~ pad 15 . gap 10 $ do
    viewCriteria prog ds
    viewWavelengthRanges prog.program.spectralLines prog.datasets.items
    viewFriedHistogram (sample ds).friedParameter


-- viewIronPlot GenIronImage ds.items

----------------------------------------------------
-- ProgramInversions
----------------------------------------------------

data ProgramInversions = ProgramInversions (Id Proposal) (Id InstrumentProgram)
  deriving (Generic, ViewId)


instance (Inversions :> es, Auth :> es, Tasks GenTask :> es) => HyperView ProgramInversions es where
  data Action ProgramInversions
    = CreateInversion
    deriving (Generic, ViewAction)


  type Require ProgramInversions = '[]


  update = \case
    CreateInversion -> do
      ProgramInversions propId progId <- viewId
      invId <- send Inversions.NewId
      redirect $ routeUri $ Route.inversionUpload propId progId invId


viewProgramInversions :: ProgramFamily -> View ProgramInversions ()
viewProgramInversions prog =
  case prog.inversions of
    (_ : _) -> viewInversions
    [] -> firstInversion
 where
  viewInversions = do
    col ~ Style.card $ do
      el ~ Style.cardHeader invHeaderColor $ text "Inversions"
      col ~ gap 10 . pad 15 $ do
        col $ do
          dataRows prog.inversions rowInversion
        iconButton CreateInversion Icons.plus "Create New Inversion" ~ Style.btnOutline Primary

  firstInversion = do
    let res = qualify prog.datasets
    iconButton CreateInversion Icons.plus "Create Inversion" ~ Style.btn Primary . qualified res
    qualifyMessage res

  qualifyMessage = \case
    Left _ -> el ~ italic $ "This Instrument Program failed to qualify for inversion. See below."
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
  deriving (Generic, ViewId)


instance (Inversions :> es, Auth :> es, Datasets :> es, Time :> es, Reader App :> es, Log :> es, Transfer :> es) => HyperView ProgramDatasets es where
  data Action ProgramDatasets
    = SortDatasets SortField
    deriving (Generic, ViewAction)


  update (SortDatasets srt) = do
    ProgramDatasets _ progId <- viewId
    progs <- Programs.loadProgram progId
    l1 <- send RemoteLevel1
    pure $ do
      forM_ progs $ \prog -> do
        viewDatasets l1 (NE.toList prog.datasets.items) srt


viewDatasets :: Remote Level1 -> [Dataset] -> SortField -> View ProgramDatasets ()
viewDatasets l1 ds srt = do
  col ~ gap 15 . pad 15 $ do
    DatasetsTable.datasetsTable l1 SortDatasets srt ds

{-# LANGUAGE UndecidableInstances #-}

module App.Page.Program where

import App.Colors
import App.Effect.Auth as Auth
import App.Effect.Transfer
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
import Data.Grouped (Group (..), sample)
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus (Globus)
import Effectful.Log hiding (Info)
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs as Programs
import NSO.Data.Qualify (qualify)
import NSO.Files.RemoteFolder (Ingest, Level1, Remote)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Proposal (Proposal)
import Web.Atomic.CSS
import Web.Hyperbole


-- import App.Page.Datasets.Download (ActiveDownload (..))

page
  :: (Hyperbole :> es, Time :> es, Datasets :> es, Inversions :> es, Programs :> es, Auth :> es, Globus :> es, Transfer Level1 Ingest :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Page es '[ProgramInversions, ProgramDatasets, ProgramDetails]
page propId progId = do
  ds <- Datasets.findLatest (Datasets.ByProgram progId) >>= expectFound
  progs <- send (Programs.Lookup progId) >>= expectFound
  let prog = head progs
  now <- currentTime
  l1 <- send (RemoteSource @Level1 @Ingest)
  invs <- send $ Inversions.ByProgram progId
  -- ActiveDownload download <- query

  appLayout Route.Proposals $ do
    col ~ Style.page . gap 30 $ do
      viewPageHeader (head ds)

      hyper (ProgramInversions propId progId) $ viewProgramInversions prog ds invs

      col ~ Style.card $ do
        el ~ Style.cardHeader Secondary $ text "Program"
        hyper (ProgramDetails propId progId) $ viewProgramDetails prog ds now
        hyper (ProgramDatasets propId progId) $ viewDatasets l1 (NE.toList ds) Latest ProductId

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


viewProgramDetails :: InstrumentProgram -> NonEmpty Dataset -> UTCTime -> View c ()
viewProgramDetails prog ds now = do
  viewProgramRowLink now prog

  View.hr ~ color Gray

  col ~ pad 15 . gap 10 $ do
    viewCriteria prog ds
    viewWavelengthRanges prog.spectralLines ds
    viewFriedHistogram (head ds).friedParameter


-- viewIronPlot GenIronImage ds.items

----------------------------------------------------
-- ProgramInversions
----------------------------------------------------

data ProgramInversions = ProgramInversions (Id Proposal) (Id InstrumentProgram)
  deriving (Generic, ViewId)


instance (Inversions :> es, Auth :> es) => HyperView ProgramInversions es where
  data Action ProgramInversions
    = CreateInversion
    deriving (Generic, ViewAction)


  type Require ProgramInversions = '[]


  update = \case
    CreateInversion -> do
      ProgramInversions propId progId <- viewId
      invId <- send Inversions.NewId
      redirect $ routeUri $ Route.inversionUpload propId progId invId


viewProgramInversions :: InstrumentProgram -> NonEmpty Dataset -> [Inversion] -> View ProgramInversions ()
viewProgramInversions prog ds invs =
  case invs of
    (_ : _) -> viewInversions
    [] -> firstInversion
 where
  viewInversions = do
    col ~ Style.card $ do
      el ~ Style.cardHeader invHeaderColor $ text "Inversions"
      col ~ gap 10 . pad 15 $ do
        col $ do
          dataRows invs rowInversion
        iconButton CreateInversion Icons.plus "Create New Inversion" ~ Style.btnOutline Primary

  firstInversion = do
    let res = qualify ds
    iconButton CreateInversion Icons.plus "Create Inversion" ~ Style.btn Primary . qualified res
    qualifyMessage res

  qualifyMessage = \case
    Left _ -> el ~ italic $ "This Instrument Program failed to qualify for inversion. See below."
    Right _ -> none

  qualified = \case
    Left _ -> Style.disabled
    Right _ -> id

  invHeaderColor
    | any isPublished invs = Success
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


instance (Inversions :> es, Auth :> es, Datasets :> es, Time :> es, Reader App :> es, Log :> es, Transfer Level1 Ingest :> es) => HyperView ProgramDatasets es where
  data Action ProgramDatasets
    = SortDatasets Reprocessing SortField
    deriving (Generic, ViewAction)


  update (SortDatasets r srt) = do
    ProgramDatasets _ progId <- viewId
    ds <- send $ Datasets.Find (Datasets.ByProgram progId) r
    l1 <- send (RemoteSource @Level1 @Ingest)
    pure $ viewDatasets l1 ds r srt


viewDatasets :: Remote Level1 -> [Dataset] -> Reprocessing -> SortField -> View ProgramDatasets ()
viewDatasets l1 ds r srt = do
  col ~ gap 15 . pad 15 $ do
    DatasetsTable.datasetsTable l1 (SortDatasets r) srt ds
    case r of
      Complete -> button (SortDatasets Latest srt) ~ Style.link $ "Hide Reprocessed Datasets"
      Latest -> button (SortDatasets Complete ProductId) ~ Style.link $ "Show Reprocessed Datasets"

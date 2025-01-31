{-# LANGUAGE UndecidableInstances #-}

module App.Page.Program where

import App.Colors
import App.Effect.Auth
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Inversion qualified as Inversion
import App.Route qualified as Route
import App.Style qualified as Style
import App.View.Common as View
import App.View.DatasetsTable as DatasetsTable
import App.View.Inversions (inversionStepLabel)
import App.View.Layout
import App.View.ProposalDetails
import App.Worker.GenWorker
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs hiding (programInversions)
import NSO.Prelude
import NSO.Types.InstrumentProgram (Proposal)
import Web.Hyperbole


page
  :: (Hyperbole :> es, Log :> es, Time :> es, Datasets :> es, Inversions :> es, Auth :> es, Globus :> es, Tasks GenFits :> es, IOE :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Eff es (Page '[ProgramInversions, ProgramDatasets])
page propId progId = do
  ds <- Datasets.find (Datasets.ByProgram progId) >>= expectFound
  invs <- findInversionsByProgram progId
  now <- currentTime
  let progs = programFamilies invs (NE.toList ds)

  appLayout Route.Proposals $ do
    col (Style.page . gap 30) $ do
      col (gap 5) $ do
        el Style.header $ do
          text "Instrument Program - "
          text progId.fromId
        experimentLink (head ds)

      viewExperimentDescription (head ds).experimentDescription

      hyper (ProgramInversions propId progId) $ viewProgramInversions invs

      -- we don't really need/want to defer this
      mapM_ (viewProgramSummary now) progs
 where
  experimentLink :: Dataset -> View c ()
  experimentLink d = do
    el_ $ do
      text "Proposal - "
      route (Route.Proposal d.primaryProposalId Route.PropRoot) Style.link $ do
        text d.primaryProposalId.fromId


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
      ProgramInversions ip iip <- viewId
      inv <- send $ Inversions.Create ip iip
      redirect $ inversionUrl ip inv.inversionId


inversionUrl :: Id Proposal -> Id Inversion -> Url
inversionUrl ip ii = routeUrl $ Route.Proposal ip $ Route.Inversion ii Route.Inv


viewProgramInversions :: [Inversion] -> View ProgramInversions ()
viewProgramInversions (inv : is) = do
  viewCurrentInversion inv
  col (gap 10 . pad 10) $ do
    mapM_ viewOldInversion is
    row id $ do
      button CreateInversion Style.link "Start Over With New Inversion"
viewProgramInversions _ = do
  button CreateInversion (Style.btn Primary) "Create Inversion"


viewCurrentInversion :: Inversion -> View c ()
viewCurrentInversion inv = do
  let step = inversionStep inv
  Inversion.viewInversionContainer step $ do
    Inversion.downloadStep step $ do
      viewDownload step

    Inversion.invertStep step $ do
      viewInvert step

    Inversion.generateStep step $ do
      viewGenerate step

    Inversion.publishStep step $ do
      viewPublish step
 where
  viewDownload = \case
    StepDownload _ -> continueButton
    _ -> none

  viewInvert = \case
    StepInvert _ -> continueButton
    _ -> none

  viewGenerate = \case
    StepGenerate _ -> continueButton
    _ -> none

  viewPublish = \case
    StepPublish _ ->
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
  el_ $ text $ inversionStepLabel (inversionStep inv)

-- clearInversion :: Id Proposal -> Id InstrumentProgram -> Eff es (View InversionStatus ())
-- clearInversion ip iip = pure $ do
--   target (ProgramInversions ip iip) $ onLoad ReloadAll 0 emptyButtonSpace
--  where
--   emptyButtonSpace = el (height 44) none

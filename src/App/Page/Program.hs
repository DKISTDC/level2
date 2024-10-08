module App.Page.Program where

import App.Colors
import App.Effect.Auth
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Inversion as Inversion
import App.Route qualified as Route
import App.Style qualified as Style
import App.View.Common as View
import App.View.DatasetsTable as DatasetsTable
import App.View.Inversions (inversionStatusLabel)
import App.View.Layout
import App.View.ProposalDetails
import App.Worker.GenWorker
import Data.Grouped as G
import Data.List (nub)
import Data.Ord (Down (..))
import Data.String.Interpolate (i)
import Effectful.Dispatch.Dynamic
import Effectful.Log (Log)
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs hiding (programInversions)
import NSO.Prelude
import NSO.Types.InstrumentProgram (Proposal)
import Web.Hyperbole


page
  :: (Hyperbole :> es, Log :> es, Time :> es, Datasets :> es, Inversions :> es, Auth :> es, Globus :> es, Tasks GenFits :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Page es (ProgramInversions : ProgramDatasets : InversionStatus : InversionViews)
page ip iip = do
  handle (inversions (clearInversion ip iip))
  $ handle programInversions
  $ handle DatasetsTable.actionSort
  $ handle Inversion.inversionCommit
  $ handle Inversion.preprocessCommit
  $ handle Inversion.downloadTransfer
  $ handle Inversion.generateTransfer
  $ handle Inversion.uploadTransfer
  $ load
  $ do
    ds' <- send $ Datasets.Query (Datasets.ByProgram iip)
    ds <- expectFound ds'
    let d = head ds

    dse <- send $ Datasets.Query (ByProposal ip)
    invs <- latestInversions iip
    steps <- mapM inversionStep invs
    now <- currentTime
    let gds = Grouped ds :: Grouped InstrumentProgram Dataset
    let p = instrumentProgram gds invs

    appLayout Route.Proposals $ do
      col (Style.page . gap 30) $ do
        col (gap 5) $ do
          el Style.header $ do
            text "Instrument Program - "
            text iip.fromId

          experimentLink d (numOtherIps dse)

        viewExperimentDescription d.experimentDescription

        hyper (ProgramInversions ip iip) $ viewProgramInversions invs steps

        viewProgramSummary now $ ProgramFamily p gds invs
 where
  instrumentProgramIds :: [Dataset] -> [Id InstrumentProgram]
  instrumentProgramIds ds = nub $ map (\d -> d.instrumentProgramId) ds

  numOtherIps :: [Dataset] -> Int
  numOtherIps dse = length (instrumentProgramIds dse) - 1

  experimentLink :: Dataset -> Int -> View c ()
  experimentLink d n = do
    el_ $ do
      text "Proposal - "
      route (Route.Proposal d.primaryProposalId Route.PropRoot) Style.link $ do
        text d.primaryProposalId.fromId
      text $
        if n > 0
          then [i|(#{n} other Instrument Programs)|]
          else ""


latestInversions :: (Inversions :> es, Globus :> es) => Id InstrumentProgram -> Eff es [Inversion]
latestInversions ip = fmap sortLatest <$> send $ Inversions.ByProgram ip
 where
  sortLatest :: [Inversion] -> [Inversion]
  sortLatest = sortOn (Down . (.updated))


inversionStep :: (Globus :> es, Tasks GenFits :> es) => Inversion -> Eff es CurrentStep
inversionStep inv = currentStep inv.proposalId inv.inversionId inv.step


data ProgramInversions = ProgramInversions (Id Proposal) (Id InstrumentProgram)
  deriving (Show, Read, ViewId)
instance HyperView ProgramInversions where
  type Action ProgramInversions = InvsAction
  type Require ProgramInversions = InversionStatus : InversionViews


data InvsAction
  = CreateInversion
  | ReloadAll
  deriving (Show, Read, ViewAction)


programInversions :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es, Tasks GenFits :> es) => ProgramInversions -> InvsAction -> Eff es (View ProgramInversions ())
programInversions (ProgramInversions ip iip) = \case
  CreateInversion -> do
    _ <- send $ Inversions.Create ip iip
    refreshInversions iip
  ReloadAll -> do
    refreshInversions iip


viewProgramInversions :: [Inversion] -> [CurrentStep] -> View ProgramInversions ()
viewProgramInversions (inv : is) (step : ss) = do
  hyper (InversionStatus inv.proposalId inv.programId inv.inversionId) $ viewInversion inv step
  col (gap 10 . pad 10) $ do
    zipWithM_ viewOldInversion is ss
    row id $ do
      button CreateInversion Style.link "Start Over With New Inversion"
viewProgramInversions _ _ = do
  button CreateInversion (Style.btn Primary) "Create Inversion"


viewOldInversion :: Inversion -> CurrentStep -> View c ()
viewOldInversion inv _ = row (gap 4) $ do
  el_ "•"
  link (routeUrl $ Route.Inversion inv.inversionId Route.Inv) Style.link $ do
    text inv.inversionId.fromId
  el_ $ text $ cs $ showDate inv.created
  el_ $ text $ inversionStatusLabel inv.step


refreshInversions :: (Inversions :> es, Globus :> es, Tasks GenFits :> es) => Id InstrumentProgram -> Eff es (View ProgramInversions ())
refreshInversions iip = do
  invs <- latestInversions iip
  steps <- mapM inversionStep invs
  pure $ viewProgramInversions invs steps


clearInversion :: Id Proposal -> Id InstrumentProgram -> Eff es (View InversionStatus ())
clearInversion ip iip = pure $ do
  target (ProgramInversions ip iip) $ onLoad ReloadAll 0 emptyButtonSpace
 where
  emptyButtonSpace = el (height 44) none

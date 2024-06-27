module App.Page.Proposal where

import App.Colors
import App.Globus
import App.Route as Route
import App.Style qualified as Style
import App.View.DatasetsTable as DatasetsTable
import App.View.Layout
import App.View.ProposalDetails
import Data.Grouped as G
import Effectful.Dispatch.Dynamic
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs as Programs
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole


page
  :: (Hyperbole :> es, Time :> es, Datasets :> es, Inversions :> es, Auth :> es)
  => Id Proposal
  -> Page es Response
page pid = do
  handle DatasetsTable.actionSort

  load $ do
    ds <- send $ Datasets.Query (ByProposal pid)
    ai <- send Inversions.All
    now <- currentTime
    let pwds = Programs.fromDatasets ai ds

    appLayout Proposals $ do
      col Style.page $ do
        el Style.header $ do
          text "Proposal  "
          text pid.fromId

        viewPrograms now pwds


-- DatasetsTable.datasetsTable ds

-- each InstrumentProgram MUST have datasets
viewPrograms :: UTCTime -> [WithDatasets] -> View c ()
viewPrograms _ [] = el_ "Not Found"
viewPrograms now (p : ps) = do
  let wds = Grouped (p :| ps) :: Grouped Proposal WithDatasets
  viewProposal now wds


viewProposal :: UTCTime -> Grouped Proposal WithDatasets -> View c ()
viewProposal now gx = do
  let wd = sample gx
  viewExperimentDescription wd.program.experimentDescription
  el Style.subheader $ do
    text "Instrument Programs"
  mapM_ (programSummary now) gx


programSummary :: UTCTime -> WithDatasets -> View c ()
programSummary now wdp = do
  col (gap 10) $ do
    col (bg White . gap 10 . pad 10) $ do
      route (Route.Proposal wdp.program.proposalId $ Route.Program wdp.program.programId) flexRow $ do
        viewProgramRow now wdp.program
      row (gap 10) $ do
        route (Route.Proposal wdp.program.proposalId $ Route.Program wdp.program.programId) Style.link $ do
          text wdp.program.programId.fromId
        space
        forM_ wdp.datasets.items $ \d -> do
          route (Route.Dataset d.datasetId) Style.link $ do
            text d.datasetId.fromId

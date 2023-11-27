module App.Page.Experiment where

import App.Colors
import App.Route
import App.View.DatasetsTable as DatasetsTable
import App.View.InstrumentProgramSummary as InstrumentProgramSummary
import Data.Grouped as G
import Effectful.Rel8
import NSO.Data.Dataset
import NSO.Data.Program as Program
import NSO.Data.Provenance as Provenance
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole
import Web.View

page :: (Hyperbole :> es, Rel8 :> es) => Id Experiment -> Page es ()
page eid = do
  hyper DatasetsTable.actionSort

  load $ do
    ds <- queryExperiment eid
    pv <- loadAllProvenance
    let pwds = Program.fromDatasets pv ds

    pure $ appLayout Experiments $ do
      col (pad 20 . gap 20) $ do
        el (fontSize 24 . bold) $ do
          text "Experiment  "
          text $ cs $ show eid

        viewPrograms pwds

-- DatasetsTable.datasetsTable ds

-- each InstrumentProgram MUST have datasets
viewPrograms :: [WithDatasets] -> View c ()
viewPrograms [] = el_ "Not Found"
viewPrograms (p : ps) = do
  let wds = Grouped (p :| ps) :: Grouped Experiment WithDatasets
  viewExperiment wds

viewExperiment :: Grouped Experiment WithDatasets -> View c ()
viewExperiment gx = do
  let wd = sample gx
  el_ $ text wd.program.experimentDescription
  mapM_ programSummary gx

programSummary :: WithDatasets -> View c ()
programSummary wdp = do
  col (bg White . gap 10 . pad 10) $ do
    row id $ do
      InstrumentProgramSummary.viewRow wdp.program
      space
      link (Program wdp.program.programId) (color Primary . bold) $ do
        text wdp.program.programId.fromId
    -- :: Grouped InstrumentProgram Dataset
    InstrumentProgramSummary.viewCriteria wdp.program wdp.datasets
    viewId (ProgramDatasets wdp.program.programId) $ do
      DatasetsTable.datasetsTable UpdateDate $ G.toList wdp.datasets

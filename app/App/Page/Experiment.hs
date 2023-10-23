module App.Page.Experiment where

import App.Colors
import App.Route
import App.View.DatasetsTable as DatasetsTable
import App.View.InstrumentProgramSummary as InstrumentProgramSummary
import Data.List.NonEmpty qualified as NE
import Effectful.Rel8
import NSO.Data.Dataset
import NSO.Data.Types
import NSO.Prelude
import Web.Hyperbole
import Web.UI

page :: (Page :> es, Rel8 :> es) => Id Experiment -> Eff es ()
page eid = do
  ds <- queryExperiment eid

  view $ appLayout Experiments $ do
    col (pad 20 . gap 20) $ do
      el (fontSize 24 . bold) $ do
        text "Experiment  "
        text $ cs $ show eid

      viewDatasets ds

-- DatasetsTable.datasetsTable ds

viewDatasets :: [Dataset] -> View c ()
viewDatasets [] = el_ "No Datasets?"
viewDatasets (d : ds) = do
  let dss = d :| ds
  let ips = toInstrumentPrograms dss

  el_ $ text d.experimentDescription

  mapM_ programSummary ips

programSummary :: InstrumentProgram -> View c ()
programSummary ip = do
  col (bg White . gap 10 . pad 10) $ do
    row (gap 10) $ InstrumentProgramSummary.viewRow ip
    InstrumentProgramSummary.viewCriteria ip
    DatasetsTable.datasetsTable $ NE.toList ip.datasets

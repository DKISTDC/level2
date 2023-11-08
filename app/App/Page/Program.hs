module App.Page.Program where

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

page :: (Page :> es, Rel8 :> es) => Id InstrumentProgram -> Eff es ()
page ipid = do
  ds <- queryProgram ipid

  view $ appLayout Experiments $ do
    col (pad 20 . gap 20) $ do
      el (fontSize 24 . bold) $ do
        text "Instrument Program  "
        text $ cs $ show ipid

      viewDatasets ds

viewDatasets :: [Dataset] -> View c ()
viewDatasets [] = el_ "No Datasets?"
viewDatasets (d : ds) = do
  let dss = d :| ds
  let ip = instrumentProgram dss
  el_ $ text d.experimentDescription

  col (bg White . gap 10) $ do
    row (gap 10 . pad 10 . textAlign Center . border (TRBL 0 0 1 0) . borderColor GrayLight) $ InstrumentProgramSummary.viewRow ip
    col (gap 10 . pad 10) $ do
      InstrumentProgramSummary.viewCriteria ip
      DatasetsTable.datasetsTable $ NE.toList ip.datasets

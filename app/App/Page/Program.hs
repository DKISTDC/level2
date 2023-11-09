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
  pageAction statusAction

  pageLoad $ do
    ds <- queryProgram ipid

    pure $ appLayout Experiments $ do
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
    row (pad 10 . gap 10 . textAlign Center . border (TRBL 0 0 1 0) . borderColor GrayLight) $ do
      InstrumentProgramSummary.viewRow ip

    col (gap 10 . pad 10) $ do
      liveView Status $ statusView
      InstrumentProgramSummary.viewCriteria ip
      DatasetsTable.datasetsTable $ NE.toList ip.datasets

data Status = Status
  deriving (Show, Read, Param)

data StatusAction
  = Queue
  | Complete
  deriving (Show, Read, Param)

instance LiveView Status StatusAction

statusAction :: (Page :> es, Rel8 :> es) => Status -> StatusAction -> Eff es (View Status ())
statusAction _ Queue = do
  pure $ el_ "Queued!"
statusAction _ Complete = do
  pure $ el_ "Completed!"

statusView :: View Status ()
statusView = do
  row (gap 10) $ do
    liveButton Queue btn "Queue"
    liveButton Complete btn "Complete"
 where
  btn = color White . pad (XY 15 10) . bg Secondary . hover (bg SecondaryLight)

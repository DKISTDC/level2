module App.Page.Program where

import App.Colors
import App.Route
import App.View.DatasetsTable as DatasetsTable
import App.View.InstrumentProgramSummary as InstrumentProgramSummary
import Data.Grouped as G
import Effectful.Rel8
import Effectful.Time
import NSO.Data.Dataset
import NSO.Data.Program
import NSO.Data.Provenance as Provenance
import NSO.Prelude
import Web.Hyperbole
import Web.UI

page :: (Page :> es, Time :> es, Rel8 :> es) => Id InstrumentProgram -> Eff es ()
page ip = do
  pageAction statusAction

  pageLoad $ do
    ds <- queryProgram ip
    ps <- Provenance.loadProvenance ip

    pure $ appLayout Experiments $ do
      col (pad 20 . gap 20) $ do
        el (fontSize 24 . bold) $ do
          text "Instrument Program: "
          text ip.fromId

        viewDatasets ds ps

viewDatasets :: [Dataset] -> [ProvenanceEntry] -> View c ()
viewDatasets [] _ = el_ "No Datasets?"
viewDatasets (d : ds) ps = do
  let gd = Grouped (d :| ds) :: Grouped InstrumentProgram Dataset
  let ip = instrumentProgram gd ps
  el_ $ text d.experimentDescription

  col (bg White . gap 10) $ do
    row (pad 10 . gap 10 . textAlign Center . border (TRBL 0 0 1 0) . borderColor GrayLight) $ do
      InstrumentProgramSummary.viewRow ip

    col (gap 10 . pad 10) $ do
      liveView (Status ip.programId) statusView
      InstrumentProgramSummary.viewCriteria ip gd
      DatasetsTable.datasetsTable $ G.toList gd

newtype Status = Status (Id InstrumentProgram)
  deriving newtype (Show, Read, Param)

data StatusAction
  = Queue
  | Complete
  deriving (Show, Read, Param)

instance LiveView Status StatusAction

statusAction :: (Time :> es, Page :> es, Rel8 :> es) => Status -> StatusAction -> Eff es (View Status ())
statusAction (Status ip) Queue = do
  -- TODO: higher level: mark an ip as queued but check to make sure it its valid first?
  Provenance.markQueued ip
  pure $ el_ "Queued!"
statusAction (Status ip) Complete = do
  Provenance.markInverted ip
  pure $ el_ "Inverted!"

statusView :: View Status ()
statusView = do
  row (gap 10) $ do
    liveButton Queue btn "Queue"
    liveButton Complete btn "Complete"
 where
  btn = color White . pad (XY 15 10) . bg Secondary . hover (bg SecondaryLight)

module App.Page.Program where

import App.Colors
import App.Route
import App.View.Common (showTimestamp)
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
import Web.View.Style (Align (Center))

page :: (Hyperbole :> es, Time :> es, Rel8 :> es) => Id InstrumentProgram -> Page es ()
page ip = do
  hyper statusAction
  hyper DatasetsTable.actionSort

  load $ do
    ds <- queryProgram ip
    ps <- Provenance.loadProvenance ip

    pure $ appLayout Experiments $ do
      col (pad 20 . gap 20) $ do
        el (fontSize 24 . bold) $ do
          text "Instrument Program: "
          text ip.fromId

        description ds

        col (bg White . gap 10) $ do
          viewDatasets (filter (.latest) ds) ps
          el (pad 10) $ viewId (ProgramDatasets ip) $ DatasetsTable.datasetsTable UpdateDate ds
 where
  description :: [Dataset] -> View c ()
  description [] = none
  description (d : _) = text d.experimentDescription

viewDatasets :: [Dataset] -> [ProvenanceEntry] -> View c ()
viewDatasets [] _ = none
viewDatasets (d : ds) ps = do
  let gd = Grouped (d :| ds)
  let ip = instrumentProgram gd ps

  row (pad 10 . gap 10 . textAlign Center . border (TRBL 0 0 1 0) . borderColor GrayLight) $ do
    InstrumentProgramSummary.viewRow ip

  col (pad 10 . gap 10) $ do
    viewId (Status ip.programId) statusView

    el bold "Provenance"
    mapM_ viewProvenanceEntry ps

    InstrumentProgramSummary.viewCriteria ip gd

viewProvenanceEntry :: ProvenanceEntry -> View c ()
viewProvenanceEntry (WasInverted p) = do
  row (gap 10) $ do
    el_ "Inverted"
    text $ showTimestamp p.completed
viewProvenanceEntry (WasQueued p) = do
  row (gap 10) $ do
    el_ "Queued"
    text $ showTimestamp p.completed

-- Status -----------------------------------------------

newtype Status = Status (Id InstrumentProgram)
  deriving newtype (Show, Read, Param)

data StatusAction
  = Queue
  | Complete
  deriving (Show, Read, Param)

instance HyperView Status where
  type Action Status = StatusAction

statusAction :: (Time :> es, Hyperbole :> es, Rel8 :> es) => Status -> StatusAction -> Eff es (View Status ())
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
    button Queue btn "Queue"
    button Complete btn "Complete"
 where
  btn = color White . pad (XY 15 10) . bg Secondary . hover (bg SecondaryLight)

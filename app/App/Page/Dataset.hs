module App.Page.Dataset where

import App.Colors
import App.Route
import App.View.Common (showTimestamp)
import App.View.DatasetsTable as DatasetsTable
import App.View.InstrumentProgramSummary as InstrumentProgramSummary
import Data.Aeson (ToJSON, encode)
import Data.Grouped as G
import Effectful.Rel8
import Effectful.Time
import NSO.Data.Dataset as Dataset
import NSO.Data.Program
import NSO.Data.Provenance as Provenance
import NSO.Prelude
import Web.Hyperbole
import Web.UI

page :: (Page :> es, Rel8 :> es) => Id Dataset -> Eff es ()
page di = pageLoad $ do
  ds <- Dataset.queryById di

  pure $ appLayout Experiments $ do
    col (pad 20 . gap 20) $ do
      el (fontSize 24 . bold) $ do
        text "Dataset: "
        text di.fromId

      mapM_ viewDataset ds

viewDataset :: Dataset -> View c ()
viewDataset d =
  col (bg White . gap 10 . pad 10) $ do
    field "Latest" $ DatasetsTable.latest d.latest
    field "Scan Date" $ text $ showTimestamp d.scanDate
    field "Instrument" $ text $ cs $ show d.instrument
    field "Instrument Program Id" $ link (routeUrl $ Program d.instrumentProgramId) (color Primary) $ text d.instrumentProgramId.fromId
    field "Experiment Id" $ link (routeUrl $ Experiment d.primaryExperimentId) (color Primary) $ text d.primaryExperimentId.fromId
    field "Proposal Id" $ text d.primaryProposalId.fromId
    field "Stokes Parameters" $ text $ cs $ show d.stokesParameters
    field "Create Date" $ text $ showTimestamp d.createDate
    field "Update Date" $ text $ showTimestamp d.updateDate
    field "Wavelength Min" $ text $ cs $ show d.wavelengthMin
    field "Wavelength Max" $ text $ cs $ show d.wavelengthMax
    field "Start Time" $ text $ showTimestamp d.startTime
    field "End Time" $ text $ showTimestamp d.endTime
    field "Frame Count" $ text $ cs $ show d.frameCount
    field "Exposure Time" $ text $ cs $ show d.exposureTime
    field "Bounding Box" $ boundingBox d.boundingBox
    field "AO Locked" $ text $ cs $ show d.aoLocked
    field "Health" $ json d.health
    field "GOS Status" $ json d.gosStatus
    field "Light Level" $ json d.lightLevel
    field "Polarimetric Accuracy" $ json d.polarimetricAccuracy
    field "friedParameter" $ json d.friedParameter

field :: Text -> View c () -> View c ()
field nm cnt =
  row (gap 10) $ do
    row (width 180) $ do
      space
      label bold (text nm)
    el_ cnt

boundingBox :: Maybe BoundingBox -> View c ()
boundingBox Nothing = none
boundingBox (Just b) = code $ cs $ show b

json :: (ToJSON a) => a -> View c ()
json a = code $ cs $ encode a

code :: Text -> View c ()
code = pre (fontSize 14)

--       description ds
--
--       col (bg White . gap 10) $ do
--         viewDatasets (filter (.latest) ds) ps
--         el (pad 10) $ liveView (ProgramDatasets ip) $ DatasetsTable.datasetsTable UpdateDate ds
--  where
--   description :: [Dataset] -> View c ()
--   description [] = none
--   description (d : _) = text d.experimentDescription
--
-- viewDatasets :: [Dataset] -> [ProvenanceEntry] -> View c ()
-- viewDatasets [] _ = none
-- viewDatasets (d : ds) ps = do
--   let gd = Grouped (d :| ds)
--   let ip = instrumentProgram gd ps
--
--   row (pad 10 . gap 10 . textAlign Center . border (TRBL 0 0 1 0) . borderColor GrayLight) $ do
--     InstrumentProgramSummary.viewRow ip
--
--   col (pad 10 . gap 10 . pad 10) $ do
--     liveView (Status ip.programId) statusView
--
--     el bold "Provenance"
--     mapM_ viewProvenanceEntry ps
--
--     InstrumentProgramSummary.viewCriteria ip gd
--
-- viewProvenanceEntry :: ProvenanceEntry -> View c ()
-- viewProvenanceEntry (WasInverted p) = do
--   row (gap 10) $ do
--     el_ "Inverted"
--     text $ showTimestamp p.completed
-- viewProvenanceEntry (WasQueued p) = do
--   row (gap 10) $ do
--     el_ "Queued"
--     text $ showTimestamp p.completed
--
-- newtype Status = Status (Id InstrumentProgram)
--   deriving newtype (Show, Read, Param)
--
-- data StatusAction
--   = Queue
--   | Complete
--   deriving (Show, Read, Param)
--
-- instance LiveView Status StatusAction
--
-- statusAction :: (Time :> es, Page :> es, Rel8 :> es) => Status -> StatusAction -> Eff es (View Status ())
-- statusAction (Status ip) Queue = do
--   -- TODO: higher level: mark an ip as queued but check to make sure it its valid first?
--   Provenance.markQueued ip
--   pure $ el_ "Queued!"
-- statusAction (Status ip) Complete = do
--   Provenance.markInverted ip
--   pure $ el_ "Inverted!"
--
-- statusView :: View Status ()
-- statusView = do
--   row (gap 10) $ do
--     liveButton Queue btn "Queue"
--     liveButton Complete btn "Complete"
--  where
--   btn = color White . pad (XY 15 10) . bg Secondary . hover (bg SecondaryLight)

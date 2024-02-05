module App.Page.Program where

import App.Colors
import App.Error
import App.Route
import App.Style qualified as Style
import App.View.DatasetsTable as DatasetsTable
import App.View.ExperimentDetails
import Data.Grouped as G
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Effectful.Error.Static
import Effectful.Rel8
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs
import NSO.Data.Provenance as Provenance
import NSO.Prelude
import Web.Hyperbole
import Web.View.Style (Align (Center))


page
  :: (Hyperbole :> es, Time :> es, Rel8 :> es, Error AppError :> es)
  => Id InstrumentProgram
  -> Page es ()
page ip = do
  hyper statusAction
  hyper DatasetsTable.actionSort

  load $ do
    ds' <- queryProgram ip
    ds <- expectFound ds'
    let d = head ds

    dse <- Datasets.queryExperiment d.primaryExperimentId
    is <- Inversions.queryInstrumentProgram ip
    now <- currentTime

    pure $ appLayout Experiments $ do
      col Style.page $ do
        col (gap 5) $ do
          el Style.header $ do
            text "Instrument Program: "
            text ip.fromId

          el_ $ do
            text "Experiment: "
            link (Experiment d.primaryExperimentId) Style.link $ do
              text d.primaryExperimentId.fromId

        -- viewExperimentDescription d.experimentDescription

        col (gap 10) $ do
          col (bg White . gap 10) $ do
            viewDatasets now (NE.filter (.latest) ds) is
            el (pad 10) $ viewId (ProgramDatasets ip) $ DatasetsTable.datasetsTable Latest (NE.toList ds)

        case instrumentProgramIds dse of
          [] -> none
          [_] -> none
          ips -> do
            link (Experiment d.primaryExperimentId) Style.link $ do
              text "View "
              text $ cs $ show (length ips - 1)
              text " other Instrument Programs in this Experiment"
 where
  instrumentProgramIds :: [Dataset] -> [Id InstrumentProgram]
  instrumentProgramIds ds = nub $ map (\d -> d.instrumentProgramId) ds

  expectFound :: (Hyperbole :> es) => [a] -> Eff es (NonEmpty a)
  expectFound [] = notFound
  expectFound (a : as) = pure $ a :| as


viewDatasets :: UTCTime -> [Dataset] -> [Inversion] -> View c ()
viewDatasets _ [] _ = none
viewDatasets now (d : ds) is = do
  let gd = Grouped (d :| ds)
  let ip = instrumentProgram gd is

  row (pad 10 . gap 10 . textAlign Center . border (TRBL 0 0 1 0) . borderColor GrayLight) $ do
    viewProgramRow now ip

  col (pad 10 . gap 10) $ do
    viewId (Status ip.programId) statusView

    -- el bold "Provenance"
    -- mapM_ viewProvenanceEntry ps

    viewCriteria ip gd


-- viewProvenanceEntry :: ProvenanceEntry -> View c ()
-- viewProvenanceEntry (WasInverted p) = do
--   row (gap 10) $ do
--     el_ "Inverted"
--     text $ showTimestamp p.completed
-- viewProvenanceEntry (WasQueued p) = do
--   row (gap 10) $ do
--     el_ "Queued"
--     text $ showTimestamp p.completed

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

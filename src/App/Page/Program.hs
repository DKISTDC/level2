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
import Data.String.Interpolate (i)
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Rel8
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs
import NSO.Prelude
import Web.Hyperbole
import Web.View.Style (Align (Center))


page
  :: (Hyperbole :> es, Time :> es, Rel8 :> es, GenRandom :> es, Error AppError :> es)
  => Id InstrumentProgram
  -> Page es ()
page ip = do
  hyper inversions
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

          experimentLink d (numOtherIps dse)

        -- viewExperimentDescription d.experimentDescription

        viewId (Inversions ip) $ viewInversions is

        col (bg White . gap 10) $ do
          viewDatasets now (NE.filter (.latest) ds) is
          el (pad 10) $ viewId (ProgramDatasets ip) $ DatasetsTable.datasetsTable Latest (NE.toList ds)
 where
  instrumentProgramIds :: [Dataset] -> [Id InstrumentProgram]
  instrumentProgramIds ds = nub $ map (\d -> d.instrumentProgramId) ds

  expectFound :: (Hyperbole :> es) => [a] -> Eff es (NonEmpty a)
  expectFound [] = notFound
  expectFound (a : as) = pure $ a :| as

  numOtherIps :: [Dataset] -> Int
  numOtherIps dse = length (instrumentProgramIds dse) - 1

  experimentLink :: Dataset -> Int -> View c ()
  experimentLink d n = do
    el_ $ do
      text "Experiment: "
      link (Experiment d.primaryExperimentId) Style.link $ do
        text d.primaryExperimentId.fromId
      text
        $ if n > 0
          then [i|(#{n} other Instrument Programs)|]
          else ""


viewDatasets :: UTCTime -> [Dataset] -> [Inversion] -> View c ()
viewDatasets _ [] _ = none
viewDatasets now (d : ds) is = do
  let gd = Grouped (d :| ds)
  let ip = instrumentProgram gd is

  row (pad 10 . gap 10 . textAlign Center . border (TRBL 0 0 1 0) . borderColor (light Secondary)) $ do
    viewProgramRow now ip

  col (pad 10 . gap 10) $ do
    -- viewId (Status ip.programId) statusView

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

newtype Inversions = Inversions (Id InstrumentProgram)
  deriving newtype (Show, Read, Param)


data InversionsAction
  = CreateInversion
  deriving (Show, Read, Param)


instance HyperView Inversions where
  type Action Inversions = InversionsAction


inversions :: (Hyperbole :> es, Rel8 :> es, Time :> es, GenRandom :> es) => Inversions -> InversionsAction -> Eff es (View Inversions ())
inversions (Inversions ip) CreateInversion = do
  inv <- Inversions.create ip
  pure $ viewInversion inv


viewInversions :: [Inversion] -> View Inversions ()
viewInversions [] = do
  button CreateInversion (Style.btn Primary) "Create Inversion"
viewInversions is = mapM_ viewInversion is


viewInversion :: Inversion -> View Inversions ()
viewInversion inv = do
  col (bg White . gap 10) $ do
    el_ "INVERSION: "
    el_ $ text inv.inversionId.fromId

--
--
-- statusView :: View Status ()
-- statusView = do
--   row (gap 10) $ do
--     button Queue btn "Queue"
--     button Complete btn "Complete"
--  where
--   btn = color White . pad (XY 15 10) . bg Secondary . hover (bg SecondaryLight)

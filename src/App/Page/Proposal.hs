{-# LANGUAGE UndecidableInstances #-}

module App.Page.Proposal where

import App.Colors
import App.Effect.Auth
import App.Error (expectFound)
import App.Route as Route
import App.Style qualified as Style
import App.View.Datasets as DatasetsTable
import App.View.Icons (skeleton)
import App.View.Layout
import App.View.ProposalDetails
import Data.Grouped
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs as Programs
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import Web.Atomic.CSS
import Web.Hyperbole


page
  :: (Hyperbole :> es, Time :> es, Datasets :> es, Inversions :> es, Auth :> es)
  => Id Proposal
  -> Page es '[Programs, ProgramSummary]
page propId = do
  ds <- Datasets.find (Datasets.ByProposal propId) >>= expectFound
  fs <- query
  appLayout Proposals $ do
    col ~ Style.page $ do
      el ~ Style.header $ do
        text "Proposal - "
        text propId.fromId

      viewExperimentDescription (head ds).experimentDescription

      el ~ Style.subheader $ text "Instrument Programs"

      hyper (Programs propId) $ viewPrograms fs (grouped (.instrumentProgramId) $ NE.toList ds)


data Filters = Filters
  { term :: Text
  }
  deriving (Generic, ToQuery, FromQuery)


----------------------------------------------------
-- Programs
----------------------------------------------------

data Programs = Programs (Id Proposal)
  deriving (Generic, ViewId)


instance (Datasets :> es, Time :> es, Inversions :> es) => HyperView Programs es where
  data Action Programs
    = SearchTerm Text
    deriving (Generic, ViewAction)


  type Require Programs = '[ProgramSummary]


  update = \case
    SearchTerm t -> do
      Programs propId <- viewId
      let fs = Filters t
      setQuery fs
      ds <- Datasets.find (Datasets.ByProposal propId)
      pure $ viewPrograms fs $ grouped (.instrumentProgramId) ds


-- filterProgramDatasets [dsById] _ = [dsById]
-- filterProgramDatasets [] ds = ds

viewPrograms :: Filters -> [Group (Id InstrumentProgram) Dataset] -> View Programs ()
viewPrograms fs gds = do
  Programs propId <- viewId
  col ~ gap 25 $ do
    search SearchTerm 250 ~ Style.input @ att "placeholder" "search: BEEMM"
    forM_ (filter (isMatch fs.term) gds) $ \ds -> do
      let d = sample ds
      hyper (ProgramSummary propId d.instrumentProgramId) viewProgramSummaryLoad
 where
  isMatch :: Text -> Group (Id InstrumentProgram) Dataset -> Bool
  isMatch t g =
    any (\d -> t `T.isInfixOf` d.datasetId.fromId) g.items


----------------------------------------------------
-- ProgramSummary
----------------------------------------------------

data ProgramSummary = ProgramSummary (Id Proposal) (Id InstrumentProgram)
  deriving (Generic, ViewId)


instance (Datasets :> es, Time :> es, Inversions :> es) => HyperView ProgramSummary es where
  data Action ProgramSummary
    = ProgramDetails SortField
    deriving (Generic, ViewAction)


  update = \case
    ProgramDetails srt -> do
      ProgramSummary _ progId <- viewId
      now <- currentTime
      progs <- Programs.loadProgram progId
      pure $ mapM_ (viewProgramSummary srt now) progs


viewProgramSummaryLoad :: View ProgramSummary ()
viewProgramSummaryLoad = do
  programCard @ onLoad (ProgramDetails ByLatest) 0 $ do
    el ~ pad 20 . width 600 $ skeleton


viewProgramSummary :: SortField -> UTCTime -> ProgramFamily -> View ProgramSummary ()
viewProgramSummary srt now pf = do
  let ds = pf.datasets.items
  programCard $ do
    viewProgramDetails pf now pf.datasets
    col ~ pad (TRBL 0 15 15 15) $ do
      DatasetsTable.datasetsTable ProgramDetails srt (NE.toList ds)


programCard :: View ProgramSummary () -> View ProgramSummary ()
programCard cnt = do
  ProgramSummary propId progId <- viewId
  col ~ Style.card . minHeight 200 $ do
    appRoute (Route.Proposal propId $ Program progId Prog) ~ Style.cardHeader Secondary $ text $ "Instrument Program - " <> progId.fromId
    cnt

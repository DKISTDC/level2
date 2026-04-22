{-# LANGUAGE UndecidableInstances #-}

module App.Page.Proposal where

import App.Colors
import App.Effect.Auth
import App.Effect.Transfer
import App.Error (expectFound)
import App.Page.Program (viewProgramDetails)
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
import NSO.Data.Proposals as Proposals
import NSO.Files (Ingest, Level1)
import NSO.Files.RemoteFolder (Remote)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import Web.Atomic.CSS
import Web.Hyperbole


page
  :: (Hyperbole :> es, Time :> es, Programs :> es, Datasets :> es, Auth :> es)
  => Id Proposal
  -> Page es '[ProgramSummary]
page propId = do
  progs <- send (Programs.ByProposal propId) >>= expectFound
  prop <- Proposals.lookupProposal propId >>= fmap head . expectFound
  appLayout Proposals $ do
    col ~ Style.page $ do
      el ~ Style.header $ do
        text "Proposal - "
        text propId.fromId

      viewExperimentDescription prop.dataset.experimentDescription

      el ~ Style.subheader $ text "Instrument Programs"

      col ~ gap 25 $ do
        forM_ progs $ \prog -> do
          hyper (ProgramSummary propId prog.programId) viewProgramSummaryLoad


data Filters = Filters
  { term :: Text
  }
  deriving (Generic, ToQuery, FromQuery)


----------------------------------------------------
-- ProgramSummary
----------------------------------------------------

data ProgramSummary = ProgramSummary (Id Proposal) (Id InstrumentProgram)
  deriving (Generic, ViewId)


instance (Datasets :> es, Time :> es, Programs :> es, Transfer Level1 Ingest :> es) => HyperView ProgramSummary es where
  data Action ProgramSummary
    = ProgramDetails SortField
    | GenIronImage
    deriving (Generic, ViewAction)


  update = \case
    ProgramDetails srt -> do
      ProgramSummary _ progId <- viewId
      now <- currentTime
      prog <- fmap head . expectFound =<< send (Programs.Lookup progId)
      ds <- Datasets.findLatest (Datasets.ByProgram progId) >>= expectFound
      l1 <- send (RemoteSource @Level1 @Ingest)
      pure $ viewProgramSummary l1 srt now prog ds
    GenIronImage -> pure none


viewProgramSummaryLoad :: View ProgramSummary ()
viewProgramSummaryLoad = do
  programCard @ onLoad (ProgramDetails ProductId) 0 $ do
    el ~ pad 20 . width 600 $ skeleton


viewProgramSummary :: Remote Level1 -> SortField -> UTCTime -> InstrumentProgram -> NonEmpty Dataset -> View ProgramSummary ()
viewProgramSummary l1 srt now prog ds = do
  programCard $ do
    viewProgramDetails prog ds now
    col ~ pad (TRBL 0 15 15 15) $ do
      DatasetsTable.datasetsTable l1 ProgramDetails srt (NE.toList ds)


programCard :: View ProgramSummary () -> View ProgramSummary ()
programCard cnt = do
  ProgramSummary propId progId <- viewId
  col ~ Style.card . minHeight 200 $ do
    appRoute (Route.Proposal propId $ Program progId Prog) ~ Style.cardHeader Secondary $ text $ "Instrument Program - " <> progId.fromId
    cnt

{-# LANGUAGE UndecidableInstances #-}

module App.Page.Proposal where

import App.Colors
import App.Effect.Auth
import App.Error (expectFound)
import App.Route as Route
import App.Style qualified as Style
import App.View.DatasetsTable as DatasetsTable
import App.View.Layout
import App.View.ProposalDetails
import Data.Grouped (Grouped (..))
import Data.List.NonEmpty qualified as NE
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs as Programs
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole


page
  :: (Hyperbole :> es, Time :> es, Datasets :> es, Inversions :> es, Auth :> es)
  => Id Proposal
  -> Eff es (Page '[ProgramSummary])
page propId = do
  ds <- Datasets.find (Datasets.DistinctPrograms propId) >>= expectFound

  appLayout Proposals $ do
    col Style.page $ do
      el Style.header $ do
        text "Proposal - "
        text propId.fromId

      viewExperimentDescription (head ds).experimentDescription

      el Style.subheader $ text "Instrument Programs"

      forM_ ds $ \d -> do
        hyper (ProgramSummary propId d.instrumentProgramId) viewProgramSummaryLoad


----------------------------------------------------
-- ProgramSummary
----------------------------------------------------

data ProgramSummary = ProgramSummary (Id Proposal) (Id InstrumentProgram)
  deriving (Show, Read, ViewId)


instance (Datasets :> es, Time :> es, Inversions :> es) => HyperView ProgramSummary es where
  data Action ProgramSummary
    = ProgramDetails SortField
    deriving (Show, Read, ViewAction)


  update = \case
    ProgramDetails srt -> do
      ProgramSummary _ progId <- viewId
      now <- currentTime
      progs <- Programs.loadProgram progId
      pure $ mapM_ (viewProgramSummary srt now) progs


viewProgramSummaryLoad :: View ProgramSummary ()
viewProgramSummaryLoad = do
  el (onLoad (ProgramDetails ByLatest) 100) ""


viewProgramSummary :: SortField -> UTCTime -> ProgramFamily -> View ProgramSummary ()
viewProgramSummary srt now pf = do
  let ds = pf.datasets.items
  let prog = pf.program :: InstrumentProgram
  col Style.card $ do
    route (Route.Proposal prog.proposalId $ Program prog.programId Prog) (Style.cardHeader Secondary) $ text $ "Instrument Program - " <> prog.programId.fromId
    viewProgramDetails pf now pf.datasets
    col (pad (TRBL 0 15 15 15)) $ do
      DatasetsTable.datasetsTable ProgramDetails srt (NE.toList ds)

{-# LANGUAGE UndecidableInstances #-}

module App.Page.Proposal where

import App.Effect.Auth
import App.Error (expectFound)
import App.Route as Route
import App.Style qualified as Style
import App.View.DatasetsTable as DatasetsTable
import App.View.Layout
import App.View.ProposalDetails
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
  -> Eff es (Page '[ProgramDatasets, ProgramSummary])
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


-- viewPrograms :: (HyperViewHandled ProgramDatasets c) => UTCTime -> [ProgramFamily] -> View c ()
-- viewPrograms _ [] = el_ "Not Found"
-- viewPrograms now (p : ps) = do
--   let wds = Grouped (p :| ps) :: Grouped Proposal ProgramFamily
--   viewProposal now wds

-- viewProposal :: (HyperViewHandled ProgramDatasets c) => UTCTime -> Grouped Proposal ProgramFamily -> View c ()
-- viewProposal now gx = do
--   let pf = sample gx

----------------------------------------------------
-- ProgramSummary
----------------------------------------------------

data ProgramSummary = ProgramSummary (Id Proposal) (Id InstrumentProgram)
  deriving (Show, Read, ViewId)


instance (Datasets :> es, Time :> es, Inversions :> es) => HyperView ProgramSummary es where
  data Action ProgramSummary
    = ProgramDetails
    deriving (Show, Read, ViewAction)


  type Require ProgramSummary = '[ProgramDatasets]


  update ProgramDetails = do
    ProgramSummary _ progId <- viewId
    ds <- Datasets.find (Datasets.ByProgram progId)
    now <- currentTime
    invs <- findInversionsByProgram progId
    let progs = programFamilies invs ds
    pure $ do
      mapM_ (viewProgramSummary now) progs


viewProgramSummaryLoad :: View ProgramSummary ()
viewProgramSummaryLoad = do
  el (onLoad ProgramDetails 100) ""

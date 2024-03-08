module App.Page.Program where

import App.Colors
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Inversion
import App.Page.Inversions.InvForm
import App.Route
import App.Style qualified as Style
import App.Types (App (..))
import App.View.Common as View
import App.View.DatasetsTable as DatasetsTable
import App.View.Layout
import App.View.ProposalDetails
import Data.Grouped as G
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs
import NSO.Prelude
import Web.Hyperbole


page
  :: (Hyperbole :> es, Time :> es, Datasets :> es, Inversions :> es, Layout :> es, Reader App :> es)
  => Id InstrumentProgram
  -> Page es Response
page ip = do
  hyper $ inversions (refreshInversions ip)
  hyper DatasetsTable.actionSort
  hyper inversionCommit

  load $ do
    ds' <- send $ Datasets.Query (Datasets.ByProgram ip)
    ds <- expectFound ds'
    let d = head ds

    dse <- send $ Datasets.Query (ByProposal d.primaryProposalId)
    invs <- send $ Inversions.ByProgram ip
    steps <- mapM (currentStep . (.step)) invs
    now <- currentTime

    appLayout Proposals $ do
      col (Style.page . gap 30) $ do
        col (gap 5) $ do
          el Style.header $ do
            text "Instrument Program: "
            text ip.fromId

          experimentLink d (numOtherIps dse)

        -- viewExperimentDescription d.experimentDescription

        viewId (InversionStatus ip) $ viewInversions invs steps

        col Style.card $ do
          el (Style.cardHeader Secondary) "Instrument Program Details"
          col (gap 15 . pad 15) $ do
            viewDatasets now (NE.filter (.latest) ds) invs
            viewId (ProgramDatasets ip) $ DatasetsTable.datasetsTable ByLatest (NE.toList ds)
 where
  instrumentProgramIds :: [Dataset] -> [Id InstrumentProgram]
  instrumentProgramIds ds = nub $ map (\d -> d.instrumentProgramId) ds

  numOtherIps :: [Dataset] -> Int
  numOtherIps dse = length (instrumentProgramIds dse) - 1

  experimentLink :: Dataset -> Int -> View c ()
  experimentLink d n = do
    el_ $ do
      text "Proposal: "
      link (Proposal d.primaryProposalId) Style.link $ do
        text d.primaryProposalId.fromId
      text $
        if n > 0
          then [i|(#{n} other Instrument Programs)|]
          else ""


viewDatasets :: UTCTime -> [Dataset] -> [Inversion] -> View c ()
viewDatasets _ [] _ = none
viewDatasets now (d : ds) is = do
  let gd = Grouped (d :| ds)
  let ip = instrumentProgram gd is

  row (textAlign Center) $ do
    viewProgramRow now ip

  View.hr (color Gray)

  viewCriteria ip gd


viewInversions :: [Inversion] -> [CurrentStep] -> View InversionStatus ()
viewInversions [] [] = do
  button CreateInversion (Style.btn Primary) "Create Inversion"
viewInversions is ss =
  col (gap 20) $ do
    zipWithM_ viewInversion is ss


refreshInversions :: (Inversions :> es, Globus :> es) => Id InstrumentProgram -> Eff es (View InversionStatus ())
refreshInversions ip = do
  invs <- send $ Inversions.ByProgram ip
  steps <- mapM (currentStep . (.step)) invs
  pure $ viewInversions invs steps

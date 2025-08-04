module App.Page.Inversions where

import App.Effect.Auth
import App.Page.Proposals qualified as Proposals
import App.Route as Route
import App.Style qualified as Style
import App.View.DataRow (dataRows)
import App.View.Inversion (rowInversion)
import App.View.Layout
import Data.Grouped
import Data.List.NonEmpty qualified as NE
import Data.Ord (Down (..))
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs as Programs
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Status
import Web.Atomic.CSS
import Web.Hyperbole


page :: (Log :> es, Hyperbole :> es, Inversions :> es, Auth :> es, Datasets :> es) => Eff es (Page '[])
page = do
  AllInversions ivs <- send Inversions.All
  props <- Programs.loadProposalPrograms
  -- let actv = groupProposals props $ filter activeInvs ivs
  -- let cmpl = groupProposals props $ filter completeInvs ivs
  let allPropInvs = groupProposals props ivs

  -- let sorted = sortOn sortInv ivs
  appLayout Inversions $ do
    col ~ Style.page $ do
      -- el (fontSize 24 . bold) "Completed"
      viewProposals allPropInvs
 where
  mostRecentlyUpdated :: ProposalInversions -> Down UTCTime
  mostRecentlyUpdated p =
    let allInvs = mconcat $ fmap (NE.toList . (.items)) p.programInversions :: [Inversion]
     in Down $ maximum $ fmap (.updated) allInvs

  groupProposals :: [ProposalPrograms] -> [Inversion] -> [ProposalInversions]
  groupProposals props invs =
    sortOn mostRecentlyUpdated $ filter hasInversions $ fmap proposalInversions props
   where
    proposalInversions :: ProposalPrograms -> ProposalInversions
    proposalInversions p =
      let inversions = filter (\inv -> p.proposal.proposalId == inv.proposalId) invs :: [Inversion]
       in ProposalInversions
            { programInversions = groupPrograms inversions
            , proposal = p.proposal
            }

    groupPrograms :: [Inversion] -> [Group (Id InstrumentProgram) Inversion]
    groupPrograms = grouped (.programId)

    hasInversions p =
      case p.programInversions of
        [] -> False
        _ -> True


data ProposalInversions = ProposalInversions
  { proposal :: Proposal
  , programInversions :: [Group (Id InstrumentProgram) Inversion]
  }


viewProposals :: [ProposalInversions] -> View id ()
viewProposals pis = do
  col ~ gap 20 $ do
    mapM_ viewProposal pis


viewProposal :: ProposalInversions -> View id ()
viewProposal p = do
  -- what's the instrument program id though?
  Proposals.proposalCard p.proposal $ do
    mapM_ viewProgram p.programInversions


viewProgram :: Group (Id InstrumentProgram) Inversion -> View id ()
viewProgram g = do
  let inv = sample g
  let sorted = sortOn (Down . (.updated)) $ NE.toList g.items :: [Inversion]
  col ~ gap 10 $ do
    appRoute (Route.Proposal inv.proposalId $ Route.Program inv.programId Prog) ~ Style.link $ do
      text inv.programId.fromId
    col $ do
      dataRows sorted rowInversion

-- viewByProposal :: Proposal -> [Inversion] -> View id ()
-- viewByProposal _ [] = none
-- viewByProposal p invs = none

-- viewInversions :: [Inversion] -> View c ()
-- viewInversions invs = do
--   table View.table invs $ do
--     tcol (hd "Status") $ \inv -> View.cell $ route (routeInv inv) (color $ statusColor inv) $ inversionStatusTag inv
--     tcol (hd "Proposal") $ \inv -> cellLink (Route.Proposal inv.proposalId PropRoot) inv.proposalId
--     tcol (hd "Program") $ \inv -> cellLink (Route.Proposal inv.proposalId (Route.Program inv.programId)) inv.programId
--     tcol (hd "Inversion") $ \inv -> cellLink (routeInv inv) inv.inversionId
--     tcol (hd "Created") $ \inv -> View.cell $ text $ cs $ showDate inv.created
--     tcol (hd "Updated") $ \inv -> View.cell $ text $ cs $ showDate inv.updated
--  where
--   hd = View.hd
--   routeInv inv = Route.Proposal inv.proposalId $ Route.Inversion inv.inversionId Inv
--
--   cellLink r i =
--     View.cell $ route r Style.link $ pre id i.fromId
--
--   statusColor i =
--     case i.invError of
--       Just _ -> Danger
--       _ -> Black

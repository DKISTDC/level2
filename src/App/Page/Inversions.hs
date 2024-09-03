module App.Page.Inversions where

import App.Colors
import App.Effect.Auth
import App.Route as Route
import App.Style qualified as Style
import App.View.Common (showDate)
import App.View.DataRow qualified as View
import App.View.Inversions (inversionStatusTag)
import App.View.Layout
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common
import Web.Hyperbole


page :: (Hyperbole :> es, Inversions :> es, Auth :> es) => Page es Response
page = do
  load $ do
    AllInversions ivs <- send Inversions.All
    let sorted = sortOn sortInv ivs
    appLayout Inversions $ do
      col Style.page $ do
        col Style.card $ do
          el (Style.cardHeader Info) "Active"
          col section $ do
            viewInversions (filter isActive sorted)

        el (fontSize 24 . bold) "Completed"
        col Style.card $ do
          col section $ do
            viewInversions (filter (not . isActive) sorted)
 where
  section = gap 10 . pad 10
  sortInv i = (i.proposalId, i.updated)


viewInversions :: [Inversion] -> View c ()
viewInversions invs = do
  table View.table invs $ do
    tcol (hd "Status") $ \inv -> View.cell $ route (routeInv inv) (color $ statusColor inv) $ inversionStatusTag inv
    tcol (hd "Proposal") $ \inv -> cellLink (Route.Proposal inv.proposalId PropRoot) inv.proposalId
    tcol (hd "Program") $ \inv -> cellLink (Route.Proposal inv.proposalId (Route.Program inv.programId)) inv.programId
    tcol (hd "Inversion") $ \inv -> cellLink (routeInv inv) inv.inversionId
    tcol (hd "Created") $ \inv -> View.cell $ text $ cs $ showDate inv.created
    tcol (hd "Updated") $ \inv -> View.cell $ text $ cs $ showDate inv.updated
 where
  hd = View.hd
  routeInv inv = Route.Proposal inv.proposalId $ Route.Inversion inv.inversionId Inv

  cellLink r i =
    View.cell $ route r Style.link $ pre id i.fromId

  statusColor i =
    case i.invError of
      Just _ -> Danger
      _ -> Black

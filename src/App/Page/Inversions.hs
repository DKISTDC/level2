module App.Page.Inversions where

import App.Colors
import App.Globus
import App.Route as Route
import App.Style qualified as Style
import App.View.Common (showDate)
import App.View.DataRow (dataRows)
import App.View.Inversions (inversionStatusLabel)
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
    appLayout Inversions $ do
      col Style.page $ do
        col Style.card $ do
          el (Style.cardHeader Info) "Active"
          col section $ do
            dataRows (filter isActive ivs) $ \iv ->
              viewInversion iv

        el (fontSize 24 . bold) "Completed"
        col Style.card $ do
          col section $ do
            dataRows (filter (not . isActive) ivs) $ \iv ->
              viewInversion iv
 where
  section = gap 10 . pad 10


isActive :: Inversion -> Bool
isActive inv =
  case inv.step of
    StepPublished _ -> False
    _ -> True


viewInversion :: Inversion -> View c ()
viewInversion inv = do
  -- TODO: Show more detailed status for each one... which step are we on?
  -- what are the dates that things have been happening
  row (gap 10) $ do
    -- route (Route.Program inv.programId) Style.link $
    --   text inv.programId.fromId
    route (Route.Inversion inv.inversionId Inv) Style.link $
      pre id inv.inversionId.fromId
    el_ $ text $ cs $ showDate inv.created
    el_ $ text $ inversionStatusLabel inv.step

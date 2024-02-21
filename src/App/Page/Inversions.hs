module App.Page.Inversions where

import App.Colors
import App.Route
import App.Style qualified as Style
import App.View.Layout
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common
import Web.Hyperbole


page :: (Hyperbole :> es, Inversions :> es, Layout :> es) => Page es Response
page = do
  load $ do
    AllInversions ivs <- send Inversions.All
    appLayout Inversions $ do
      col Style.page $ do
        col Style.card $ do
          el (Style.cardHeader Info) "Active"
          col section $ do
            mapM_ viewInversion $ filter isActive ivs

        el (fontSize 24 . bold) "Completed"
        col Style.card $ do
          -- el (Style.cardHeader Success) "Completed"
          col section $ do
            mapM_ viewInversion $ filter (not . isActive) ivs
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
  col (Style.card . gap 10) $ do
    link (Program inv.programId) Style.link $
      text inv.inversionId.fromId
    pre id $ cs (show inv.step)

module App.View.Layout where

import App.Colors
import App.Route
import Effectful
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole hiding (content)


appLayout :: forall es c. (Hyperbole :> es) => AppRoute -> View c () -> Eff es (View c ())
appLayout r content = do
  pure $ layout r content


layout :: AppRoute -> View c () -> View c ()
layout rc content = do
  col ~ color Black . grow $ do
    nav ~ gap 0 . bg Primary . color White . topbar $ do
      row ~ pad 15 $ do
        space
        route Dashboard ~ bold . fontSize 24 . pad (XY 20 0) $ "NSO Level 2"
        space
      -- nav Dashboard "Dashboard"
      item Proposals "Proposals"
      item Inversions "Inversions"
      item (Datasets DatasetRoot) "Datasets"
      space
      route Logout ~ center $ "Log Out"

    col ~ grow . overflow Scroll $ do
      -- el_ $ text $ cs $ show tok
      content
 where
  item r =
    route
      r
      ~ (center . hover (borderColor White . color White) . if r == rc then current else other)

  center = pad 20

  current = bg (light Primary) . borderColor Primary
  other = borderColor Primary
  topbar = height 70 . flexRow

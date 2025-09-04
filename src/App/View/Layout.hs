module App.View.Layout where

import App.Colors
import App.Effect.Auth
import App.Route
import App.Style qualified as Style
import Effectful
import Effectful.Globus
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole hiding (content)


appLayout :: forall es c. (Auth :> es, Hyperbole :> es) => AppRoute -> View c () -> Eff es (View c ())
appLayout r content = do
  login <- loginUrl
  saveCurrentUrl
  mtok <- getAccessToken
  madmin <- send AdminToken

  pure $ layout login r mtok madmin content


layout :: URI -> AppRoute -> Maybe (Token Access) -> Maybe (Token Access) -> View c () -> View c ()
layout login rc tok madmin content = do
  col ~ color Black . grow $ do
    nav ~ gap 0 . bg Primary . color White . topbar $ do
      row ~ pad 15 $ do
        space
        route Dashboard ~ bold . fontSize 24 . pad (XY 20 0) $ "Level 2"
        space
      -- nav Dashboard "Dashboard"
      item Proposals "Proposals"
      item Inversions "Inversions"
      item (Datasets DatasetRoot) "Datasets"
      space
      adminLink
      loginLink tok
    -- WebView.link login center "Login"

    col ~ grow . overflow Scroll $ do
      -- el_ $ text $ cs $ show tok
      content
 where
  item r =
    route
      r
      ~ (center . hover (borderColor White . color White) . if r == rc then current else other)

  loginLink Nothing =
    link login ~ center $ "Login"
  loginLink (Just _) =
    route Logout ~ center $ "Log Out"

  adminLink = do
    case madmin of
      Nothing -> do
        row ~ pad 10 $ do
          link login ~ Style.btn Warning $ "Needs Globus Login"
      Just _ -> pure ()

  center = pad 20

  current = bg (light Primary) . borderColor Primary
  other = borderColor Primary
  topbar = height 70 . flexRow

module App.View.Layout where

import App.Colors
import App.Globus as Globus
import App.Route
import NSO.Prelude
import Web.Hyperbole hiding (layout)
import Web.View qualified as WebView


type Layout = Globus


appLayout :: forall es c. (Globus :> es, Hyperbole :> es) => AppRoute -> View c () -> Eff es (View c ())
appLayout r content = do
  login <- authUrl
  mtok <- fmap Token <$> session "globus"
  pure $ layout login r mtok content


layout :: Url -> AppRoute -> Maybe (Token Access) -> View c () -> View c ()
layout login rc tok content = do
  WebView.layout (color Black . flexCol) $ do
    nav (gap 0 . bg Primary . color White . topbar) $ do
      row (pad 15) $ do
        space
        link Dashboard (bold . fontSize 24 . pad (XY 20 0)) "Level 2"
        space
      -- nav Dashboard "Dashboard"
      item Experiments "Experiments"
      item Inversions "Inversions"
      item Scan "Scan"
      space
      loginLink tok
    -- WebView.link login center "Login"

    col (grow . scroll) $ do
      el_ $ text $ cs $ show tok
      content
 where
  item r =
    link
      r
      (center . hover (borderColor White . color White) . if r == rc then current else other)

  loginLink Nothing =
    WebView.link login center "Login"
  loginLink (Just _) =
    link Logout center "Log Out"

  center = pad 20

  current = bg (light Primary) . borderColor Primary
  other = borderColor Primary
  topbar = height 70 . flexRow

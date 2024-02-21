module App.Page.Dashboard where

import App.Route
import App.Version
import App.View.Layout
import NSO.Prelude
import Web.Hyperbole


page :: (Hyperbole :> es, Layout :> es) => Page es Response
page = do
  load $ do
    appLayout Dashboard $ col (pad 20) $ do
      el (fontSize 24 . bold) "Level 2"
      el_ $ text $ cs appVersion

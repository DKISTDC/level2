module App.Page.Dashboard where

import App.Route
import App.Version
import Effectful.Rel8
import NSO.Prelude
import Web.Hyperbole
import Web.View

page :: (Hyperbole :> es, Rel8 :> es) => Page es ()
page = do
  load $ do
    pure $ appLayout Dashboard $ col (pad 20) $ do
      -- el (fontSize 24 . bold) "Level 2"
      el_ $ text $ cs appVersion

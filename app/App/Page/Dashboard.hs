module App.Page.Dashboard where

import App.Route
import App.Version
import Effectful.Rel8
import NSO.Prelude
import Web.Hyperbole
import Web.UI

page :: (Page :> es, Rel8 :> es) => Eff es ()
page = do
  pageLoad $ do
    pure $ appLayout Dashboard $ col (pad 20) $ do
      -- el (fontSize 24 . bold) "Level 2"
      el_ $ text $ cs appVersion

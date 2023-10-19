module App.Page.Scan where

import App.Route
import Effectful.Rel8
import NSO.Prelude
import Web.Hyperbole
import Web.UI

-- import NSO.Data.Dataset
-- import NSO.Data.Types

page :: (Page :> es, Rel8 :> es) => Eff es ()
page = do
  pageLoad $ do
    pure $ appLayout Scan $ col_ $ do
      el bold "TODO"
      el_ "Show Previous Scans"
      el_ "Scan Button"

module App.Page.Experiment where

import App.Route
import Effectful.Rel8
import NSO.Data.Dataset
import NSO.Data.Types
import NSO.Prelude
import Web.Hyperbole
import Web.UI

page :: (Page :> es, Rel8 :> es) => Id Experiment -> Eff es ()
page eid = do
  ds <- queryExperiment eid
  view $ layout Experiments $ do
    col_ $ do
      el_ "EXPERIMENT"
      el_ $ text $ cs $ show eid
      el_ $ text $ cs $ show $ length ds

module App.Page.Program where

import App.Route
import Effectful.Rel8
import NSO.Data.Dataset
import NSO.Data.Types
import NSO.Prelude
import Web.Hyperbole
import Web.UI

page :: (Page :> es, Rel8 :> es) => Id InstrumentProgram -> Eff es ()
page i = do
  ds <- queryProgram i
  view $ appLayout Experiments $ do
    col_ $ do
      el_ "INSTRUMENT PROGRAM"
      el_ $ text $ cs $ show i
      el_ $ text $ cs $ show $ length ds

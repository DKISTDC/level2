module App.Route where

import App.Colors
import NSO.Data.Dataset
import NSO.Data.Types
import NSO.Prelude
import Web.Hyperbole
import Web.UI

data AppRoute
  = Dashboard
  | Scan
  | Experiments
  | Experiment (Id Experiment)
  deriving (Show, Generic, Eq, Route)

layout :: AppRoute -> View c () -> View c ()
layout rc content = do
  row (bg Light . color Dark) $ do
    sidebar
    col (gap 25 . pad 25 . grow) $ do
      content
 where
  nav r = link (routeUrl r) (pad 20 . color White . if r == rc then current else id)
  current = bg PrimaryLight . border' (TRBL 0 0 0 5) . padX 15

  sidebar = do
    col (gap 0 . bg Primary . width 400 . color White) $ do
      row (pad 20) $ do
        space
        link "/" (bold . fontSize 32) "Level 2"
        space
      -- nav Dashboard "Dashboard"
      nav Experiments "Experiments"
      nav Scan "Scan"

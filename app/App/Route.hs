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
  | Program (Id InstrumentProgram)
  deriving (Show, Generic, Eq, Route)

appLayout :: AppRoute -> View c () -> View c ()
appLayout rc content = do
  layout (color Dark) $ row grow $ do
    sidebar
    col collapse content
 where
  nav r =
    link
      (routeUrl r)
      (border (TRBL 0 0 0 5) . pad 20 . hover |: borderColor White . hover |: color White . if r == rc then current else other)

  current = bg PrimaryLight . borderColor GrayLight
  other = borderColor Primary

  sidebar = do
    col (gap 0 . bg Primary . width 400 . color GrayLight) $ do
      row (pad 20) $ do
        space
        link "/" (bold . fontSize 32) "Level 2"
        space
      -- nav Dashboard "Dashboard"
      nav Experiments "Experiments"
      nav Scan "Scan"

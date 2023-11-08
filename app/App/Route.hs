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
  layout (color Dark . small flexCol . big flexRow) $ do
    nav (gap 0 . bg Primary . color GrayLight . small topbar . big sidebar) $ do
      row (pad 20 . small (pad 15)) $ do
        space
        link "/" (bold . fontSize 32 . small (fontSize 24)) "Level 2"
        space
      -- nav Dashboard "Dashboard"
      item Experiments "Experiments"
      item Scan "Scan"

    col (collapse . grow . scroll) content
 where
  item r =
    link
      (routeUrl r)
      (border (TRBL 0 0 0 5) . pad 20 . small (border (TRBL 0 0 5 0)) . hover (borderColor White . color White) . if r == rc then current else other)

  current = bg PrimaryLight . borderColor GrayLight
  other = borderColor Primary

  sidebar = width 400 . flexCol
  topbar = height 70 . flexRow
  big = media (MinWidth 1000)
  small = media (MaxWidth 1000)

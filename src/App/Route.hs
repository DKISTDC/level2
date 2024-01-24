module App.Route where

import App.Colors
import NSO.Data.Datasets
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole


data AppRoute
  = Dashboard
  | Scan
  | Experiments
  | Experiment (Id Experiment)
  | Program (Id InstrumentProgram)
  | Dataset (Id Dataset)
  deriving (Show, Generic, Eq, Route)


appLayout :: AppRoute -> View c () -> View c ()
appLayout rc content = do
  layout (color Dark . flexCol) $ do
    nav (gap 0 . bg Primary . color GrayLight . topbar) $ do
      row (pad 15) $ do
        space
        link Dashboard (bold . fontSize 24 . pad (XY 20 0)) "Level 2"
        space
      -- nav Dashboard "Dashboard"
      item Experiments "Experiments"
      item Scan "Scan"

    col (grow . scroll) content
 where
  item r =
    link
      r
      (pad 20 . hover (borderColor White . color White) . if r == rc then current else other)

  current = bg PrimaryLight . borderColor GrayLight
  other = borderColor Primary

  topbar = height 70 . flexRow

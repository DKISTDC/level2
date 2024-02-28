module App.Route where

import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Web.Hyperbole


data AppRoute
  = Dashboard
  | Scan
  | Experiments
  | Inversions
  | Experiment (Id Experiment)
  | Program (Id InstrumentProgram)
  | Dataset (Id Dataset)
  | Redirect
  | Logout
  | Transfer (Id Inversion)
  deriving (Show, Generic, Eq, Route)




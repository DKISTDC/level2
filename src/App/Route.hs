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
  | Proposals
  | Experiments
  | Inversions
  | Inversion (Id Inversion)
  | Proposal (Id Proposal)
  | Program (Id InstrumentProgram)
  | Dataset (Id Dataset)
  | Redirect
  | Logout
  | SubmitDownload (Id Inversion)
  deriving (Show, Generic, Eq, Route)

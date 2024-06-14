module App.Route where

import App.Globus as Globus
import NSO.Prelude
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
  | Inversion (Id Inversion) InversionRoute
  | Proposal (Id Proposal)
  | Program (Id InstrumentProgram)
  | Dataset (Id Dataset)
  | Redirect
  | Logout
  deriving (Show, Generic, Eq, Route)


data InversionRoute
  = Inv
  | SubmitDownload
  | SubmitUpload
  deriving (Show, Generic, Eq, Route)

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
  | Inversion (Id Inversion) InversionRoute
  | Proposal (Id Proposal) ProposalRoute
  | Dataset (Id Dataset)
  | Redirect
  | Logout
  deriving (Show, Generic, Eq, Route)


data ProposalRoute
  = PropRoot
  | Program (Id InstrumentProgram)
  deriving (Show, Generic, Eq, Route)


data InversionRoute
  = Inv
  | SubmitDownload
  | SubmitUpload
  deriving (Show, Generic, Eq, Route)

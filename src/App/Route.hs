module App.Route where

import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Web.Hyperbole


data AppRoute
  = Dashboard
  | Proposals
  | Experiments
  | Inversions
  | Proposal (Id Proposal) ProposalRoute
  | Datasets DatasetRoute
  | Redirect
  | Logout
  | Dev DevRoute
  deriving (Show, Generic, Eq)
instance Route AppRoute where
  baseRoute = Just Dashboard


data ProposalRoute
  = PropRoot
  | Program (Id InstrumentProgram) ProgramRoute
  | Inversion (Id Inversion) InversionRoute
  deriving (Show, Generic, Eq)
instance Route ProposalRoute where
  baseRoute = Just PropRoot


data InversionRoute
  = Inv
  | SubmitUpload
  deriving (Show, Generic, Eq)
instance Route InversionRoute where
  baseRoute = Just Inv


data ProgramRoute
  = Prog
  | SubmitDownload
  deriving (Show, Generic, Eq)
instance Route ProgramRoute where
  baseRoute = Just Prog
  matchRoute [] = Just Prog
  matchRoute ["submitdownload"] = Just SubmitDownload
  matchRoute _ = Nothing


data DatasetRoute
  = DatasetRoot
  | Dataset (Id Dataset)
  deriving (Show, Generic, Eq)
instance Route DatasetRoute where
  matchRoute [] = Just DatasetRoot
  matchRoute [d] = Just (Dataset $ Id d)
  matchRoute _ = Nothing
  routePath DatasetRoot = []
  routePath (Dataset d) = [d.fromId]


data DevRoute
  = DevAuth
  deriving (Show, Generic, Eq)
instance Route DevRoute where
  baseRoute = Just DevAuth


inversionUrl :: Id Proposal -> Id Inversion -> Url
inversionUrl ip ii = routeUrl $ App.Route.Proposal ip $ App.Route.Inversion ii Inv

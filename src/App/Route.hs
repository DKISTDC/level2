module App.Route where

import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import NSO.Data.Sync (SyncId)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Web.Hyperbole
import Web.Hyperbole.Route (genMatchRoute, genRoutePath)


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
  deriving (Show, Generic, Eq)
instance Route InversionRoute where
  baseRoute = Just Inv


data ProgramRoute
  = Prog
  | SubmitDownload
  | SubmitUpload (Id Inversion)
  | InvUpload (Id Inversion)
  deriving (Show, Generic, Eq)
instance Route ProgramRoute where
  baseRoute = Just Prog


  routePath (InvUpload invId) = ["upload", invId.fromId]
  routePath other = genRoutePath other


  matchRoute [] = Just Prog
  matchRoute ["upload", invId] = Just $ InvUpload $ Id invId
  matchRoute segs = genMatchRoute segs


data DatasetRoute
  = DatasetRoot
  | Dataset (Id Dataset)
  | Sync SyncId
  deriving (Show, Generic, Eq)
instance Route DatasetRoute where
  baseRoute = Just DatasetRoot
  matchRoute [] = Just DatasetRoot
  matchRoute [d] = Just (Dataset $ Id d)
  matchRoute ["sync", st] = do
    s <- iso8601ParseM (cs st) :: Maybe UTCTime
    pure $ Sync s
  matchRoute _ = Nothing
  routePath DatasetRoot = []
  routePath (Dataset d) = [d.fromId]
  routePath (Sync s) = ["sync", cs $ iso8601Show s]


data DevRoute
  = DevAuth
  deriving (Show, Generic, Eq)
instance Route DevRoute where
  baseRoute = Just DevAuth


inversion :: Id Proposal -> Id Inversion -> AppRoute
inversion ip ii = App.Route.Proposal ip $ App.Route.Inversion ii Inv


inversionUpload :: Id Proposal -> Id InstrumentProgram -> Id Inversion -> AppRoute
inversionUpload propId progId invId =
  App.Route.Proposal propId $ Program progId $ InvUpload invId


submitUpload :: Id Proposal -> Id InstrumentProgram -> Id Inversion -> AppRoute
submitUpload propId progId invId =
  App.Route.Proposal propId $ Program progId $ SubmitUpload invId


proposal :: Id Proposal -> AppRoute
proposal propId =
  App.Route.Proposal propId PropRoot


program :: Id Proposal -> Id InstrumentProgram -> AppRoute
program propId progId =
  App.Route.Proposal propId $ Program progId Prog


appRoute :: AppRoute -> Mod c -> View c () -> View c ()
appRoute = route

module App.Route where

import App.Globus as Globus
import App.Types
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Prelude
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Network.Globus as Globus (Endpoint (..), Query (..), Scheme (..), Uri (..))
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


-- Various useful effects
data Routes :: Effect where
  LoginUrl :: Routes m Url
  RedirectUri :: Routes m (Uri Globus.Redirect)


type instance DispatchOf Routes = 'Dynamic


runAppRoutes
  :: (Globus :> es)
  => AppDomain
  -> AppRoute
  -> Eff (Routes : es) a
  -> Eff es a
runAppRoutes dom r = interpret $ \_ -> \case
  LoginUrl -> Globus.authUrl (redirectUri dom r)
  RedirectUri -> pure $ redirectUri dom r


redirectUri :: (Route r) => AppDomain -> r -> Uri Globus.Redirect
redirectUri dom r = do
  Uri Https (cs dom.unTagged) (routePath r) (Query [("one", Just "two")])

module App.Globus
  ( GlobusClient (..)
  , Globus
  , authUrl
  , accessToken
  , redirectUri
  , Uri
  , Token (..)
  , Token' (..)
  , runGlobus
  ) where

import App.Route qualified as Route
import App.Types
import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus as Globus
import NSO.Prelude
import Web.Hyperbole.Route
import Web.View as WebView


authUrl :: (Globus :> es) => Eff es WebView.Url
authUrl = do
  uri <- send $ AuthUrl [TransferAll] (State "_")
  pure $ Url $ renderUri uri


redirectUri :: AppDomain -> Uri Redirect
redirectUri domain =
  let Url path = pathUrl $ routePath Route.Redirect
   in Uri Https domain.unTagged [path] (Query [])


accessToken :: (Globus :> es) => Token Exchange -> Eff es (Token Access)
accessToken tok = send $ AccessToken tok

-- accessToken :: Globus -> Uri Redirect -> Token Exchange -> m TokenResponse
-- accessToken (Token cid) (Token sec) red (Token code) =

module App.Page.Auth where

import App.Colors
import App.Effect.Auth
import App.Globus as Globus
import App.Route
import App.View.Icons as Icons
import Effectful.Dispatch.Dynamic
import Effectful.Log
import NSO.Prelude
import Web.Hyperbole


-- show the page, then handle the login second
login :: (Log :> es, Globus :> es, Hyperbole :> es, Auth :> es) => Page es AuthRed
login = do
  handle authRed $ do
    code <- Tagged <$> reqParam "code"

    pure $ col (pad 20 . gap 10) $ do
      el bold "Login"
      hyper AuthRed $ do
        onLoad (LazyAuth code) 0 $ do
          el_ "Authenticating..."
          el (width 200 . color Primary) Icons.spinner


logout :: (Hyperbole :> es, Auth :> es) => Page es ()
logout = do
  handle () $ do
    clearAccessToken
    u <- redirectTo
    redirect u


redirectTo :: (Hyperbole :> es) => Eff es Url
redirectTo = do
  mu <- getLastUrl
  pure $ fromMaybe (pathUrl $ routePath Proposals) mu


data AuthRed = AuthRed
  deriving (Show, Read, ViewId)


data GoRed
  = LazyAuth (Token Exchange)
  deriving (Show, Read, ViewAction)


instance HyperView AuthRed where
  type Action AuthRed = GoRed


authRed :: (Hyperbole :> es, Globus :> es, Auth :> es, Log :> es) => AuthRed -> GoRed -> Eff es (View AuthRed ())
authRed _ (LazyAuth code) = do
  u <- send $ AuthWithCode code
  saveAccessToken u.transfer

  uri <- redirectTo
  redirect uri

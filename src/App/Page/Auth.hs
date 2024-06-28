module App.Page.Auth where

import App.Globus as Globus
import App.Route
import Effectful.Concurrent.STM
import NSO.Prelude
import Web.Hyperbole


login :: (Globus :> es, Concurrent :> es, Hyperbole :> es, Auth :> es) => TMVar (Token Access) -> Page es Response
login adtok = do
  handle authRed
  load $ do
    code <- reqParam "code"
    red <- getRedirectUri
    tok <- Globus.accessToken red (Tagged code)
    saveAccessToken tok
    _ <- atomically $ tryPutTMVar adtok tok
    u <- redirectTo
    -- pure $ Layout.layout mempty Redirect Nothing $ do
    pure $ col (pad 20 . gap 20) $ do
      hyper AuthRed $ onLoad (GoRed u) 1000 $ do
        el bold "Logged In"
        el_ "Redirecting..."


logout :: (Hyperbole :> es, Auth :> es) => Page es Response
logout = do
  load $ do
    clearAccessToken
    u <- redirectTo
    redirect u


redirectTo :: (Hyperbole :> es) => Eff es Url
redirectTo = do
  mu <- getLastUrl
  pure $ fromMaybe (pathUrl $ routePath Proposals) mu


data AuthRed = AuthRed
  deriving (Show, Read, ViewId)


data GoRed = GoRed Url
  deriving (Show, Read, ViewAction)


instance HyperView AuthRed where
  type Action AuthRed = GoRed


authRed :: (Hyperbole :> es) => AuthRed -> GoRed -> Eff es (View AuthRed ())
authRed _ (GoRed u) = do
  redirect u

{-# LANGUAGE UndecidableInstances #-}

module App.Page.Auth where

import App.Colors
import App.Effect.Auth
import App.Route
import App.View.Icons as Icons
import Effectful.Dispatch.Dynamic
import Effectful.Globus
import Effectful.Log
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI (Path (..), pathUri)


-- show the page, then handle the login second
login :: (Log :> es, Hyperbole :> es, Auth :> es) => Page es '[AuthRed]
login = do
  authCode <- Tagged <$> param "code"
  pure $ col ~ pad 20 . gap 10 $ do
    el ~ bold $ "Login"
    hyper AuthRed $ do
      col ~ gap 10 @ onLoad (LazyAuth authCode) 0 $ do
        el "Authenticating..."
        el ~ width 200 . color Primary $ Icons.spinner


logout :: (Hyperbole :> es, Auth :> es) => Page es '[]
logout = do
  clearAccessToken
  u <- redirectTo
  redirect u


redirectTo :: (Hyperbole :> es) => Eff es URI
redirectTo = do
  mu <- getLastUrl
  pure $ fromMaybe (pathUri $ Path True $ routePath Proposals) mu


data AuthRed = AuthRed
  deriving (Generic, ViewId)


instance (Auth :> es, Log :> es) => HyperView AuthRed es where
  data Action AuthRed
    = LazyAuth (Token Exchange)
    deriving (Generic, ViewAction)


  update (LazyAuth authCode) = do
    u <- send $ AuthWithCode authCode
    saveAccessToken u.transfer

    url <- redirectTo
    redirect url

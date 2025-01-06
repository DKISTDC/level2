{-# LANGUAGE UndecidableInstances #-}

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
login :: (Log :> es, Globus :> es, Hyperbole :> es, Auth :> es) => Eff es (Page '[AuthRed])
login = do
  authCode <- Tagged <$> param "code"
  pure $ col (pad 20 . gap 10) $ do
    el bold "Login"
    hyper AuthRed $ do
      col (gap 10 . onLoad (LazyAuth authCode) 0) $ do
        el_ "Authenticating..."
        el (width 200 . color Primary) Icons.spinner


logout :: (Hyperbole :> es, Auth :> es) => Eff es (Page '[])
logout = do
  clearAccessToken
  u <- redirectTo
  redirect u


redirectTo :: (Hyperbole :> es) => Eff es Url
redirectTo = do
  mu <- getLastUrl
  pure $ fromMaybe (pathUrl $ routePath Proposals) mu


data AuthRed = AuthRed
  deriving (Show, Read, ViewId)


instance (Globus :> es, Auth :> es, Log :> es) => HyperView AuthRed es where
  data Action AuthRed
    = LazyAuth (Token Exchange)
    deriving (Show, Read, ViewAction)


  update (LazyAuth authCode) = do
    u <- send $ AuthWithCode authCode
    saveAccessToken u.transfer

    uri <- redirectTo
    redirect uri

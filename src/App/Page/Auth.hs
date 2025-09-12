{-# LANGUAGE UndecidableInstances #-}

module App.Page.Auth where

import App.Colors
import App.Config
import App.Effect.Auth
import App.Route
import App.Style qualified as Style
import App.View.Icons as Icons
import Effectful.Dispatch.Dynamic
import Effectful.Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI (pathUri)


-- LOGGGED OUT PAGE

page :: (Auth :> es, Hyperbole :> es, Reader App :> es, Globus :> es) => Page es '[]
page = do
  muser <- getAccessToken
  madmin <- getAdminToken

  -- go to the login page below after redirecting back
  logUrl <- loginUrl Login

  case (muser, madmin) of
    -- shouldn't be on this page, redirect to the app
    (Just _, Just _) -> redirect $ routeUri Dashboard
    -- need an admin login
    (Just _, Nothing) -> pure $ layout $ do
      loginButton logUrl "Admin Login"
    -- need a login
    (Nothing, _) -> pure $ layout $ do
      loginButton logUrl "User Login"


layout :: View c () -> View c ()
layout cnt = do
  col ~ grow . pad 20 . gap 20 $ do
    el ~ Style.header $ "NSO Level2"
    cnt


loginButton :: URI -> Text -> View c ()
loginButton logUrl lbl = do
  link logUrl ~ Style.btn Primary $ text lbl


------------------------------------------------------------------------------------
-- LOGIN PAGE
-- the target for the redirect. handle authentication
------------------------------------------------------------------------------------

-- show the page, then handle the login second
login :: (Log :> es, Hyperbole :> es) => Page es '[AuthRed]
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
  pure $ fromMaybe (pathUri $ routePath Proposals) mu


data AuthRed = AuthRed
  deriving (Generic, ViewId)


instance (Log :> es, Auth :> es) => HyperView AuthRed es where
  data Action AuthRed
    = LazyAuth (Token Exchange)
    deriving (Generic, ViewAction)


  update (LazyAuth authCode) = do
    u <- send $ AuthWithCode authCode
    saveAccessToken u.transfer

    url <- redirectTo
    redirect url

{-# LANGUAGE OverloadedLists #-}

module App.Effect.Auth.Globus where

import App.Types
import Data.Tagged
import Data.Time.Clock (addUTCTime)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.Globus
import Effectful.Globus qualified as Globus
import Effectful.Log as Log
import Effectful.Reader.Dynamic
import Effectful.Time
import NSO.Prelude
import NSO.Types.User (CurrentAccess (..))
import Network.Globus (UserEmail (..), UserInfo (..), UserInfoResponse (..), UserProfile (..))
import Web.Hyperbole
import Web.Hyperbole.Data.URI as URI


-- Globus stuff -------------------------------------------------------------

loginUrl :: (Globus :> es, Route r, Reader App :> es) => r -> Eff es URI
loginUrl r = do
  app <- ask @App
  authUrl $ redirectUri app.domain r


authUrl :: (Globus :> es) => Uri Redirect -> Eff es URI
authUrl red = do
  url <- send $ AuthUrl red [TransferAll [hardCodedCUBoulderBLANCACollection], Identity Globus.Email, Identity Globus.Profile, Identity Globus.OpenId] (State "_")
  -- url <- send $ AuthUrl red [TransferAll [], Identity Globus.Email, Identity Globus.Profile, Identity Globus.OpenId] (State "_")
  pure $ convertUrl url
 where
  convertUrl :: Uri a -> URI
  convertUrl (Tagged u) = u


fetchAccessTokens :: (Globus :> es) => Uri Redirect -> Token Exchange -> Eff es (NonEmpty TokenItem)
fetchAccessTokens red tok = do
  -- etoks <- try $ send (GetAccessTokens tok red)
  -- case etoks of
  --   Left ex@(HttpExceptionRequest _ cnt) -> do
  --     liftIO $ putStrLn $ "HttpExceptionRequest Content: " <> show cnt
  --     throwIO ex
  --   Left ex@(InvalidUrlException _ _) ->
  --     throwIO ex
  --   Right toks -> pure toks
  send (GetAccessTokens tok red)


globusAuthWithCode :: (Globus :> es, Log :> es, Time :> es, Route route) => AppDomain -> route -> Token Exchange -> Eff es UserLoginInfo
globusAuthWithCode dom rt authCode = do
  let red = redirectUri dom rt
  ts <- fetchAccessTokens red authCode
  res <- runErrorNoCallStack @GlobusError $ userInfo ts
  user :: UserLoginInfo <- either throwIO pure res
  -- when (isAdmin user) $ do
  --   log Debug $ dump "FOUND ADMIN" user.transfer
  --   void $ atomically $ writeTMVar admin.adminToken user.transfer
  pure user
 where
  userInfo :: (Globus :> es, Log :> es, Error GlobusError :> es, Time :> es) => NonEmpty TokenItem -> Eff es UserLoginInfo
  userInfo tis = do
    oid <- requireScopeToken (Identity OpenId) tis

    -- TEST: does this use the same token for everything? Will CU Boulder work?
    trn :: TokenItem <- requireScopeTokenItem (TransferAll []) tis
    access <- fromTokenItem trn

    -- BUG: need to change Globus back to requireScopeToken carrying a type for this
    ur <- send $ GetUserInfo oid
    pure $
      UserLoginInfo
        { info = ur.info
        , email = ur.email
        , profile = ur.profile
        , transfer = access
        }


fromTokenItem :: (Time :> es) => TokenItem -> Eff es CurrentAccess
fromTokenItem item = do
  now <- currentTime
  pure $
    CurrentAccess
      { token = item.access_token
      , expires = addUTCTime (fromIntegral item.expires_in) now
      }


data UserLoginInfo = UserLoginInfo
  { info :: UserInfo
  , email :: Maybe UserEmail
  , profile :: Maybe UserProfile
  , transfer :: CurrentAccess
  }
  deriving (Show)


hardCodedCUBoulderBLANCACollection :: Id Collection
hardCodedCUBoulderBLANCACollection = "4718fe94-aafd-498a-8bae-6bd430bb50a0"


-- WARNING:  doesn't support querystring
redirectUri :: (Route r) => AppDomain -> r -> Uri Redirect
redirectUri dom r = do
  Tagged $ URI "https:" (Just $ URIAuth "" (cs dom.unTagged) "") (cs $ pathToText True $ routePath r) "" ""

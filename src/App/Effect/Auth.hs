{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Effect.Auth where

import App.Config (App (..))
import App.Effect.FileManager (FileLimit (..), fileManagerSelectUrl)
import App.Types
import Control.Monad (void)
import Data.Aeson (FromJSON (..), withText)
import Data.Tagged
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.Globus
import Effectful.Globus qualified as Globus
import Effectful.Log as Log
import Effectful.Reader.Dynamic
import NSO.Prelude
import Network.Globus (UserEmail (..), UserInfo (..), UserInfoResponse (..), UserProfile (..))
import Web.Hyperbole
import Web.Hyperbole.Data.URI as URI


-- Authentication!
data Auth :: Effect where
  AuthWithCode :: Token Exchange -> Auth m UserLoginInfo
  AdminToken :: Auth m (Maybe (Token Access))
  AdminTokenWait :: Auth m (Token Access)
type instance DispatchOf Auth = 'Dynamic


-- https://github.com/haskell/aeson/pull/1144
newtype CurrentUrl = CurrentUrl {uri :: URI}
  deriving newtype (Eq, Show, FromParam, ToParam, ToJSON)
instance FromJSON CurrentUrl where
  parseJSON = withText "URI" $ \t -> do
    case URI.parseURIReference (cs t) of
      Nothing -> fail $ "invalid relative URI: " <> cs t
      Just u -> pure $ CurrentUrl u


data GlobusAuth = GlobusAuth
  { token :: Maybe (Token Access)
  , currentUrl :: Maybe CurrentUrl
  }
  deriving (Generic, FromParam, ToParam, ToJSON, FromJSON)
instance Session GlobusAuth where
  cookiePath = Just []


instance Default GlobusAuth where
  def = GlobusAuth mempty Nothing


runAuth
  :: (Globus :> es, Route r, Concurrent :> es, Log :> es)
  => AppDomain
  -> r
  -> AdminState
  -> Eff (Auth : es) a
  -> Eff es a
runAuth dom r admin = reinterpret id $ \_ -> \case
  AdminToken -> do
    atomically $ tryReadTMVar admin.adminToken
  AdminTokenWait -> do
    atomically $ readTMVar admin.adminToken
  AuthWithCode authCode -> do
    let red = redirectUri dom r
    ts <- accessTokens red authCode
    res <- runErrorNoCallStack @GlobusError $ userInfo ts
    user :: UserLoginInfo <- either throwIO pure res
    when (isAdmin user) $ do
      log Debug $ dump "FOUND ADMIN" user.transfer
      void $ atomically $ tryPutTMVar admin.adminToken user.transfer
    pure user
 where
  userInfo :: (Globus :> es, Log :> es, Error GlobusError :> es) => NonEmpty TokenItem -> Eff es UserLoginInfo
  userInfo tis = do
    oid <- requireScopeToken (Identity OpenId) tis
    -- TEST: does this use the same token for everything? Will CU Boulder work?
    Tagged trn <- requireScopeToken (TransferAll []) tis

    ur <- send $ GetUserInfo oid
    pure $
      UserLoginInfo
        { info = ur.info
        , email = ur.email
        , profile = ur.profile
        , transfer = Tagged trn
        }

  isAdmin :: UserLoginInfo -> Bool
  isAdmin u = do
    case u.email of
      Just ue -> ue `elem` admin.admins
      Nothing -> False


data AdminState = AdminState
  { adminToken :: TMVar (Token Access)
  , admins :: [UserEmail]
  }


initAdmin :: (Concurrent :> es) => [UserEmail] -> Maybe (Token Access) -> Eff es AdminState
initAdmin admins mtok = do
  adminToken <- token mtok
  pure $ AdminState{admins, adminToken}
 where
  token Nothing = newEmptyTMVarIO
  token (Just t) = newTMVarIO t


-- WARNING:  doesn't support querystring
redirectUri :: (Route r) => AppDomain -> r -> Uri Redirect
redirectUri dom r = do
  Tagged $ URI "https:" (Just $ URIAuth "" (cs dom.unTagged) "") (cs $ pathToText $ Path True $ routePath r) "" ""


-- expectAuth :: (Hyperbole :> es, Auth :> es) => Maybe a -> Eff es a
-- expectAuth Nothing = do
--   u <- loginUrl
--   redirect u
-- expectAuth (Just a) = pure a

getAccessToken :: (Hyperbole :> es) => Eff es (Maybe (Token Access))
getAccessToken = do
  auth <- session @GlobusAuth
  pure auth.token


getAdminToken :: (Auth :> es) => Eff es (Maybe (Token Access))
getAdminToken = do
  send AdminToken


saveAccessToken :: (Hyperbole :> es) => Token Access -> Eff es ()
saveAccessToken token = do
  _ <- modifySession $ \auth -> auth{token = Just token}
  pure ()


clearAccessToken :: (Hyperbole :> es) => Eff es ()
clearAccessToken = deleteSession @GlobusAuth


waitForAccess :: (Auth :> es) => Eff (Reader (Token Access) : es) a -> Eff es a
waitForAccess eff = do
  acc <- send AdminTokenWait
  runWithAccess acc eff


runWithAccess :: Token Access -> Eff (Reader (Token Access) : es) a -> Eff es a
runWithAccess = runReader


-- requireLogin :: (Hyperbole :> es, Auth :> es, Log :> es) => Eff (Reader (Token Access) : es) a -> Eff es a
-- requireLogin eff = do
--   acc <- getAccessToken >>= expectAuth
--   runWithAccess acc eff

newtype RedirectPath = RedirectPath [Segment]
  deriving (Show, Eq)


instance Route RedirectPath where
  baseRoute = Just (RedirectPath [])
  routePath (RedirectPath ss) = ss
  matchRoute ss = Just $ RedirectPath ss


currentUrl :: (Hyperbole :> es) => Eff es URI
currentUrl = do
  r <- request
  pure $ URI "" Nothing (cs $ pathToText r.path) (queryString $ filter isNotHyperbole r.query) ""
 where
  isNotHyperbole (p, _) =
    p /= "hyp-id"
      && p /= "hyp-action"


saveCurrentUrl :: (Hyperbole :> es) => Eff es ()
saveCurrentUrl = do
  u <- currentUrl
  _ <- modifySession $ \auth -> auth{currentUrl = Just $ CurrentUrl u}
  pure ()


getLastUrl :: (Hyperbole :> es) => Eff es (Maybe URI)
getLastUrl = do
  auth <- session @GlobusAuth
  pure $ (.uri) <$> auth.currentUrl


openFileManager :: (Hyperbole :> es, Reader App :> es) => FileLimit -> Text -> URI -> Eff es a
openFileManager files lbl submitUrl = do
  cancelUrl <- currentUrl
  app <- ask @App
  redirect $ fileManagerSelectUrl files lbl app.domain submitUrl cancelUrl


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


accessTokens :: (Globus :> es) => Uri Redirect -> Token Exchange -> Eff es (NonEmpty TokenItem)
accessTokens red tok = do
  send $ GetAccessTokens tok red


data UserLoginInfo = UserLoginInfo
  { info :: UserInfo
  , email :: Maybe UserEmail
  , profile :: Maybe UserProfile
  , transfer :: Token Access
  }
  deriving (Show)


hardCodedCUBoulderBLANCACollection :: Id Collection
hardCodedCUBoulderBLANCACollection = "4718fe94-aafd-498a-8bae-6bd430bb50a0"

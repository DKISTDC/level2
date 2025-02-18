module App.Effect.Auth where

import App.Globus (FileLimit (..), GlobusError (..), UserEmail (..), UserLoginInfo (..))
import App.Globus qualified as Globus
import App.Types
import Control.Monad (void)
import Data.Tagged
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Globus
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Prelude
import Web.Hyperbole
import Web.View as WebView


-- Authentication!
data Auth :: Effect where
  LoginUrl :: Auth m Url
  AuthWithCode :: Token Exchange -> Auth m UserLoginInfo
  AdminToken :: Auth m (Maybe (Token Access))
  AdminTokenWait :: Auth m (Token Access)


data GlobusAuth = GlobusAuth
  { token :: Maybe (Token Access)
  , currentUrl :: Maybe Url
  }
  deriving (Generic, Show, Read, FromParam, ToParam)
instance Session GlobusAuth where
  cookiePath = Just []
instance DefaultParam GlobusAuth where
  defaultParam = GlobusAuth mempty mempty


type instance DispatchOf Auth = 'Dynamic


runAuth
  :: (Globus :> es, Route r, Concurrent :> es, Log :> es, Error GlobusError :> es)
  => AppDomain
  -> r
  -> AuthState
  -> Eff (Auth : es) a
  -> Eff es a
runAuth dom r auth = interpret $ \_ -> \case
  LoginUrl -> do
    Globus.authUrl $ redirectUri dom r
  AuthWithCode authCode -> do
    let red = redirectUri dom r
    ts <- Globus.accessTokens red authCode
    log Debug $ dump "TS" ts

    u <- Globus.userInfo ts
    when (isAdmin u) $ do
      log Debug $ dump "FOUND ADMIN" u.transfer
      void $ atomically $ tryPutTMVar auth.adminToken u.transfer
    pure u
  AdminToken -> do
    atomically $ tryReadTMVar auth.adminToken
  AdminTokenWait -> do
    atomically $ readTMVar auth.adminToken
 where
  isAdmin :: UserLoginInfo -> Bool
  isAdmin u = do
    case u.email of
      Just ue -> ue `elem` auth.admins
      Nothing -> False


data AuthState = AuthState
  { adminToken :: TMVar (Token Access)
  , admins :: [UserEmail]
  }


initAuth :: (Concurrent :> es) => [UserEmail] -> Maybe (Token Access) -> Eff es AuthState
initAuth admins mtok = do
  adminToken <- token mtok
  pure $ AuthState{admins, adminToken}
 where
  token Nothing = newEmptyTMVarIO
  token (Just t) = newTMVarIO t


-- WARNING:  until we update the globus app, it only allows /redirect, no query, no nothing
redirectUri :: (Route r) => AppDomain -> r -> Uri Globus.Redirect
redirectUri dom r = do
  -- let path = T.intercalate "/" rp
  Uri Https (cs dom.unTagged) (routePath r) (Query []) -- (Query [("path", Just path)])


-- getRedirectUri :: (Hyperbole :> es, Auth :> es) => Eff es (Uri Globus.Redirect)
-- getRedirectUri = do
--   send RedirectUri

expectAuth :: (Hyperbole :> es, Auth :> es) => Maybe a -> Eff es a
expectAuth Nothing = do
  u <- loginUrl
  redirect u
expectAuth (Just a) = pure a


loginUrl :: (Hyperbole :> es, Auth :> es) => Eff es Url
loginUrl = do
  send LoginUrl


getAccessToken :: (Hyperbole :> es, Auth :> es) => Eff es (Maybe (Token Access))
getAccessToken = do
  auth <- session @GlobusAuth
  pure auth.token


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


requireLogin :: (Hyperbole :> es, Auth :> es) => Eff (Reader (Token Access) : es) a -> Eff es a
requireLogin eff = do
  acc <- getAccessToken >>= expectAuth
  runWithAccess acc eff


newtype RedirectPath = RedirectPath [Segment]
  deriving (Show, Eq)


instance Route RedirectPath where
  baseRoute = Just (RedirectPath [])
  routePath (RedirectPath ss) = ss
  matchRoute ss = Just $ RedirectPath ss


currentUrl :: (Hyperbole :> es) => Eff es Url
currentUrl = do
  r <- request
  pure $ Url "" "" r.path r.query


saveCurrentUrl :: (Hyperbole :> es) => Eff es ()
saveCurrentUrl = do
  u <- currentUrl
  _ <- modifySession $ \auth -> auth{currentUrl = Just u}
  pure ()


getLastUrl :: (Hyperbole :> es) => Eff es (Maybe Url)
getLastUrl = do
  auth <- session @GlobusAuth
  pure auth.currentUrl


openFileManager :: (Hyperbole :> es, Auth :> es) => FileLimit -> Url -> Text -> Eff es a
openFileManager files red lbl = do
  r <- request
  requireLogin $ do
    redirect $ Globus.fileManagerSelectUrl files red lbl r

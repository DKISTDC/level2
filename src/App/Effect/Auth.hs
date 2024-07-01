module App.Effect.Auth where

import App.Globus (UserEmail (..), UserLoginInfo (..))
import App.Globus qualified as Globus
import App.Types
import Control.Monad (void)
import Data.Tagged
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Globus
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Prelude
import Web.Hyperbole
import Web.Hyperbole.Effect (Request (..))
import Web.Hyperbole.Route (Route (..))
import Web.View as WebView


-- Authentication!
data Auth :: Effect where
  LoginUrl :: Auth m Url
  AuthWithCode :: Token Exchange -> Auth m UserLoginInfo
  AdminToken :: Auth m (Maybe (Token Access))


type instance DispatchOf Auth = 'Dynamic


runAuth
  :: (Globus :> es, Route r, Concurrent :> es, Log :> es)
  => AppDomain
  -> r
  -> [UserEmail]
  -> TMVar (Token Access)
  -> Eff (Auth : es) a
  -> Eff es a
runAuth dom r admins adtok = interpret $ \_ -> \case
  LoginUrl -> do
    Globus.authUrl $ redirectUri dom r
  AuthWithCode code -> do
    let red = redirectUri dom r
    ts <- Globus.accessTokens red code
    u <- Globus.userInfo ts
    when (isAdmin u) $ do
      log Debug "FOUND ADMIN"
      void $ atomically $ tryPutTMVar adtok u.transfer
    pure u
  AdminToken -> do
    atomically $ tryReadTMVar adtok
 where
  isAdmin :: UserLoginInfo -> Bool
  isAdmin u = do
    case u.email of
      Just ue -> ue `elem` admins
      Nothing -> False


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
  fmap Tagged <$> session "globus"


saveAccessToken :: (Hyperbole :> es) => Token Access -> Eff es ()
saveAccessToken (Tagged acc) = setSession "globus" acc


clearAccessToken :: (Hyperbole :> es) => Eff es ()
clearAccessToken = clearSession "globus"


runWithAccess :: Token Access -> Eff (Reader (Token Access) : es) a -> Eff es a
runWithAccess = runReader


requireLogin :: (Hyperbole :> es, Auth :> es) => Eff (Reader (Token Access) : es) a -> Eff es a
requireLogin eff = do
  acc <- getAccessToken >>= expectAuth
  runWithAccess acc eff


newtype RedirectPath = RedirectPath [Segment]
  deriving (Show, Eq)


instance Route RedirectPath where
  defRoute = RedirectPath []
  routePath (RedirectPath ss) = ss
  matchRoute ss = Just $ RedirectPath ss


currentUrl :: (Hyperbole :> es) => Eff es Url
currentUrl = do
  r <- request
  pure $ Url "" "" r.path r.query


saveCurrentUrl :: (Hyperbole :> es) => Eff es ()
saveCurrentUrl = do
  u <- currentUrl
  setSession "current-url" (renderUrl u)


getLastUrl :: (Hyperbole :> es) => Eff es (Maybe Url)
getLastUrl = do
  u <- session "current-url"
  pure $ url <$> u

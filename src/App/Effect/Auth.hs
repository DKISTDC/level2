{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Effect.Auth where

import App.Effect.FileManager (FileLimit (..), fileManagerSelectUrl)
import App.Types
import Data.Aeson (FromJSON (..), withText)
import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus
import Effectful.Log as Log
import Effectful.Reader.Dynamic
import NSO.Prelude
import NSO.Types.User (User (..))
import Web.Hyperbole
import Web.Hyperbole.Data.URI as URI


-- NOTE: how is this going to work for user auth?
-- User: must identify via globus to access the app

-- The app itself needs rights to all the collections it will be moving files to

-- Authentication! Presence of this effect means you ARE authenticated
data Auth :: Effect where
  -- AdminToken :: Auth m (Maybe (Token Access))
  -- AdminTokenWait :: Auth m (Token Access)
  -- AuthWithCode :: Token Exchange -> Auth m UserLoginInfo
  GetUser :: Auth m User
type instance DispatchOf Auth = 'Dynamic


-- https://github.com/haskell/aeson/pull/1144
newtype CurrentUrl = CurrentUrl {uri :: URI}
  deriving newtype (Eq, Show, FromParam, ToParam, ToJSON)
instance FromJSON CurrentUrl where
  parseJSON = withText "URI" $ \t -> do
    case URI.parseURIReference (cs t) of
      Nothing -> fail $ "invalid relative URI: " <> cs t
      Just u -> pure $ CurrentUrl u


data UserSession = UserSession
  { user :: Maybe User
  , currentUrl :: Maybe CurrentUrl
  }
  deriving (Generic, ToEncoded, FromEncoded, ToParam, FromParam)
instance Session UserSession where
  cookiePath = Just []


instance ToParam (Tagged a Text)
instance FromParam (Tagged a Text)


instance Default UserSession where
  def = UserSession Nothing Nothing


runAuth
  :: (Log :> es)
  => User
  -> Eff (Auth : es) a
  -> Eff es a
runAuth u = reinterpret id $ \_ -> \case
  GetUser -> pure u


-- runUserAuth
--   :: UserSession -> Eff (GlobusAccess User : es) a -> Eff es a
-- runUserAuth = interpret $ \_ -> \case
--   GetCurrentAccess -> _
--

-- isAdmin :: UserLoginInfo -> Bool
-- isAdmin u = do
--   case u.email of
--     Just ue -> ue `elem` admin.admins
--     Nothing -> False

-- data AdminState = AdminState
--   { adminToken :: TMVar (Token Access)
--   , admins :: [UserEmail]
--   }
--
--
-- initAdmin :: (Concurrent :> es) => [UserEmail] -> Maybe (Token Access) -> Eff es AdminState
-- initAdmin admins mtok = do
--   adminToken <- token mtok
--   pure $ AdminState{admins, adminToken}
--  where
--   token Nothing = newEmptyTMVarIO
--   token (Just t) = newTMVarIO t

-- TODO: expect auth!
-- expectAuth :: (Hyperbole :> es, Auth :> es) => Maybe a -> Eff es a
-- expectAuth Nothing = do
--   u <- loginUrl
--   redirect u
-- expectAuth (Just a) = pure a

-- requireLogin :: (Hyperbole :> es, Auth :> es, Log :> es) => Eff (Reader (Token Access) : es) a -> Eff es a
-- requireLogin eff = do
--   acc <- getAccessToken >>= expectAuth
--   runWithAccess acc eff

-- TODO: lookup, redirect if missing
-- getUser :: (Hyperbole :> es) => Eff es UserSession
-- getUser = do
--   auth <- session @UserSession
--   pure auth.token

lookupUser :: (Hyperbole :> es) => Eff es UserSession
lookupUser = session


-- we can only save if the user exists!
saveUser :: (Hyperbole :> es) => User -> Eff es ()
saveUser u = do
  _ <- modifySession $ \s -> s{user = Just u}
  pure ()


clearUser :: (Hyperbole :> es) => Eff es ()
clearUser = deleteSession @UserSession


-- waitForAdmin :: (Auth :> es) => Eff (Reader (Token Access) : es) a -> Eff es a
-- waitForAdmin eff = do
--   acc <- send AdminTokenWait
--   runWithAccess acc eff

runWithAccess :: Token Access -> Eff (Reader (Token Access) : es) a -> Eff es a
runWithAccess = runReader


newtype RedirectPath = RedirectPath Path
  deriving (Show, Eq)


instance Route RedirectPath where
  baseRoute = Just (RedirectPath [])
  routePath (RedirectPath p) = p
  matchRoute p = Just $ RedirectPath p


currentUrl :: (Hyperbole :> es) => Eff es URI
currentUrl = do
  r <- request
  pure $ URI "" Nothing (cs $ pathToText True r.path) (queryString $ filter isNotHyperbole r.query) ""
 where
  isNotHyperbole (p, _) =
    p /= "hyp-id"
      && p /= "hyp-action"


saveCurrentUrl :: (Hyperbole :> es) => Eff es ()
saveCurrentUrl = do
  u <- currentUrl
  _ <- modifySession $ \s -> s{currentUrl = Just $ CurrentUrl u}
  pure ()


getLastUrl :: (Hyperbole :> es) => Eff es (Maybe URI)
getLastUrl = do
  auth <- session @UserSession
  pure $ (.uri) <$> auth.currentUrl


openFileManager :: (Hyperbole :> es, Reader App :> es) => FileLimit -> Text -> URI -> Eff es a
openFileManager files lbl submitUrl = do
  cancelUrl <- currentUrl
  app <- ask @App
  redirect $ fileManagerSelectUrl files lbl app.domain submitUrl cancelUrl

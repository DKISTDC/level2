module App.Config
  ( Config (..)
  , initConfig
  , initServices
  , Services (..)
  , IsMock
  , initDb
  , initGlobus
  , initApp
  , App (..)
  , AuthInfo (..)
  , Tagged (..)
  , AppDomain
  , document
  , GlobusConfig (..)
  , GlobusDevConfig (..)
  ) where

import App.Dev.Globus (DKIST)
import App.Effect.Scratch qualified as Scratch
import App.Globus (GlobusClient (..), Id' (..), Token, Token' (..), UserEmail (..))
import App.Types
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Data.Tagged
import Data.Text
import Effectful
import Effectful.Environment
import Effectful.Error.Static
import Effectful.Fail
import Effectful.GraphQL (Service (..), service)
import Effectful.Log
import Effectful.Rel8 as Rel8
import NSO.Prelude
import NSO.Types.Common
import Text.Read (readMaybe)
import Web.Hyperbole


data Config = Config
  { services :: Services
  , servicesIsMock :: Bool
  , app :: App
  , globus :: GlobusConfig
  , scratch :: Scratch.Config
  , auth :: AuthInfo
  , db :: Rel8.Connection
  }


data AuthInfo = AuthInfo
  { admins :: [UserEmail]
  , adminToken :: Maybe (Token Access)
  }


data Services = Services
  {metadata :: Service}


data GlobusConfig
  = GlobusLive GlobusClient
  | GlobusDev GlobusDevConfig


data GlobusDevConfig = GlobusDevConfig
  { dkist :: Path' Dir DKIST
  }


initConfig :: (Log :> es, Environment :> es, Fail :> es, IOE :> es, Error Rel8Error :> es) => Eff es Config
initConfig = do
  app <- initApp
  db <- initDb
  (services, servicesIsMock) <- initServices
  globus <- initGlobus
  scratch <- initScratch
  auth <- initAuth
  pure $ Config{services, servicesIsMock, globus, app, db, scratch, auth}


initAuth :: (Environment :> es) => Eff es AuthInfo
initAuth = do
  let admins = [UserEmail "shess@nso.edu"]
  adminToken <- fmap (Tagged . cs) <$> lookupEnv "GLOBUS_ADMIN_TOKEN"
  pure $ AuthInfo{admins, adminToken}


type IsMock = Bool
initServices :: (Environment :> es, Fail :> es) => Eff es (Services, IsMock)
initServices = do
  mock <- parseServices <$> lookupEnv "SERVICES"
  meta <- parseService =<< getEnv "METADATA_API"
  pure (Services meta, mock)
 where
  parseServices (Just "MOCK") = True
  parseServices _ = False


parseService :: (Fail :> es) => String -> Eff es Service
parseService u =
  case service (cs u) of
    Nothing -> fail $ "Could not parse service url: " <> cs u
    Just s -> pure s


initScratch :: (Environment :> es) => Eff es Scratch.Config
initScratch = do
  collection <- Tagged @'Collection @Text . cs <$> getEnv "GLOBUS_LEVEL2_ENDPOINT"
  mount <- Path . cs <$> getEnv "SCRATCH_DIR"
  pure $ Scratch.Config{collection, mount}


initGlobus :: (Environment :> es, Log :> es) => Eff es GlobusConfig
initGlobus = do
  res <- runErrorNoCallStack @GlobusDevConfig $ do
    checkGlobusDev
    clientId <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_ID"
    clientSecret <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_SECRET"
    pure $ GlobusClient{clientId, clientSecret}
  case res of
    Left dev -> do
      log Debug "Using DEV GLOBUS"
      pure $ GlobusDev dev
    Right cfg -> pure $ GlobusLive cfg
 where
  checkGlobusDev = do
    dkist <- fmap Path <$> lookupEnv "DEV_GLOBUS_DKIST_DIR"
    case dkist of
      Nothing -> pure ()
      Just d -> throwError $ GlobusDevConfig d


initDb :: (Environment :> es, Error Rel8Error :> es, IOE :> es) => Eff es Rel8.Connection
initDb = do
  postgres <- getEnv "DATABASE_URL"
  Rel8.connect $ cs postgres


initApp :: (Environment :> es, Fail :> es) => Eff es App
initApp = do
  port <- readEnv "APP_PORT"
  domain <- Tagged . cs <$> getEnv "APP_DOMAIN"
  pure $ App{port, domain}


readEnv :: (Environment :> es, Fail :> es) => (Read a) => String -> Eff es a
readEnv e = do
  env <- getEnv e
  case readMaybe env of
    Nothing -> fail $ "Could not read env: " <> env
    Just a -> pure a


document :: BL.ByteString -> BL.ByteString
document cnt =
  [i|<html>
    <head>
      <title>Level2</title>
      <script type="text/javascript">#{scriptEmbed}</script>
      <style type="text/css">#{cssResetEmbed}</style>
      <style type="text/css">body { background-color: \#d3dceb }</style>
    </head>
    <body>#{cnt}</body>
  </html>|]

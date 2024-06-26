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
  , Tagged (..)
  , AppDomain
  , document
  ) where

import App.Effect.Scratch qualified as Scratch
import App.Globus (GlobusClient (..), Id' (..))
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
import Effectful.Rel8 as Rel8
import NSO.Prelude
import NSO.Types.Common
import Text.Read (readMaybe)
import Web.Hyperbole


data Config = Config
  { services :: Services
  , servicesIsMock :: Bool
  , app :: App
  , globus :: GlobusClient
  , scratch :: Scratch.Config
  , db :: Rel8.Connection
  }


data Services = Services
  {metadata :: Service}


initConfig :: (Environment :> es, Fail :> es, IOE :> es, Error Rel8Error :> es) => Eff es Config
initConfig = do
  app <- initApp
  db <- initDb
  (services, servicesIsMock) <- initServices
  globus <- initGlobus
  scratch <- initScratch
  pure $ Config{services, servicesIsMock, globus, app, db, scratch}


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


initGlobus :: (Environment :> es) => Eff es GlobusClient
initGlobus = do
  clientId <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_ID"
  clientSecret <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_SECRET"
  pure $ GlobusClient{clientId, clientSecret}


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

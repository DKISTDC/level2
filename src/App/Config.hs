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

import App.Globus (GlobusClient (..), Token (..))
import App.Types
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Data.Tagged
import Effectful
import Effectful.Error.Static
import Effectful.GraphQL (Service (..), service)
import Effectful.Rel8 as Rel8
import NSO.Prelude
import System.Environment
import Text.Read (readMaybe)
import Web.Hyperbole


data Config = Config
  { services :: Services
  , servicesIsMock :: Bool
  , app :: App
  , globus :: GlobusClient
  , db :: Rel8.Connection
  }


data Services = Services
  {metadata :: Service}


initConfig :: (Rel8Error -> Eff '[IOE] Connection) -> IO Config
initConfig onDbErr = do
  app <- initApp
  db <- initDb onDbErr
  (services, servicesIsMock) <- initServices
  globus <- initGlobus
  pure $ Config{services, servicesIsMock, globus, app, db}


type IsMock = Bool
initServices :: IO (Services, IsMock)
initServices = do
  mock <- parseServices <$> lookupEnv "SERVICES"
  meta <- parseService =<< getEnv "METADATA_API"
  pure (Services meta, mock)
 where
  parseServices (Just "MOCK") = True
  parseServices _ = False


parseService :: String -> IO Service
parseService u =
  case service (cs u) of
    Nothing -> fail $ "Could not parse service url: " <> cs u
    Just s -> pure s


initGlobus :: IO GlobusClient
initGlobus = do
  clientId <- Token . cs <$> getEnv "GLOBUS_CLIENT_ID"
  clientSecret <- Token . cs <$> getEnv "GLOBUS_CLIENT_SECRET"
  pure $ GlobusClient{clientId, clientSecret}


initDb :: (Rel8Error -> Eff '[IOE] Connection) -> IO Rel8.Connection
initDb onErr = do
  postgres <- getEnv "DATABASE_URL"
  runEff . runErrorNoCallStackWith @Rel8Error onErr $ Rel8.connect $ cs postgres


initApp :: IO App
initApp = do
  port <- readEnv "APP_PORT"
  domain <- Tagged . cs <$> getEnv "APP_DOMAIN"
  pure $ App{port, domain}


readEnv :: (Read a) => String -> IO a
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

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
  , GlobusInfo (..)
  , GlobusEndpoint (..)
  ) where

import App.Globus (GlobusClient (..), GlobusEndpoint (..), Id' (..))
import App.Types
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Data.Tagged
import Data.Text
import Effectful
import Effectful.Error.Static
import Effectful.GraphQL (Service (..), service)
import Effectful.Rel8 as Rel8
import NSO.Prelude
import Network.Globus.Types qualified as Globus
import System.Environment
import Text.Read (readMaybe)
import Web.Hyperbole


data Config = Config
  { services :: Services
  , servicesIsMock :: Bool
  , app :: App
  , globus :: GlobusInfo
  , db :: Rel8.Connection
  }


data Services = Services
  {metadata :: Service}


data GlobusInfo = GlobusInfo
  { client :: GlobusClient
  , level2 :: GlobusEndpoint App
  }


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


initGlobus :: IO GlobusInfo
initGlobus = do
  clientId <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_ID"
  clientSecret <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_SECRET"
  level2Collection <- Tagged @'Collection @Text . cs <$> getEnv "GLOBUS_LEVEL2_ENDPOINT" :: IO (Globus.Id Collection)
  level2Path <- getEnv "GLOBUS_LEVEL2_PATH"
  level2Mount <- getEnv "GLOBUS_LEVEL2_MOUNT"
  pure $
    GlobusInfo
      { client = GlobusClient{clientId, clientSecret}
      , level2 =
          GlobusEndpoint
            { collection = level2Collection
            , path = level2Path
            , mount = level2Mount
            }
      }


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

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
  , documentHead
  , GlobusConfig (..)
  ) where

import App.Types
import App.Worker.CPU
import Data.Tagged
import Data.Text
import Effectful
import Effectful.Concurrent
import Effectful.Environment
import Effectful.Error.Static
import Effectful.Fail
import Effectful.Globus (GlobusClient (..))
import Effectful.GraphQL (Service (..), service)
import Effectful.Log
import Effectful.Rel8 as Rel8
import NSO.Files.Scratch qualified as Scratch
import NSO.Metadata
import NSO.Prelude
import NSO.Types.Common
import Network.Globus (Id' (..), Token, Token' (..), UserEmail (..))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Http
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
  , cpuWorkers :: CPUWorkers
  , manager :: Http.Manager
  }


data AuthInfo = AuthInfo
  { admins :: [UserEmail]
  , adminToken :: Maybe (Token Access)
  }


data Services = Services
  {metadata :: MetadataService}


data GlobusConfig
  = GlobusLive GlobusClient


initConfig :: (Log :> es, Environment :> es, Fail :> es, IOE :> es, Error Rel8Error :> es, Concurrent :> es) => Eff es Config
initConfig = do
  app <- initApp
  db <- initDb
  (services, servicesIsMock) <- initServices
  globus <- initGlobus
  scratch <- initScratch
  auth <- initAuth globus
  manager <- liftIO $ Http.newManager Http.tlsManagerSettings
  cpus <- initCPUWorkers

  log Debug $ dump " (config) metadata datasets" services.metadata.datasets
  log Debug $ dump " (config) metadata inversions" services.metadata.inversions
  pure $ Config{services, servicesIsMock, globus, app, db, scratch, auth, cpuWorkers = cpus, manager}


initAuth :: (Environment :> es) => GlobusConfig -> Eff es AuthInfo
initAuth = \case
  GlobusLive _ -> do
    adminToken <- fmap (Tagged . cs) <$> lookupEnv "GLOBUS_ADMIN_TOKEN"
    pure $ AuthInfo{admins, adminToken}
 where
  admins = [UserEmail "shess@nso.edu"]


type IsMock = Bool
initServices :: (Environment :> es, Fail :> es) => Eff es (Services, IsMock)
initServices = do
  mock <- parseServices <$> lookupEnv "SERVICES"
  metaDatasets <- parseMockService <$> getEnv "METADATA_API_DATASETS"
  metaInversions <- parseService =<< getEnv "METADATA_API_INVERSIONS"
  pure (Services (MetadataService metaDatasets metaInversions), mock)
 where
  parseServices (Just "MOCK") = True
  parseServices _ = False

  -- the env is still required
  parseMockService :: String -> Maybe Service
  parseMockService "MOCK" = Nothing
  parseMockService s = parseService s


initCPUWorkers :: (Concurrent :> es, Environment :> es, Fail :> es) => Eff es CPUWorkers
initCPUWorkers = do
  num <- readEnv "CPU_WORKERS"
  cpuWorkers num


parseService :: (MonadFail m) => String -> m Service
parseService u =
  case service (cs u) of
    Nothing -> fail $ "Could not parse service url: " <> cs u
    Just s -> pure s


initScratch :: (Environment :> es) => Eff es Scratch.Config
initScratch = do
  collection <- Tagged @'Collection @Text . cs <$> getEnv "GLOBUS_LEVEL2_ENDPOINT"
  mount <- Path . cs <$> getEnv "SCRATCH_DIR"
  globus <- Path . cs <$> getEnv "SCRATCH_GLOBUS_DIR"
  pure $ Scratch.Config{collection, mount, globus}


initGlobus :: (Environment :> es, Log :> es) => Eff es GlobusConfig
initGlobus = do
  clientId <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_ID"
  clientSecret <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_SECRET"
  pure $ GlobusLive $ GlobusClient{clientId, clientSecret}


initDb :: (Environment :> es, Error Rel8Error :> es, IOE :> es) => Eff es Rel8.Connection
initDb = do
  postgres <- getEnv "DATABASE_URL"
  Rel8.connect $ cs postgres


initApp :: (Environment :> es, Fail :> es) => Eff es App
initApp = do
  let port = 3033 -- readEnv "APP_PORT" -- hard coded, since nginx is hard coded
  domain <- Tagged . cs <$> getEnv "APP_DOMAIN"
  pure $ App{port, domain}


readEnv :: (Environment :> es, Fail :> es) => (Read a) => String -> Eff es a
readEnv e = do
  env <- getEnv e
  case readMaybe env of
    Nothing -> fail $ "Could not read env: " <> env
    Just a -> pure a


documentHead :: View DocumentHead ()
documentHead = do
  title "Level2"
  script' scriptEmbed
  script' scriptLiveReload
  style (cs cssEmbed)
  style "body { background-color: #d3dceb }"

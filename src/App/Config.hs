module App.Config
  ( Config (..)
  , initConfig
  , initServices
  , Services (..)
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

import App.Config.MeshConfig
import App.Types
import App.Worker.CPU
import Data.Tagged
import Effectful
import Effectful.Concurrent
import Effectful.Environment
import Effectful.Error.Static
import Effectful.Fail
import Effectful.Globus (GlobusClient (..))
import Effectful.GraphQL (Service (..), service)
import Effectful.Log
import Effectful.Rel8 as Rel8
import NSO.Files.DKIST (Level1, Publish)
import NSO.Files.RemoteFolder
import NSO.Files.Scratch qualified as Scratch
import NSO.InterserviceBus as ISB (InterserviceBusConfig (..), initBusConfig)
import NSO.Metadata
import NSO.Prelude
import NSO.Types.Common
import Network.Endpoint (Endpoint (..), EndpointAuth (..), toURI)
import Network.Globus (Token, Token' (..), UserEmail (..))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Http
import Text.Read (readMaybe)
import Web.Hyperbole


data Config = Config
  { services :: Services
  , app :: App
  , globus :: GlobusConfig
  , scratch :: Scratch.Config
  , auth :: AuthInfo
  , db :: Rel8.Connection
  , cpuWorkers :: CPUWorkers
  , manager :: Http.Manager
  , level1 :: Remote Level1
  , publish :: Remote Publish
  }


data AuthInfo = AuthInfo
  { admins :: [UserEmail]
  , adminToken :: Maybe (Token Access)
  }


data Services = Services
  { metadata :: MetadataService
  , interserviceBus :: InterserviceBusConfig
  }


data GlobusConfig
  = GlobusLive GlobusClient


initConfig :: (Log :> es, Environment :> es, Fail :> es, IOE :> es, Error Rel8Error :> es, Concurrent :> es) => Eff es Config
initConfig = do
  app <- initApp

  db <- initDb
  globus <- initGlobus
  scratch <- initScratch
  auth <- initAuth globus
  manager <- liftIO $ Http.newManager Http.tlsManagerSettings
  cpus <- initCPUWorkers
  (level1, publish) <- initRemotes

  -- DKIST Services
  mesh <- initMesh
  services <- initServices mesh

  log Debug $ dump " (config) metadata datasets" services.metadata.datasets
  log Debug $ dump " (config) metadata inversions" services.metadata.inversions
  pure $ Config{services, globus, app, db, scratch, auth, cpuWorkers = cpus, manager, level1, publish}


initMesh :: (Environment :> es) => Eff es (MeshConfig Endpoint)
initMesh = do
  var <- getEnv "MESH_CONFIG"
  mesh <- parseMeshConfig var
  auths <- meshAuths

  pure $ meshEndpoints auths mesh meshPaths
 where
  meshPaths =
    MeshConfig
      { interserviceBus = ""
      , internalApiGateway = "/graphql"
      }

  meshAuths = do
    userISB <- getEnv "ISB_USERNAME"
    passISB <- getEnv "ISB_PASSWORD"

    tokGQL <- getEnv "GQL_AUTH_TOKEN"

    pure $
      MeshConfig
        { interserviceBus = AuthUser (cs userISB) (cs passISB)
        , internalApiGateway = AuthToken (cs tokGQL)
        }


initAuth :: (Environment :> es) => GlobusConfig -> Eff es AuthInfo
initAuth = \case
  GlobusLive _ -> do
    adminToken <- fmap (Tagged . cs) <$> lookupEnv "GLOBUS_ADMIN_TOKEN"
    pure $ AuthInfo{admins, adminToken}
 where
  admins = [UserEmail "shess@nso.edu"]


initServices :: forall es. (Environment :> es, Fail :> es) => MeshConfig Endpoint -> Eff es Services
initServices mesh = do
  bus <- isbService mesh.interserviceBus
  metadata <- metadataService
  pure $
    Services
      { metadata = metadata
      , interserviceBus = bus
      }
 where
  metadataService :: Eff es MetadataService
  metadataService = do
    gateway <- parseService mesh.internalApiGateway
    pure $
      MetadataService
        { datasets = gateway
        , inversions = gateway
        }

  isbService :: (Environment :> es) => Endpoint -> Eff es InterserviceBusConfig
  isbService endpoint = do
    exg <- getEnv "ISB_EXCHANGE"
    ISB.initBusConfig endpoint exg

  parseService :: (MonadFail m) => Endpoint -> m Service
  parseService e =
    case service e of
      Nothing -> fail $ "Could not parse service: " <> show e <> " || " <> show (toURI e)
      Just s -> pure s


initCPUWorkers :: (Concurrent :> es, Environment :> es, Fail :> es, Log :> es) => Eff es CPUWorkers
initCPUWorkers = do
  num <- readEnv "CPU_WORKERS"
  cpuWorkers num


initScratch :: (Environment :> es, Fail :> es) => Eff es Scratch.Config
initScratch = do
  mountPath <- Path . cs <$> getEnv "SCRATCH_DIR"
  remote <- parseGlobusRemoteURI =<< getEnv "GLOBUS_SCRATCH"
  pure $ Scratch.Config{mount = mountPath, remote}


initRemotes :: (Environment :> es, Fail :> es) => Eff es (Remote Level1, Remote Publish)
initRemotes = do
  level1 <- parseGlobusRemoteURI =<< getEnv "GLOBUS_LEVEL1"
  publish <- parseGlobusRemoteURI =<< getEnv "GLOBUS_PUBLISH"
  pure (level1, publish)


initGlobus :: (Environment :> es, Log :> es) => Eff es GlobusConfig
initGlobus = do
  clientId <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_ID"
  clientSecret <- Tagged . cs <$> getEnv "GLOBUS_CLIENT_SECRET"
  pure $ GlobusLive $ GlobusClient{clientId, clientSecret}


initDb :: (Environment :> es, Error Rel8Error :> es, IOE :> es) => Eff es Rel8.Connection
initDb = do
  postgres <- cs <$> getEnv "DATABASE_URL"
  Rel8.connect [Rel8.connectionSettingFromUrl postgres]


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
  script "/hyperbole.js"
  script "/live-reload.js"
  style (cs cssEmbed)
  -- style "body { background-color: #d3dceb }"
  stylesheet "/level2.css"

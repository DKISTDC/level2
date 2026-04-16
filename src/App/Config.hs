module App.Config
  ( Config (..)
  , initConfig
  , initServices
  , Services (..)
  , initDb
  , initScratch
  , initGlobus
  , initApp
  , initRemotes
  , initMesh
  , App (..)
  , AuthConfig (..)
  , Tagged (..)
  , AppDomain
  , documentHead
  ) where

import App.Config.MeshConfig
import App.Effect.Auth (AuthConfig (..))
import App.Effect.GlobusAccess (dummyUser)
import App.Types
import App.Version
import App.Worker.CPU
import Data.String.Conversions (ConvertibleStrings)
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
import Effectful.Time
import NSO.Files.RemoteFolder (Level1, Publish, Remote (..), parseGlobusRemoteURI)
import NSO.Files.Scratch (Scratches (..))
import NSO.Files.Scratch qualified as Scratch
import NSO.Metadata
import NSO.Prelude
import NSO.Types.Common
import Network.AMQP.Config (AMQPConfig (..), initAMQPConfig, initAMQPConnection)
import Network.AMQP.Worker.Connection qualified as AMQP
import Network.Endpoint (Endpoint (..), EndpointAuth (..), toURI)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Http
import Text.Read (readMaybe)
import Web.Hyperbole


data Config = Config
  { services :: Services
  , app :: App
  , globus :: GlobusClient
  , scratch :: Scratch.Scratches
  , auth :: AuthConfig
  , db :: Rel8.Connection
  , cpuWorkers :: CPUWorkers
  , manager :: Http.Manager
  , level1 :: Remote Level1
  , publish :: Remote Publish
  , amqp :: AMQP.Connection
  }


data Services = Services
  { metadata :: MetadataService
  , interserviceBus :: AMQPConfig
  }


initConfig :: (Log :> es, Environment :> es, Fail :> es, Time :> es, IOE :> es, Error Rel8Error :> es, Concurrent :> es) => Eff es Config
initConfig = do
  app <- initApp

  db <- initDb
  globus <- initGlobus
  scratch <- initScratch
  manager <- liftIO $ Http.newManager Http.tlsManagerSettings
  cpus <- initCPUWorkers
  (level1, publish) <- initRemotes
  auth <- initAuth

  -- DKIST Services
  mesh <- initMesh
  services <- initServices mesh

  -- InterserviceBus
  amqp <- initAMQPConnection services.interserviceBus

  log Debug $ dump "AppVersion" appVersion.value
  log Debug $ dump "GitVersion" gitVersion.value

  pure $ Config{services, globus, app, db, scratch, cpuWorkers = cpus, manager, level1, publish, amqp, auth}


initMesh :: (Environment :> es, Fail :> es) => Eff es (MeshConfig Endpoint)
initMesh = do
  var <- requireEnv "MESH_CONFIG"
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
    userISB <- requireEnv "ISB_USERNAME"
    passISB <- requireEnv "ISB_PASSWORD"

    tokGQL <- requireEnv "GQL_AUTH_TOKEN"

    pure $
      MeshConfig
        { interserviceBus = AuthUser userISB passISB
        , internalApiGateway = AuthToken tokGQL
        }


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

  isbService :: (Environment :> es) => Endpoint -> Eff es AMQPConfig
  isbService endpoint = do
    exg <- requireEnv "ISB_EXCHANGE"
    initAMQPConfig endpoint exg

  parseService :: (MonadFail m) => Endpoint -> m Service
  parseService e =
    case service e of
      Nothing -> fail $ "Could not parse service: " <> show e <> " || " <> show (toURI e)
      Just s -> pure s


initAuth :: (Environment :> es, Time :> es) => Eff es AuthConfig
initAuth = do
  me <- lookupEnv "AUTH_DUMMY"
  du <- maybe (pure Nothing) (fmap Just . dummyUser) me
  pure $ AuthConfig du


initCPUWorkers :: (Concurrent :> es, Environment :> es, Fail :> es, Log :> es) => Eff es CPUWorkers
initCPUWorkers = do
  num <- readEnv "CPU_WORKERS"
  cpuWorkers num


initScratch :: (Environment :> es, Fail :> es) => Eff es Scratch.Scratches
initScratch = do
  ingestGlobus <- parseGlobusRemoteURI =<< requireEnv "SCRATCH_INGEST_GLOBUS"
  ingestMount <- Path <$> requireEnv "SCRATCH_INGEST_DIR"
  let ingest = Scratch.Config ingestMount ingestGlobus

  outputGlobus <- parseGlobusRemoteURI =<< requireEnv "SCRATCH_OUTPUT_GLOBUS"
  outputMount <- Path <$> requireEnv "SCRATCH_OUTPUT_DIR"
  let output = Scratch.Config outputMount outputGlobus

  pure $ Scratches{ingest, output}


initRemotes :: (Environment :> es, Fail :> es) => Eff es (Remote Level1, Remote Publish)
initRemotes = do
  level1 <- parseGlobusRemoteURI =<< requireEnv "GLOBUS_LEVEL1"
  publish <- parseGlobusRemoteURI =<< requireEnv "GLOBUS_PUBLISH"
  pure (level1, publish)


initGlobus :: (Environment :> es, Fail :> es, Log :> es) => Eff es GlobusClient
initGlobus = do
  clientId <- Tagged <$> requireEnv "GLOBUS_CLIENT_ID"
  clientSecret <- Tagged <$> requireEnv "GLOBUS_CLIENT_SECRET"
  pure $ GlobusClient{clientId, clientSecret}


initDb :: (Environment :> es, Fail :> es, Error Rel8Error :> es, IOE :> es) => Eff es Rel8.Connection
initDb = do
  postgres <- requireEnv "DATABASE_URL"
  Rel8.connect [Rel8.connectionSettingFromUrl postgres]


initApp :: (Environment :> es, Fail :> es) => Eff es App
initApp = do
  port <- readEnv @Int "APP_PORT" -- readEnv "APP_PORT" -- hard coded, since nginx is hard coded
  domain <- Tagged <$> requireEnv "APP_DOMAIN"
  pure $ App{port, domain}


readEnv :: (Read a, Environment :> es, Fail :> es) => String -> Eff es a
readEnv e = do
  env <- requireEnv e
  case readMaybe env of
    Nothing -> fail $ "Required ENV (" <> e <> ") could not be read: " <> env
    Just a -> pure a


requireEnv :: (Environment :> es, Fail :> es, ConvertibleStrings String t) => String -> Eff es t
requireEnv e = do
  env <- lookupEnv e
  case env of
    Nothing -> fail $ "Required ENV Missing: " <> e
    Just "" -> fail $ "Required ENV Empty '': " <> e
    Just v -> pure $ cs v


documentHead :: View DocumentHead ()
documentHead = do
  title "Level2"
  script "/hyperbole.js"
  script "/live-reload.js"
  style (cs cssEmbed)
  -- style "body { background-color: #d3dceb }"
  stylesheet "/level2.css"

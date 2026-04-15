module Publisher where

import App.Config qualified as Config
import App.Effect.GlobusAccess as GlobusAccess
import App.Effect.Transfer (runTransfer)
import App.Worker.Publish as Publish
import Control.Monad.Catch (Exception, throwM)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Environment
import Effectful.Error.Static
import Effectful.Fail
import Effectful.Fetch
import Effectful.FileSystem
import Effectful.GenRandom
import Effectful.Globus (GlobusError, runGlobus)
import Effectful.GraphQL hiding (Request (..), Response (..))
import Effectful.Log as Log
import Effectful.Reader.Dynamic
import Effectful.Rel8 as Rel8
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets (runDataDatasets)
import NSO.Data.Inversions (runDataInversions)
import NSO.Files (Output, Publish)
import NSO.Files.Scratch as Scratch
import NSO.InterserviceBus
import NSO.InterserviceBus qualified as ISB
import NSO.Metadata as Metadata
import NSO.Prelude
import Network.AMQP.Config (initAMQPConnection)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Http


-- NOTE: REQUIREMENTS
-- ~ Queue Publish Task
-- ~ Work on Publish Task
-- ~ Update status -> Show in app.... how?

main :: IO ()
main = do
  runEff $ runConcurrent $ do
    logs <- Log.init
    runReader logs $ do
      concurrently_ Log.startUpdater start


start :: (IOE :> es, Reader Logs :> es, Concurrent :> es) => Eff es ()
start = do
  runLogger "Publisher" . runCore . runInit $ do
    runWorker $ do
      log Info "NSO L2 Publish Worker"
      startWorker publishTask
 where
  runInit =
    runErrorWith @Rel8Error crashWithError
      . runErrorWith @GraphQLError crashWithError
      . runErrorWith @GlobusError crashWithError
      . runFailIO
      . runEnvironment

  runCore action = do
    runFileSystem
      . runTime
      . runGenRandom
      $ action

  runWorker action = do
    mgr <- liftIO $ Http.newManager Http.tlsManagerSettings
    db <- Config.initDb
    scratch <- Config.initScratch
    (_, publish) <- Config.initRemotes
    globus <- Config.initGlobus

    -- DKIST
    mesh <- Config.initMesh
    services <- Config.initServices mesh
    amqp <- initAMQPConnection services.interserviceBus
    pubs <- initQueueAMQP publishKey amqp
    bus <- ISB.initBus amqp

    runGlobus globus mgr $ do
      access <- initGlobusClientAccess

      runReader scratch
        . runRel8 db
        . runScratch @Output scratch.output
        . runFetchHttp mgr
        . runGlobusClientAccess @Publish access
        . runGlobusClientAccess @Output access
        . runTransfer @Output @Publish scratch.output.remote publish
        . runGraphQL mgr
        . runMetadata services.metadata
        . runInterserviceBus bus
        . runDataInversions
        . runDataDatasets
        . runTaskQueueAMQP @PublishTask pubs
        $ action


crashWithError :: (IOE :> es, Log :> es, Show e, Exception e) => CallStack -> e -> Eff es a
crashWithError c e = do
  log Err $ dump "Uncaught Error" e
  log Err $ prettyCallStack c
  liftIO $ throwM e

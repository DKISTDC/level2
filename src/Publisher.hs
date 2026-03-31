module Publisher where

import App.Config qualified as Config
import App.Effect.GlobusAccess as GlobusAccess
import App.Effect.Transfer (Transfer, runTransfer)
import App.Worker.Publish as Publish
import Control.Monad.Catch (Exception, throwM)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Environment
import Effectful.Error.Static
import Effectful.Fail
import Effectful.Fetch
import Effectful.FileSystem
import Effectful.GenRandom
import Effectful.Globus (Globus (..), GlobusError, runGlobus)
import Effectful.GraphQL hiding (Request (..), Response (..))
import Effectful.Log as Log
import Effectful.Reader.Dynamic
import Effectful.Rel8 as Rel8
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets (Datasets, runDataDatasets)
import NSO.Data.Inversions (Inversions, runDataInversions)
import NSO.Files.Scratch (Scratch, runScratch)
import NSO.Files.Scratch as Scratch
import NSO.Files.Scratch qualified as Scratch
import NSO.InterserviceBus
import NSO.InterserviceBus qualified as ISB
import NSO.Metadata as Metadata
import NSO.Prelude
import NSO.Remote (Level1, Publish)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Http
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)


-- TODO: How do we get admin auth in here for globus?
-- TODO: a way to run the Tasks interface with AMQP instead of in-process workers

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  runEff $ runConcurrent . runLogs . runCore . runInit $ do
    runWorker $ do
      log Info "NSO L2 Publish Worker"
      startWorker Publish.publishTask
 where
  runLogs action = do
    logs <- Log.init
    runReader logs . runLogger "Publish" $ action

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
    pubs <- atomically taskChanNew

    mgr <- liftIO $ Http.newManager Http.tlsManagerSettings
    db <- Config.initDb
    scratch <- Config.initScratch
    (_, publish) <- Config.initRemotes
    globus <- Config.initGlobus
    services <- Config.initServices
    bus <- ISB.initBusConnection services.interserviceBus

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
        . runTasks @PublishTask pubs
        $ action


crashWithError :: (IOE :> es, Log :> es, Show e, Exception e) => CallStack -> e -> Eff es a
crashWithError c e = do
  log Err $ dump "Uncaught Error" e
  log Err $ prettyCallStack c
  liftIO $ throwM e

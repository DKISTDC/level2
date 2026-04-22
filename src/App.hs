module App where

import App.Config
import App.Effect.Auth as Auth (Auth, runAuth)
import App.Effect.Auth qualified as Auth
import App.Effect.GlobusAccess
import App.Effect.Transfer (Transfer, runTransfer)
import App.Page.Auth qualified as Auth
import App.Page.Dashboard qualified as Dashboard
import App.Page.Dataset qualified as Dataset
import App.Page.Datasets qualified as Datasets
import App.Page.Inversion qualified as Inversion
import App.Page.InversionUpload qualified as InversionUpload
import App.Page.Inversions qualified as Inversions
import App.Page.Program qualified as Program
import App.Page.Proposal qualified as Proposal
import App.Page.Proposals qualified as Proposals
import App.Page.Sync qualified as Sync
import App.Page.Tasks qualified as Tasks
import App.Route
import App.Version
import App.View.Error
import App.Worker.Generate as Gen
import App.Worker.Publish as Publish
import App.Worker.PuppetMaster qualified as PuppetMaster
import App.Worker.SyncMetadata as Sync
import App.Worker.TaskReporter (initReportQueue)
import Control.Monad (forever)
import Control.Monad.Catch (Exception, throwM)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Debug (Debug, runDebugIO)
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
import NSO.Data.Programs as Programs
import NSO.Data.Sync as Sync (History, MetadataSync, initMetadataSync, runMetadataSync)
import NSO.Files (Ingest, Level1, Output, Publish)
import NSO.Files.Scratch (Scratch, runScratch)
import NSO.Files.Scratch qualified as Scratch
import NSO.InterserviceBus
import NSO.Metadata as Metadata
import NSO.Prelude
import NSO.Types.User (User (..))
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Static as Static (addBase)
import Network.Wai.Middleware.Static qualified as Static
import Web.Hyperbole
import Web.Hyperbole.Data.URI (pathUri)
import Web.Hyperbole.Effect.Request (reqPath)


main :: IO ()
main = do
  putStrLn "APP MAIN"
  runEff $ runConcurrent $ do
    logs <- Log.init
    runReader logs start


start :: (IOE :> es, Reader Logs :> es, Concurrent :> es) => Eff es ()
start = do
  runInit $ do
    log Info "NSO Level 2a"
    config <- initConfig

    runGlobus config.globus config.manager $ do
      tasks <- initTaskStore
      report <- initReportQueue config.amqp
      fits <- initQueueIO
      pubs <- initQueueAMQP publishKey config.amqp
      metas <- initQueueIO
      props <- initQueueIO
      sync <- initMetadataSync
      bus <- initBus config.amqp
      progs <- Programs.initStore
      globusAccess <- initGlobusClientAccess

      concurrently_
        (startWebServer config tasks fits pubs sync globusAccess progs)
        (runWorkers config tasks fits sync metas props bus globusAccess $ startWorkers report)
 where
  startPuppetMaster =
    runLogger "Puppet" $ do
      forever $ do
        PuppetMaster.manageMinions

  startWebServer :: (IOE :> es, Reader (TMVar LogState) :> es, Concurrent :> es) => Config -> TaskStore -> QueueChan GenTask -> QueueAMQP PublishTask -> Sync.History -> ClientAccess -> ProgramStore -> Eff es ()
  startWebServer config tasks fits pubs sync globusAccess progs =
    runLogger "Server" $ do
      rows <- ask
      -- log Debug $ "Starting on :" <> show config.app.port
      log Debug $ "Develop using https://" <> cs config.app.domain.unTagged <> "/ (" <> show config.app.port <> ")"

      liftIO $ do
        let settings =
              Warp.setPort config.app.port
                . Warp.setHost "0.0.0.0"
                $ Warp.defaultSettings
        Warp.runSettings settings $
          Static.staticPolicy (addBase "app") $
            javascript $
              addHeaders [("app-version", cs appVersion.value)] $
                webServer config tasks fits pubs sync rows globusAccess progs

  javascript :: Application -> Application
  javascript app req respond = do
    case Wai.rawPathInfo req of
      "/hyperbole.js" -> staticFile "text/javascript" scriptEmbed
      "/hyperbole.js.map" -> staticFile "application/json" scriptEmbedSourceMap
      "/live-reload.js" -> staticFile "text/javascript" scriptLiveReload
      _ -> app req respond
   where
    staticFile mime dat = do
      respond $ Wai.responseLBS status200 [("Content-Type", mime)] (cs dat)

  startWorkers report =
    mapConcurrently_
      id
      [ startPuppetMaster
      , startWorker Gen.generateTask
      , startWorker Sync.syncMetadataTask
      , startWorker Sync.syncProposalTask
      , startReportListener report
      , Log.startUpdater
      ]

  runInit =
    runLogger "Init"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @GraphQLError crashWithError
      . runErrorWith @GlobusError crashWithError
      . runFailIO
      . runEnvironment
      . runTime

  runWorkers config tasks fits sync metas props bus globusAccess =
    runFileSystem
      . runDebugIO
      . runReader config.scratch
      . runReader config.cpuWorkers
      . runRel8 config.db
      . runScratch @Ingest config.scratch.ingest
      . runScratch @Output config.scratch.output
      . runFetchHttp config.manager
      . runGlobusClientAccess @Level1 globusAccess
      . runGlobusClientAccess @Ingest globusAccess
      . runGlobusClientAccess @Publish globusAccess
      . runTransfer @Level1 @Publish config.level1 config.publish
      . runTransfer @Level1 @Ingest config.level1 config.scratch.ingest.remote
      . runGraphQL config.manager
      . runMetadata config.services.metadata
      . runGenRandom
      . runInterserviceBus bus
      . runDataInversions
      . runDataDatasets
      . runReportTaskNoop
      . runTasksIO tasks
      . runQueueIO @GenTask fits
      . runQueueIO @SyncMetadataTask metas
      . runQueueIO @SyncProposalTask props
      . runMetadataSync sync


webServer :: Config -> TaskStore -> QueueChan GenTask -> QueueAMQP PublishTask -> Sync.History -> TMVar LogState -> ClientAccess -> ProgramStore -> Application
webServer config tasks fits pubs sync rows globusAccess progs =
  liveApp
    (document documentHead)
    respond
 where
  router Dashboard = runPage Dashboard.page
  router Proposals = runPage Proposals.page
  router Inversions = runPage Inversions.page
  router (Proposal ip (Inversion i Inv)) = runPage $ Inversion.page ip i
  router (Proposal p PropRoot) = runPage $ Proposal.page p
  router (Proposal propId (Program progId r)) =
    case r of
      Prog -> runPage $ Program.page propId progId
      InvUpload invId -> runPage $ InversionUpload.page propId progId invId
      SubmitUpload invId -> InversionUpload.submitUpload propId progId invId
  router (Datasets DatasetRoot) = runPage Datasets.page
  router (Datasets (Dataset d)) = runPage $ Dataset.page d
  router (Datasets (Sync d)) = runPage $ Sync.page d
  router Experiments = do
    redirect (pathUri $ routePath Proposals)
  router Logout = runPage Auth.logout
  router Login = runPage Auth.login
  router Tasks = runPage Tasks.page

  respond :: (IOE :> es, Concurrent :> es, Hyperbole :> es) => Eff es Response
  respond = do
    pth <- reqPath
    runBasic $ do
      case pth of
        -- allow /redirect past to process the login
        "/redirect" -> runPage Auth.login
        -- otherwise, check login status and redirect to the auth page
        _ -> do
          us <- Auth.lookupUser
          case us.user <|> config.auth.dummy of
            (Just u) -> runApp u . routeRequest $ router
            _ -> runPage Auth.page

  runBasic :: (Hyperbole :> es, Concurrent :> es, IOE :> es) => Eff (Time : Reader App : Scratch Output : Scratch Ingest : FileSystem : Globus : Error GlobusError : Log : Reader (TMVar LogState) : es) a -> Eff es a
  runBasic =
    runReader rows
      . runLogger "AppBasic"
      . runErrorWith @GlobusError onGlobus
      . runGlobus config.globus config.manager
      . runFileSystem
      . runScratch config.scratch.ingest
      . runScratch config.scratch.output
      . runReader config.app
      . runTime

  runApp
    :: (IOE :> es, Concurrent :> es, Hyperbole :> es, Scratch Output :> es, Scratch Ingest :> es, Globus :> es, Reader (TMVar LogState) :> es, Time :> es)
    => User
    -> Eff (Transfer Level1 Ingest : Transfer Output Publish : GlobusAccess User : GlobusAccess Level1 : GlobusAccess Publish : GlobusAccess Output : GlobusAccess Ingest : Auth : Debug : MetadataSync : Queue PublishTask : Queue GenTask : Tasks : ReportTaskStatus : Programs : Inversions : Datasets : MetadataDatasets : MetadataInversions : GraphQL : Fetch : Rel8 : GenRandom : Error GraphQLError : Error Rel8Error : Log : es) Response
    -> Eff es Response
  runApp u =
    runLogger "App"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @GraphQLError crashWithError
      . runGenRandom
      . runRel8 config.db
      . runFetchHttp config.manager
      . runGraphQL config.manager
      . runMetadata config.services.metadata
      . runDataDatasets
      . runDataInversions
      . runDataPrograms progs
      . runReportTaskNoop
      . runTasksIO tasks
      . runQueueIO fits
      . runQueueAMQP pubs
      . runMetadataSync sync
      . runDebugIO
      . runAuth u
      . runGlobusClientAccess @Ingest globusAccess
      . runGlobusClientAccess @Output globusAccess
      . runGlobusClientAccess @Publish globusAccess
      . runGlobusClientAccess @Level1 globusAccess
      . runGlobusUserAccess
      . runTransfer @Output @Publish config.scratch.output.remote config.publish
      . runTransfer @Level1 @Ingest config.level1 config.scratch.ingest.remote


onGlobus :: (IOE :> es, Log :> es, Hyperbole :> es) => CallStack -> GlobusError -> Eff es a
onGlobus _callstack err = do
  log Err $ dump "Globus Error" err

  respondErrorView (errorType @GlobusError) $ do
    Auth.layout $ do
      Auth.loginButton (routeUri Logout) "Reauthenticate"
      viewError err


crashAndPrint :: forall e es a. (UserFacingError e, IOE :> es, Log :> es, Show e, Exception e, Hyperbole :> es) => CallStack -> e -> Eff es a
crashAndPrint _callstack err = do
  log Err $ dump "Error Shown To User" err
  respondErrorView (errorType @e) $ viewError err


crashWithError :: (IOE :> es, Log :> es, Show e, Exception e) => CallStack -> e -> Eff es a
crashWithError c e = do
  log Err $ dump "Uncaught Error" e
  log Err $ prettyCallStack c
  liftIO $ throwM e

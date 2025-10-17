module App where

import App.Config
import App.Effect.Auth as Auth (AdminState (..), Auth, initAdmin, runAuth)
import App.Effect.Auth qualified as Auth
import App.Effect.Transfer (Transfer, runTransfer)
import App.Page.Auth qualified as Auth
import App.Page.Dashboard qualified as Dashboard
import App.Page.Dataset qualified as Dataset
import App.Page.Datasets qualified as Datasets
import App.Page.Datasets.Download qualified as Downloads
import App.Page.Inversion qualified as Inversion
import App.Page.InversionUpload qualified as InversionUpload
import App.Page.Inversions qualified as Inversions
import App.Page.Program qualified as Program
import App.Page.Proposal qualified as Proposal
import App.Page.Proposals qualified as Proposals
import App.Page.Sync qualified as Sync
import App.Route
import App.Version
import App.View.Error
import App.Worker.Generate as Gen
import App.Worker.Publish as Publish
import App.Worker.PuppetMaster qualified as PuppetMaster
import App.Worker.SyncMetadata as Sync
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
import Effectful.State.Static.Shared as State
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets (Datasets, runDataDatasets)
import NSO.Data.Inversions (Inversions, runDataInversions)
import NSO.Data.Sync as Sync (History, MetadataSync, initMetadataSync, runMetadataSync)
import NSO.Files.Scratch (Scratch, runScratch)
import NSO.Files.Scratch qualified as Scratch
import NSO.Metadata as Metadata
import NSO.Prelude
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Static as Static (addBase)
import Network.Wai.Middleware.Static qualified as Static
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Web.Hyperbole
import Web.Hyperbole.Data.URI (pathUri)
import Web.Hyperbole.Effect.Request (reqPath)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  runEff $ runConcurrent $ do
    logs <- Log.init
    runReader logs $ runInit $ do
      log Info "NSO Level 2"
      config <- initConfig

      fits <- atomically taskChanNew
      pubs <- atomically taskChanNew
      metas <- atomically taskChanNew
      props <- atomically taskChanNew
      sync <- initMetadataSync
      admin <- initAdmin config.auth.admins config.auth.adminToken

      concurrently_
        (startWebServer config admin fits pubs sync)
        (runWorkers config admin fits pubs sync metas props startWorkers)
 where
  startPuppetMaster =
    runLogger "Puppet" $ do
      forever $ do
        PuppetMaster.manageMinions

  startWebServer :: (IOE :> es, Reader (TMVar LogState) :> es, Concurrent :> es) => Config -> AdminState -> TaskChan GenTask -> TaskChan PublishTask -> Sync.History -> Eff es ()
  startWebServer config auth fits pubs sync =
    runLogger "Server" $ do
      rows <- ask
      -- log Debug $ "Starting on :" <> show config.app.port
      log Debug $ "Develop using https://" <> cs config.app.domain.unTagged <> "/"
      liftIO $
        Warp.run config.app.port $
          Static.staticPolicy (addBase "app") $
            javascript $
              addHeaders [("app-version", cs appVersion)] $
                webServer config auth fits pubs sync rows

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

  startGen = do
    runLogger "Generate" $
      startWorker Gen.generateTask

  startPublish = do
    runLogger "Publish" $
      startWorker Publish.publishTask

  startLogUpdater = do
    log Debug "start log updater"
    logContext "logger" $ do
      logStatus "start"
      _ <- runState (0 :: Int) $ forever $ do
        n <- State.get @Int
        logStatus (show n)
        State.put (n + 1)
        send Log.Render
        threadDelay (250 * 1000)
      pure ()

  startWorkers =
    mapConcurrently_
      id
      [ startPuppetMaster
      , startGen
      , startWorker Sync.syncMetadataTask
      , startWorker Sync.syncProposalTask
      , startPublish
      , startLogUpdater
      ]

  runInit =
    runLogger "Init"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @GraphQLError crashWithError
      . runErrorWith @GlobusError crashWithError
      . runFailIO
      . runEnvironment

  runWorkers config admin fits pubs sync metas props =
    runFileSystem
      . runReader config.scratch
      . runReader config.cpuWorkers
      . runRel8 config.db
      . runScratch config.scratch
      . runGlobus' config.globus config.manager
      . runFetchHttp config.manager
      . runAuth config.app.domain Login admin
      . runTransfer config.level1 config.publish config.scratch.remote
      . runGraphQL config.manager
      . runMetadata config.services.metadata
      . runGenRandom
      . runTime
      . runDataInversions
      . runDataDatasets
      . runTasks @GenTask fits
      . runTasks @PublishTask pubs
      . runTasks @SyncMetadataTask metas
      . runTasks @SyncProposalTask props
      . runMetadataSync sync


-- waitForAdminAccess :: (Auth :> es, Concurrent :> es, Log :> es, Globus :> es) => Eff (Transfer : Reader (Token Access) : es) () -> Eff es ()
-- waitForAdminAccess work = do
--   checkAndWait $ do
--     tok <- ask @(Token Access)
--     runTransfer tok $ do
--       log Debug " - got admin token"
--       work
--  where
--   checkAndWait next = do
--     admin <- Auth.getAdminToken
--     case admin of
--       Just tok -> do
--         Auth.runWithAccess tok next
--       Nothing -> do
--         log Debug "Waiting for Admin Globus Access Token"
--         Auth.waitForAdmin next

webServer :: Config -> AdminState -> TaskChan GenTask -> TaskChan PublishTask -> Sync.History -> TMVar LogState -> Application
webServer config admin fits pubs sync rows =
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
      SubmitDownload -> Downloads.submitDatasetDownload propId progId
      InvUpload invId -> runPage $ InversionUpload.page propId progId invId
      SubmitUpload invId -> InversionUpload.submitUpload propId progId invId
  router (Datasets DatasetRoot) = runPage Datasets.page
  router (Datasets (Dataset d)) = runPage $ Dataset.page d
  router (Datasets (Sync d)) = runPage $ Sync.page d
  router Experiments = do
    redirect (pathUri $ routePath Proposals)
  router Logout = runPage Auth.logout
  router Login = runPage Auth.login

  respond :: (IOE :> es, Concurrent :> es, Hyperbole :> es) => Eff es Response
  respond = do
    pth <- reqPath
    runBasic $ do
      case pth of
        -- allow /redirect past to process the login
        "/redirect" -> runPage Auth.login
        -- otherwise, check login status and redirect to the auth page
        _ -> do
          muser <- Auth.getAccessToken
          madmin <- Auth.getAdminToken
          case (muser, madmin) of
            (Just _, Just _) -> runApp . routeRequest $ router
            _ -> runPage Auth.page

  runBasic :: (Hyperbole :> es, Concurrent :> es, IOE :> es) => Eff (Reader App : Auth : Globus : Scratch : FileSystem : Error GlobusError : Log : Reader (TMVar LogState) : es) a -> Eff es a
  runBasic =
    runReader rows
      . runLogger "AppBasic"
      . runErrorWith @GlobusError onGlobus
      . runFileSystem
      . runScratch config.scratch
      . runGlobus' config.globus config.manager
      . runAuth config.app.domain Login admin
      . runReader config.app

  runApp :: (IOE :> es, Concurrent :> es, Hyperbole :> es, Auth :> es, Scratch :> es, Globus :> es, Reader (TMVar LogState) :> es) => Eff (Debug : Transfer : MetadataSync : Tasks PublishTask : Tasks GenTask : Inversions : Datasets : MetadataDatasets : MetadataInversions : GraphQL : Fetch : Rel8 : GenRandom : Error GraphQLError : Error Rel8Error : Log : Time : es) Response -> Eff es Response
  runApp =
    runTime
      . runLogger "App"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @GraphQLError crashWithError
      . runGenRandom
      . runRel8 config.db
      . runFetchHttp config.manager
      . runGraphQL config.manager
      . runMetadata config.services.metadata
      . runDataDatasets
      . runDataInversions
      . runTasks fits
      . runTasks pubs
      . runMetadataSync sync
      . runTransfer config.level1 config.publish config.scratch.remote
      . runDebugIO


runGlobus' :: forall es a. (Log :> es, IOE :> es, Scratch :> es, Error GlobusError :> es) => GlobusConfig -> Http.Manager -> Eff (Globus : es) a -> Eff es a
runGlobus' (GlobusLive g) mgr action = do
  runGlobus g mgr action


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

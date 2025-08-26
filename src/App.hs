module App where

import App.Config
import App.Effect.Auth as Auth
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
import App.Route
import App.Version
import App.Worker.GenWorker as Gen
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
import Effectful.Globus (Globus, GlobusError, Token, Token' (..), runGlobus)
import Effectful.GraphQL hiding (Request (..), Response (..))
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Rel8 as Rel8
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets (Datasets, runDataDatasets)
import NSO.Data.Inversions (Inversions, runDataInversions)
import NSO.Data.Sync as Sync (History, MetadataSync, initMetadataSync, runMetadataSync)
import NSO.Files.Scratch (Scratch, runScratch)
import NSO.Metadata as Metadata
import NSO.Prelude
import Network.HTTP.Client qualified as Http
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.AddHeaders (addHeaders)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Web.Hyperbole
import Web.Hyperbole.Data.URI (Path (..), pathUri)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  runEff $ runInit $ do
    log Info "NSO Level 2"
    config <- initConfig

    fits <- atomically taskChanNew
    asdf <- atomically taskChanNew
    pubs <- atomically taskChanNew
    metas <- atomically taskChanNew
    props <- atomically taskChanNew
    sync <- initMetadataSync
    auth <- initAuth config.auth.admins config.auth.adminToken

    concurrently_
      (startWebServer config auth fits asdf pubs sync)
      (runWorkers config auth fits asdf pubs sync metas props startWorkers)

    pure ()
 where
  startPuppetMaster =
    runLogger "Puppet" $
      forever
        PuppetMaster.manageMinions

  startWebServer :: (IOE :> es) => Config -> AuthState -> TaskChan GenFits -> TaskChan GenAsdf -> TaskChan PublishTask -> Sync.History -> Eff es ()
  startWebServer config auth fits asdf pubs sync =
    runLogger "Server" $ do
      -- log Debug $ "Starting on :" <> show config.app.port
      log Debug $ "Develop using https://" <> cs config.app.domain.unTagged <> "/"
      liftIO $
        Warp.run config.app.port $
          addHeaders [("app-version", cs appVersion)] $
            webServer config auth fits asdf pubs sync

  startGen = do
    runLogger "FitsGen" $
      waitForGlobusAccess $ do
        mapConcurrently_
          id
          [startWorker Gen.fitsTask, startWorker Gen.asdfTask]

  startWorkers =
    mapConcurrently_
      id
      [ startPuppetMaster
      , startGen
      , startWorker Sync.syncMetadataTask
      , startWorker Sync.syncProposalTask
      ]

  runInit =
    runLogger "Init"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @GlobusError crashWithError
      . runErrorWith @GraphQLError crashWithError
      . runFailIO
      . runEnvironment
      . runConcurrent

  runWorkers config auth fits asdf pubs sync metas props =
    runFileSystem
      . runReader config.scratch
      . runReader config.cpuWorkers
      . runRel8 config.db
      . runScratch config.scratch
      . runGlobus' config.globus config.manager
      . runFetchHttp config.manager
      . runAuth config.app.domain Redirect auth
      . runGraphQL config.manager
      . runMetadata config.services.metadata
      . runGenRandom
      . runTime
      . runDataInversions
      . runDataDatasets
      . runTasks @GenFits fits
      . runTasks @GenAsdf asdf
      . runTasks @PublishTask pubs
      . runTasks @SyncMetadataTask metas
      . runTasks @SyncProposalTask props
      . runMetadataSync sync


waitForGlobusAccess :: (Auth :> es, Concurrent :> es, Log :> es) => Eff (Reader (Token Access) : es) () -> Eff es ()
waitForGlobusAccess work = do
  log Debug "Waiting for Admin Globus Access Token"
  Auth.waitForAccess work


webServer :: Config -> AuthState -> TaskChan GenFits -> TaskChan GenAsdf -> TaskChan PublishTask -> Sync.History -> Application
webServer config auth fits asdf pubs sync =
  liveApp
    document
    (runApp . routeRequest $ router)
 where
  router Dashboard = runPage Dashboard.page
  router Proposals = runPage Proposals.page
  router Inversions = runPage Inversions.page
  router (Proposal ip (Inversion i Inv)) = runPage $ Inversion.page ip i
  router (Proposal p PropRoot) = runPage $ Proposal.page p
  router (Proposal propId (Program progId r)) =
    case r of
      Prog -> runPage $ Program.page propId progId
      SubmitDownload -> Program.submitDownload propId progId
      InvUpload invId -> runPage $ InversionUpload.page propId progId invId
      SubmitUpload invId -> InversionUpload.submitUpload propId progId invId
  router (Datasets DatasetRoot) = runPage Datasets.page
  router (Datasets (Dataset d)) = runPage $ Dataset.page d
  router (Datasets (Sync d)) = runPage $ Sync.page d
  router Experiments = do
    redirect (pathUri . Path True $ routePath Proposals)
  router Logout = runPage Auth.logout
  router Redirect = runPage Auth.login

  runApp :: (IOE :> es, Concurrent :> es) => Eff (Debug : MetadataSync : Tasks PublishTask : Tasks GenAsdf : Tasks GenFits : Auth : Inversions : Datasets : MetadataDatasets : MetadataInversions : GraphQL : Fetch : Rel8 : GenRandom : Reader App : Globus : Scratch : FileSystem : Error GraphQLError : Error GlobusError : Error Rel8Error : Log : Time : es) Response -> Eff es Response
  runApp =
    runTime
      . runLogger "App"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @GlobusError crashWithError
      . runErrorWith @GraphQLError crashWithError
      . runFileSystem
      . runScratch config.scratch
      . runGlobus' config.globus config.manager
      . runReader config.app
      . runGenRandom
      . runRel8 config.db
      . runFetchHttp config.manager
      . runGraphQL config.manager
      . runMetadata config.services.metadata
      . runDataDatasets
      . runDataInversions
      . runAuth config.app.domain Redirect auth
      . runTasks fits
      . runTasks asdf
      . runTasks pubs
      . runMetadataSync sync
      . runDebugIO


-- runGraphQL' :: (IOE :> es, Error GraphQLError :> es) => Bool -> Eff (GraphQL : Fetch : es) a -> Eff es a
-- runGraphQL' True = runFetchMock mockMetadata . runGraphQL
-- runGraphQL' False = runFetchHttp config.manager . runGraphQL

runGlobus' :: forall es a. (Log :> es, IOE :> es, Scratch :> es, Error GlobusError :> es) => GlobusConfig -> Http.Manager -> Eff (Globus : es) a -> Eff es a
runGlobus' (GlobusLive g) mgr action =
  -- catchHttpGlobus (runGlobus g action)
  -- come up with a better way to do this. This catches all IO errors right now
  -- maybe I just need a generic handler...
  runGlobus g mgr action


crashWithError :: (IOE :> es, Log :> es, Show e, Exception e) => CallStack -> e -> Eff es a
crashWithError c e = do
  log Err "Caught: Crashing"
  log Err $ show e
  log Err $ prettyCallStack c
  liftIO $ throwM e

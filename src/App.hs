module App where

import App.Config
import App.Dev.Globus (globusDevAuth, runGlobusDev)
import App.Effect.Auth as Auth
import App.Effect.Scratch (Scratch, runScratch)
import App.Globus as Globus
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
import App.Route
import App.Version
import App.Worker.GenWorker as Gen
import App.Worker.Publish as Publish
import App.Worker.PuppetMaster qualified as PuppetMaster
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
import Effectful.FileSystem
import Effectful.GenRandom
import Effectful.GraphQL hiding (Request (..))
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Rel8 as Rel8
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets (Datasets, runDataDatasets)
import NSO.Data.Inversions (Inversions, runDataInversions)
import NSO.Data.Sync as Sync (History, MetadataSync, initMetadataSync, runMetadataSync)
import NSO.Metadata as Metadata
import NSO.Prelude
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.AddHeaders (addHeaders)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Web.Hyperbole


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  runEff $ runInit $ do
    log Info "NSO Level 2"
    config <- initConfig

    runConcurrent $ do
      fits <- atomically taskChanNew
      asdf <- atomically taskChanNew
      pubs <- atomically taskChanNew
      sync <- initMetadataSync
      auth <- initAuth config.auth.admins config.auth.adminToken

      concurrently_
        (startWebServer config auth fits asdf pubs sync)
        (runWorkers config auth fits asdf pubs sync (startWorkers config))

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

  startGen cfg = do
    runLogger "FitsGen" $
      waitForGlobusAccess $ do
        mapConcurrently_
          id
          [startWorker (Gen.fitsTask cfg.numWorkers), startWorker Gen.asdfTask]

  startWorkers cfg =
    mapConcurrently_
      id
      [ startPuppetMaster
      , startGen cfg
      ]

  runInit =
    runLogger "Init"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @GlobusError crashWithError
      . runFailIO
      . runEnvironment

  runWorkers config auth fits asdf pubs sync =
    runFileSystem
      . runReader config.scratch
      . runRel8 config.db
      . runScratch config.scratch
      . runGlobus' config.globus
      . runAuth config.app.domain Redirect auth
      . runGenRandom
      . runTime
      . runDataInversions
      . runDataDatasets
      . runTasks @GenFits fits
      . runTasks @GenAsdf asdf
      . runTasks @PublishTask pubs
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
  router Experiments = do
    redirect (pathUrl . routePath $ Proposals)
  router Logout = runPage Auth.logout
  router Redirect = runPage Auth.login
  router (Dev DevAuth) = globusDevAuth

  runApp :: (IOE :> es) => Eff (Debug : MetadataSync : Tasks PublishTask : Tasks GenAsdf : Tasks GenFits : Auth : Inversions : Datasets : Metadata : GraphQL : Rel8 : GenRandom : Reader App : Globus : Scratch : FileSystem : Error GraphQLError : Error GlobusError : Error Rel8Error : Log : Concurrent : Time : es) Response -> Eff es Response
  runApp =
    runTime
      . runConcurrent
      . runLogger "App"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @GlobusError crashWithError
      . runErrorWith @GraphQLError crashWithError
      . runFileSystem
      . runScratch config.scratch
      . runGlobus' config.globus
      . runReader config.app
      . runGenRandom
      . runRel8 config.db
      . runGraphQL' config.servicesIsMock
      . runMetadata config.services.metadata
      . runDataDatasets
      . runDataInversions
      . runAuth config.app.domain Redirect auth
      . runTasks fits
      . runTasks asdf
      . runTasks pubs
      . runMetadataSync sync
      . runDebugIO

  runGraphQL' True = runGraphQLMock Metadata.mockRequest
  runGraphQL' False = runGraphQL


runGlobus' :: forall es a. (Log :> es, IOE :> es, Scratch :> es, Error GlobusError :> es) => GlobusConfig -> Eff (Globus : es) a -> Eff es a
runGlobus' (GlobusDev (GlobusDevConfig dkist)) action = runGlobusDev dkist action
runGlobus' (GlobusLive g) action =
  catchHttpGlobus (runGlobus g action)


crashWithError :: (IOE :> es, Log :> es, Show e, Exception e) => CallStack -> e -> Eff es a
crashWithError c e = do
  log Err "Caught: Crashing"
  log Err $ show e
  log Err $ prettyCallStack c
  liftIO $ throwM e

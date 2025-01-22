module App where

import App.Config
import App.Dev.Globus (globusDevAuth, runGlobusDev)
import App.Effect.Auth as Auth
import App.Effect.Scratch (Scratch, runScratch)
import App.Globus as Globus
import App.Page.Auth qualified as Auth
import App.Page.Dashboard qualified as Dashboard
import App.Page.Dataset qualified as Dataset
import App.Page.Inversion qualified as Inversion
import App.Page.Inversions qualified as Inversions
import App.Page.Program qualified as Program
import App.Page.Proposal qualified as Proposal
import App.Page.Proposals qualified as Proposals
import App.Page.Scan qualified as Scan
import App.Route
import App.Version
import App.Worker.GenWorker as Gen
import App.Worker.PuppetMaster qualified as PuppetMaster
import Control.Monad (forever)
import Control.Monad.Catch
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
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
import NSO.Error
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
      auth <- initAuth config.auth.admins config.auth.adminToken

      concurrently_
        (startWebServer config auth fits asdf)
        (runWorkers config auth fits asdf (startWorkers config))

      pure ()
 where
  startPuppetMaster =
    runLogger "Puppet" $
      forever
        PuppetMaster.manageMinions

  startWebServer :: (IOE :> es) => Config -> AuthState -> TaskChan GenFits -> TaskChan GenAsdf -> Eff es ()
  startWebServer config auth fits asdf =
    runLogger "Server" $ do
      log Debug $ "Starting on :" <> show config.app.port
      log Debug $ "Develop using https://" <> cs config.app.domain.unTagged <> "/"
      liftIO $
        Warp.run config.app.port $
          addHeaders [("app-version", cs appVersion)] $
            webServer config auth fits asdf

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
      . runFailIO
      . runEnvironment

  runWorkers config auth fits asdf =
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


waitForGlobusAccess :: (Auth :> es, Concurrent :> es, Log :> es) => Eff (Reader (Token Access) : es) () -> Eff es ()
waitForGlobusAccess work = do
  log Debug "Waiting for Admin Globus Access Token"
  Auth.waitForAccess work


webServer :: Config -> AuthState -> TaskChan GenFits -> TaskChan GenAsdf -> Application
webServer config auth fits asdf =
  liveApp
    document
    (runApp . routeRequest $ router)
 where
  router Dashboard = runPage Dashboard.page
  router Proposals = runPage Proposals.page
  router Inversions = runPage Inversions.page
  router (Proposal ip (Inversion i r)) =
    case r of
      Inv -> runPage $ Inversion.page ip i
      SubmitDownload -> Inversion.submitDownload ip i
      SubmitUpload -> Inversion.submitUpload ip i
  router (Proposal p PropRoot) = runPage $ Proposal.page p
  router (Proposal ip (Program iip)) = runPage $ Program.page ip iip
  router (Dataset d) = runPage $ Dataset.page d
  router Scan = runPage Scan.page
  router Experiments = do
    redirect (pathUrl . routePath $ Proposals)
  router Logout = runPage Auth.logout
  router Redirect = runPage Auth.login
  router (Dev DevAuth) = globusDevAuth

  runApp :: (IOE :> es) => Eff (Tasks GenAsdf : Tasks GenFits : Auth : Inversions : Datasets : Metadata : GraphQL : Rel8 : GenRandom : Reader App : Globus : Scratch : FileSystem : Error DataError : Error Rel8Error : Log : Concurrent : Time : es) a -> Eff es a
  runApp =
    runTime
      . runConcurrent
      . runLogger "App"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @DataError crashWithError
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

  runGraphQL' True = runGraphQLMock Metadata.mockRequest
  runGraphQL' False = runGraphQL


runGlobus' :: (Log :> es, IOE :> es, Scratch :> es) => GlobusConfig -> Eff (Globus : es) a -> Eff es a
runGlobus' (GlobusDev (GlobusDevConfig dkist)) = runGlobusDev dkist
runGlobus' (GlobusLive g) = runGlobus g


crashWithError :: (IOE :> es, Log :> es, Show e, Exception e) => CallStack -> e -> Eff es a
crashWithError c e = do
  log Err "Caught: Crashing"
  log Err $ prettyCallStack c
  liftIO $ throwM e

module App where

import App.Config
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
import App.Worker.FitsGenWorker (GenTask)
import App.Worker.FitsGenWorker qualified as Fits
import App.Worker.PuppetMaster qualified as PuppetMaster
import Control.Monad (forever, void)
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
import Effectful.GraphQL
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Rel8 as Rel8
import Effectful.Time
import Effectful.Worker
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
      auth <- initAuth config.auth.admins config.auth.adminToken

      concurrently_
        (startWebServer config auth fits)
        (runWorkers config auth $ startWorkers fits)

      pure ()
 where
  startPuppetMaster fits =
    runLogger "Puppet" $
      forever $
        PuppetMaster.manageMinions fits

  startWebServer :: (IOE :> es) => Config -> AuthState -> TaskChan GenTask -> Eff es ()
  startWebServer config auth fits =
    runLogger "Server" $ do
      log Debug $ "Starting on :" <> show config.app.port
      log Debug "Develop using https://localhost/"
      liftIO $
        Warp.run config.app.port $
          addHeaders [("app-version", cs appVersion)] $
            webServer config auth fits

  startFitsGen fits = do
    runLogger "FitsGen" $
      waitForGlobusAccess $ do
        mapConcurrently_
          id
          $ flip fmap ([1 .. 8] :: [Int])
          $ \n -> do
            runLogger (ThreadName $ "FitsGen" <> cs (show n)) $
              startWorker fits Fits.workTask

  startWorkers fits =
    mapConcurrently_
      id
      [ startPuppetMaster fits
      , startFitsGen fits
      ]

  runInit =
    runLogger "Init"
      . runErrorWith @Rel8Error crashWithError
      . runFailIO
      . runEnvironment

  runWorkers config auth =
    runFileSystem
      . runReader config.scratch
      . runRel8 config.db
      . runGlobus config.globus
      . runAuth config.app.domain Redirect auth
      . runScratch config.scratch
      . runGenRandom
      . runTime
      . runDataInversions
      . runDataDatasets


startCPUWorkers :: (Log :> es, Concurrent :> es) => Eff es () -> Eff es ()
startCPUWorkers work = do
  n <- availableWorkerCPUs
  -- start one per core
  void $ pooledForConcurrentlyN (max 1 n) [1 .. n :: Int] $ const $ do
    work


waitForGlobusAccess :: (Auth :> es, Concurrent :> es, Log :> es) => Eff (Reader (Token Access) : es) () -> Eff es ()
waitForGlobusAccess work = do
  log Debug "Waiting for Admin Globus Access Token"
  Auth.waitForAccess work


availableWorkerCPUs :: (Concurrent :> es) => Eff es Int
availableWorkerCPUs = do
  let saveCoresForWebserver = 1
  cores <- getNumCapabilities
  pure $ cores - saveCoresForWebserver


webServer :: Config -> AuthState -> TaskChan GenTask -> Application
webServer config auth fits =
  liveApp
    document
    (runApp . routeRequest $ router)
 where
  router Dashboard = page Dashboard.page
  router Proposals = page Proposals.page
  router Inversions = page Inversions.page
  router (Proposal p (Inversion i r)) = page $ Inversion.page p i r
  router (Proposal p PropRoot) = page $ Proposal.page p
  router (Proposal ip (Program iip)) = page $ Program.page ip iip
  router (Dataset d) = page $ Dataset.page d
  router Scan = page Scan.page
  router Experiments = do
    redirect (pathUrl . routePath $ Proposals)
  router Logout = page Auth.logout
  router Redirect = page Auth.login

  runApp :: (IOE :> es) => Eff (Worker GenTask : Scratch : FileSystem : Auth : Inversions : Datasets : Metadata : GraphQL : Rel8 : GenRandom : Reader App : Globus : Error DataError : Error Rel8Error : Log : Concurrent : Time : es) a -> Eff es a
  runApp =
    runTime
      . runConcurrent
      . runLogger "App"
      . runErrorWith @Rel8Error crashWithError
      . runErrorWith @DataError crashWithError
      . runGlobus config.globus
      . runReader config.app
      . runGenRandom
      . runRel8 config.db
      . runGraphQL' config.servicesIsMock
      . runMetadata config.services.metadata
      . runDataDatasets
      . runDataInversions
      . runAuth config.app.domain Redirect auth
      . runFileSystem
      . runScratch config.scratch
      . runWorker fits

  runGraphQL' True = runGraphQLMock Metadata.mockRequest
  runGraphQL' False = runGraphQL


crashWithError :: (IOE :> es, Log :> es, Show e, Exception e) => CallStack -> e -> Eff es a
crashWithError c e = do
  log Err "Caught: Crashing"
  log Err $ prettyCallStack c
  liftIO $ throwM e

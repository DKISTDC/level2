module App where

import App.Config
import App.Globus as Globus
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
import App.Worker.FitsGenWorker qualified as Fits
import App.Worker.PuppetMaster qualified as PuppetMaster
import Control.Monad (forever, void)
import Control.Monad.Catch
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Debug as Debug
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
import NSO.Data.Inversions (GenTask, Inversions, runDataInversions)
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
    logInfo "NSO Level 2"
    config <- initConfig

    runConcurrent $ do
      fits <- atomically taskChanNew
      adtok <- newEmptyTMVarIO

      mapConcurrently_
        id
        [ runLogger "Server" $ startWebServer config adtok fits
        , runAppWorker config "Puppets" $ forever $ do
            PuppetMaster.manageMinions fits
        , runAppWorker config "FitsGen" $ do
            waitForGlobusAccess adtok $ do
              startWorker fits Fits.workTask
        ]

      pure ()
 where
  runInit =
    runLogger "Init"
      . runErrorNoCallStackWith @Rel8Error onRel8Error
      . runFailIO
      . runEnvironment

  runAppWorker config t =
    runLogger t
      . runErrorNoCallStackWith @DataError onDataError
      . runErrorNoCallStackWith @Fits.GenerateError onFitsGenError
      . runReader config.globus.level2
      . runRel8 config.db
      . runGenRandom
      . runTime
      . runFileSystem
      . runGlobus config.globus.client
      . runDataInversions
      . runDataDatasets


startWebServer :: (IOE :> es, Concurrent :> es, Log :> es) => Config -> TMVar (Token Access) -> TaskChan GenTask -> Eff es ()
startWebServer config adtok fits = do
  logDebug $ "Starting on :" <> show config.app.port
  logDebug "Develop using https://localhost/"
  liftIO $
    Warp.run config.app.port $
      addHeaders [("app-version", cs appVersion)] $
        webServer config adtok fits


startCPUWorkers :: (Log :> es, Concurrent :> es) => Eff es () -> Eff es ()
startCPUWorkers work = do
  n <- availableWorkerCPUs
  -- start one per core
  void $ pooledForConcurrentlyN (max 1 n) [1 .. n :: Int] $ const $ do
    work


waitForGlobusAccess :: (Concurrent :> es, Log :> es) => TMVar (Token Access) -> Eff (Reader (Token Access) : es) () -> Eff es ()
waitForGlobusAccess advar work = do
  logDebug "Waiting for Admin Globus Access Token"
  acc <- atomically $ readTMVar advar
  Globus.runWithAccess acc work


availableWorkerCPUs :: (Concurrent :> es) => Eff es Int
availableWorkerCPUs = do
  let saveCoresForWebserver = 1
  cores <- getNumCapabilities
  pure $ cores - saveCoresForWebserver


webServer :: Config -> TMVar (Token Access) -> TaskChan GenTask -> Application
webServer config adtok fits =
  liveApp
    document
    (runApp . routeRequest $ router)
 where
  router Dashboard = page $ Dashboard.page adtok
  router Proposals = page Proposals.page
  router Inversions = page Inversions.page
  router (Inversion i r) = page $ Inversion.page i r
  router (Proposal p) = page $ Proposal.page p
  router (Program p) = page $ Program.page p
  router (Dataset d) = page $ Dataset.page d
  router Scan = page Scan.page
  router Experiments = do
    redirect (pathUrl . routePath $ Proposals)
  router Logout = do
    clearAccessToken
    redirect (pathUrl . routePath $ Proposals)
  router Redirect = do
    code <- reqParam "code"
    red <- getRedirectUri
    tok <- Globus.accessToken red (Tagged code)
    saveAccessToken tok
    _ <- atomically $ tryPutTMVar adtok tok
    redirect $ pathUrl $ routePath Proposals

  runApp :: (IOE :> es) => Eff (Worker GenTask : Log : FileSystem : Reader (GlobusEndpoint App) : Auth : Inversions : Datasets : Debug : Metadata : GraphQL : Rel8 : GenRandom : Reader App : Globus : Error DataError : Error Rel8Error : Concurrent : Time : es) a -> Eff es a
  runApp =
    runTime
      . runConcurrent
      . runErrorNoCallStackWith @Rel8Error onRel8Error
      . runErrorNoCallStackWith @DataError onDataError
      . runGlobus config.globus.client
      . runReader config.app
      . runGenRandom
      . runRel8 config.db
      . runGraphQL' config.servicesIsMock
      . runMetadata config.services.metadata
      . runDebugIO
      . runDataDatasets
      . runDataInversions
      . runAuth config.app.domain Redirect
      . runReader config.globus.level2
      . runFileSystem
      . runLogger "App"
      . runWorker fits

  runGraphQL' True = runGraphQLMock Metadata.mockRequest
  runGraphQL' False = runGraphQL


onDataError :: (IOE :> es) => DataError -> Eff es a
onDataError e = do
  putStrLn "CAUGHT Data Error"
  liftIO $ throwM e


onRel8Error :: (IOE :> es) => Rel8Error -> Eff es a
onRel8Error e = do
  putStrLn "CAUGHT Rel8Error"
  liftIO $ throwM e


onFitsGenError :: (IOE :> es) => Fits.GenerateError -> Eff es a
onFitsGenError e = do
  putStrLn "CAUGHT FitsGenError"
  liftIO $ throwM e

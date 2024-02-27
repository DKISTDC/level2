module App where

import App.Config
import App.Globus as Globus
import App.Page.Dashboard qualified as Dashboard
import App.Page.Dataset qualified as Dataset
import App.Page.Experiment qualified as Experiment
import App.Page.Experiments qualified as Experiments
import App.Page.Inversions qualified as Inversions
import App.Page.Program qualified as Program
import App.Page.Scan qualified as Scan
import App.Route
import App.Version
import Control.Monad.Catch
import Effectful
import Effectful.Debug as Debug
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.GraphQL
import Effectful.Rel8 as Rel8
import Effectful.Time
import NSO.Data.Datasets (Datasets, runDataDatasets)
import NSO.Data.Inversions (Inversions, runDataInversions)
import NSO.Error
import NSO.Metadata as Metadata
import NSO.Prelude
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Web.Hyperbole


main :: IO ()
main = do
  putStrLn "NSO Level 2"
  config <- initConfig onRel8Error
  putStrLn $ "Starting on :" <> show config.app.port
  Warp.run config.app.port $
    addHeaders [("app-version", cs appVersion)] $
      app config


app :: Config -> Application
app config =
  liveApp
    document
    (runApp . routeRequest $ router)
    (runApp . routeRequest $ router)
 where
  -- router :: (Hyperbole :> es, Time :> es, GenRandom :> es, Datasets :> es, Inversions :> es, Metadata :> es, Error DataError :> es, Debug :> es) => AppRoute -> Eff es Response
  router Dashboard = page Dashboard.page
  router Experiments = page Experiments.page
  router Inversions = page Inversions.page
  router (Experiment eid) = page $ Experiment.page eid
  router (Program pid) = page $ Program.page pid
  router (Dataset di) = page $ Dataset.page di
  router Scan = page Scan.page
  router Logout = do
    clearSession "globus"
    redirect (pathUrl . routePath $ Experiments)
  router Redirect = do
    code <- reqParam "code"
    liftIO $ putStrLn "WOOT"
    liftIO $ print code
    tok <- Globus.accessToken (Token code)
    setSession "globus" tok.text
    redirect (pathUrl . routePath $ Experiments)

  runApp :: (IOE :> es) => Eff (Inversions : Datasets : Debug : Metadata : GraphQL : Rel8 : GenRandom : Globus : Error DataError : Error Rel8Error : Time : es) a -> Eff es a
  runApp =
    runTime
      . runErrorNoCallStackWith @Rel8Error onRel8Error
      . runErrorNoCallStackWith @DataError onDataError
      . runGlobus config.globus (redirectUri config.app.domain)
      . runGenRandom
      . runRel8 config.db
      . runGraphQL' config.servicesIsMock
      . runMetadata config.services.metadata
      . runDebugIO
      . runDataDatasets
      . runDataInversions

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

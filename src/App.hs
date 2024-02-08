module App where

import App.Config
import App.Error
import App.Page.Dashboard qualified as Dashboard
import App.Page.Dataset qualified as Dataset
import App.Page.Experiment qualified as Experiment
import App.Page.Experiments qualified as Experiments
import App.Page.Program qualified as Program
import App.Page.Scan qualified as Scan
import App.Route
import App.Version
import Control.Monad.Catch
import Effectful
import Effectful.Debug (Debug, runDebugIO)
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.GraphQL
import Effectful.Reader.Static
import Effectful.Rel8 as Rel8
import Effectful.Time
import NSO.Data.Datasets (Datasets, runDataDatasets)
import NSO.Data.Inversions (Inversions, runDataInversions)
import NSO.Metadata as Metadata
import NSO.Prelude
import Network.Wai.Handler.Warp as Warp (Port, run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import System.Environment (getEnv)
import Web.Hyperbole


main :: IO ()
main = do
  putStrLn "NSO Level 2?"
  (conn, port) <- initialize
  (services, isMock) <- initServices
  putStrLn $ "Starting on :" <> show port
  Warp.run port $
    addHeaders [("app-version", cs appVersion)] $
      app conn services isMock


initialize :: IO (Rel8.Connection, Port)
initialize = do
  port <- read <$> getEnv "PORT"
  postgres <- getEnv "DATABASE_URL"
  conn <- runEff . runErrorNoCallStackWith @Rel8Error onRel8Error $ Rel8.connect $ cs postgres
  pure (conn, port)


app :: Rel8.Connection -> Services -> IsMock -> Application
app conn services isMock = application document (runApp . router)
 where
  router :: (Hyperbole :> es, Time :> es, GenRandom :> es, Datasets :> es, Inversions :> es, Metadata :> es, Error AppError :> es, Reader Services :> es, Debug :> es) => AppRoute -> Eff es ()
  router Dashboard = page Dashboard.page
  router Experiments = page Experiments.page
  router (Experiment eid) = page $ Experiment.page eid
  router (Program pid) = page $ Program.page pid
  router (Dataset di) = page $ Dataset.page di
  router Scan = page Scan.page

  runApp =
    runTime
      . runErrorNoCallStackWith @Rel8Error onRel8Error
      . runErrorNoCallStackWith @AppError onAppError
      . runReader services
      . runGenRandom
      . runRel8 conn
      . runGraphQL' isMock
      . runMetadata services.metadata
      . runDebugIO
      . runDataDatasets
      . runDataInversions

  runGraphQL' True = runGraphQLMock Metadata.mockRequest
  runGraphQL' False = runGraphQL


onRel8Error :: (IOE :> es) => Rel8Error -> Eff es a
onRel8Error e = do
  putStrLn "CAUGHT"
  liftIO $ throwM e


onAppError :: (IOE :> es) => AppError -> Eff es a
onAppError e = do
  putStrLn "CAUGHT"
  liftIO $ throwM e

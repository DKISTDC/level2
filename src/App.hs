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
import Control.Monad.Catch
import Effectful
import Effectful.Debug as Debug
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.GraphQL
import Effectful.Reader.Dynamic
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
  router Dashboard = page Dashboard.page
  router Proposals = page Proposals.page
  router Inversions = page Inversions.page
  router (Inversion i) = page $ Inversion.page i
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
    tok <- Globus.accessToken (Tagged code)
    saveAccessToken tok
    redirect (pathUrl . routePath $ Proposals)
  router (Transfer inv) = page $ Globus.handleTransfer inv

  runApp :: (IOE :> es) => Eff (Inversions : Datasets : Debug : Metadata : GraphQL : Rel8 : GenRandom : Reader App : Globus : Error DataError : Error Rel8Error : Time : es) a -> Eff es a
  runApp =
    runTime
      . runErrorNoCallStackWith @Rel8Error onRel8Error
      . runErrorNoCallStackWith @DataError onDataError
      . runGlobus config.globus (redirectUri config.app.domain)
      . runReader config.app
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

module App where

import App.Page.Dashboard qualified as Dashboard
import App.Page.Dataset qualified as Dataset
import App.Page.Experiment qualified as Experiment
import App.Page.Experiments qualified as Experiments
import App.Page.Program qualified as Program
import App.Page.Scan qualified as Scan
import App.Route
import App.Version
import Control.Monad.Catch
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Effectful
import Effectful.Debug (runDebugIO)
import Effectful.Error.Static
import Effectful.Rel8 as Rel8
import Effectful.Request
import Effectful.Time (runTime)
import NSO.Metadata qualified as Metadata
import NSO.Prelude
import Network.Wai.Middleware.AddHeaders (addHeaders)
import System.Environment (getEnv)
import Web.Hyperbole

main :: IO ()
main = do
  putStrLn "Starting on :3000"
  conn <- initialize
  run 3000
    $ addHeaders [("app-version", cs appVersion)]
    $ app conn

initialize :: IO Rel8.Connection
initialize = do
  postgres <- getEnv "DATABASE_URL"
  runEff . runErrorNoCallStackWith @Rel8Error onRel8Error $ Rel8.connect $ cs postgres

app :: Rel8.Connection -> Application
app conn = waiApplication document (runApp . router)
 where
  -- router :: (Hyperbole :> es, Rel8 :> es, Debug :> es, GraphQL :> es, Error RequestError :> es) => AppRoute -> Eff es ()
  router Dashboard = page Dashboard.page
  router Experiments = page Experiments.page
  router (Experiment eid) = page $ Experiment.page eid
  router (Program pid) = page $ Program.page pid
  router (Dataset di) = page $ Dataset.page di
  router Scan = page Scan.page

  runApp =
    runTime
      . runErrorNoCallStackWith @Rel8Error onRel8Error
      . runErrorNoCallStackWith @RequestError onRequestError
      . runRel8 conn
      . runRequestMock Metadata.mockRequest -- .runRequest
      . runHyperbole
      . runDebugIO
      . runGraphQL

onRel8Error :: (IOE :> es) => Rel8Error -> Eff es a
onRel8Error e = do
  putStrLn "CAUGHT"
  liftIO $ throwM e

onRequestError :: (IOE :> es) => RequestError -> Eff es a
onRequestError e = do
  putStrLn "CAUGHT"
  liftIO $ throwM e

-- handle (Contacts rt) = Contacts.routes rt

document :: BL.ByteString -> BL.ByteString
document cnt =
  [i|<html>
    <head>
      <title>Level2</title>
      <script type="text/javascript">#{scriptEmbed}</script>
      <style type="text/css">#{cssResetEmbed}</style>
      <style type="text/css">body { background-color: \#F2F2F3 }</style>
    </head>
    <body>#{cnt}</body>
  </html>|]

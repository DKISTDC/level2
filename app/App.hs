module App where

import App.Page.Dashboard as Dashboard
import Data.Version (showVersion)
import NSO.Prelude
import Control.Monad.Catch
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Paths_nso_level2 (version)
import Effectful
import Effectful.Error.Static
import Web.Scotty.Trans as Scotty
import System.Environment (getEnv)
import Effectful.Rel8 as Rel8
import Hasql.Connection (Connection)
-- import Web.Htmx
-- import Web.Hyperbole
-- import Web.Hyperbole.Htmx

app :: IO ()
app = do
  postgres <- getEnv "POSTGRES"
  conn <- runEff . runErrorNoCallStackWith @Rel8Error onRel8Error $ Rel8.connect $ cs postgres

  scottyT 3001 (runApp conn) $ do
    middleware appInfo
    get "/version" $ Scotty.text appVersion
    Dashboard.route
 where
  appInfo = addHeaders [("Service", cs appVersion)]
  appVersion = "NSO L2 " <> cs (showVersion Paths_nso_level2.version)

  onRel8Error :: IOE :> es => Rel8Error -> Eff es a
  onRel8Error e = do
    putStrLn "CAUGHT"
    liftIO $ throwM e

  runApp :: Connection -> Eff '[Rel8, Error Rel8Error, IOE] a -> IO a
  runApp conn = do
    runEff . runErrorNoCallStackWith @Rel8Error onRel8Error . runRel8 conn


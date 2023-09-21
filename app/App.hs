module App where

import App.Page.Dashboard as Dashboard
import Data.Version (showVersion)
import NSO.Prelude
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Paths_nso_level2 (version)
import Web.Htmx
import Web.Hyperbole
import Web.Hyperbole.Htmx
import Web.Scotty

-- OK! We want to sync some shit
app :: IO ()
app = do
  scotty 3001 $ do
    middleware appInfo
    get "/version" $ Web.Scotty.text appVersion
    Dashboard.route
 where
  appInfo = addHeaders [("Service", cs appVersion)]
  appVersion = "NSO L2 " <> cs (showVersion Paths_nso_level2.version)


module App where

import App.Page.Dashboard qualified as Dashboard
import Control.Monad.Catch
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 as Rel8
import NSO.Prelude
import System.Environment (getEnv)
import Web.Hyperbole
import Web.UI

main :: IO ()
main = do
  postgres <- getEnv "DATABASE_URL"
  conn <- runEff . runErrorNoCallStackWith @Rel8Error onRel8Error $ Rel8.connect $ cs postgres
  putStrLn "Starting on :3000"
  run 3000 $ app conn

app :: Rel8.Connection -> Application
app conn = application (runApp conn . route)
 where
  route :: (Wai :> es, Rel8 :> es) => Route -> Eff es ()
  route Main = redirect (routeUrl $ Dashboard defRoute)
  route (Dashboard d) = Dashboard.route d
  route (Hello h) = view $ row_ $ do
    text "HELLO "
    text h
  route Echo = do
    f <- formData
    view $ col id $ do
      el id "ECHO:"
      text $ cs $ show f

runApp :: (IOE :> es) => Connection -> Eff (Rel8 : Error Rel8Error : es) a -> Eff es a
runApp conn = runErrorNoCallStackWith @Rel8Error onRel8Error . runRel8 conn

onRel8Error :: (IOE :> es) => Rel8Error -> Eff es a
onRel8Error e = do
  putStrLn "CAUGHT"
  liftIO $ throwM e

-- handle (Contacts rt) = Contacts.routes rt

-- send Respond
data Route
  = Main
  | Dashboard Dashboard.Route
  | Hello Text
  | Echo
  deriving (Show, Generic, Eq, PageRoute)

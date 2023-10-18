module App where

import App.Page.Dashboard qualified as Dashboard
import Control.Monad.Catch
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Effectful
import Effectful.Error.Static
import Effectful.Rel8 as Rel8
import Effectful.Request
import Effectful.Time (Time, runTime)
import NSO.Metadata qualified as Metadata
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
app conn = waiApplication document (runApp . router)
 where
  router :: (Page :> es, Rel8 :> es, GraphQL :> es, Time :> es, Error RequestError :> es) => AppRoute -> Eff es ()
  router Dashboard = Dashboard.page
  router (Hello h) = view $ row_ $ do
    text "HELLO "
    text h
  router Echo = do
    f <- formData
    view $ col id $ do
      el id "ECHO:"
      text $ cs $ show f

  runApp =
    runErrorNoCallStackWith @Rel8Error onRel8Error
      . runErrorNoCallStackWith @RequestError onRequestError
      . runRel8 conn
      . runRequestMock Metadata.mockRequest
      . runTime
      . runPageWai
      -- . runRequest
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

-- send Respond
data AppRoute
  = Dashboard
  | Hello Text
  | Echo
  deriving (Show, Generic, Eq, Route)

document :: BL.ByteString -> BL.ByteString
document cnt =
  [i|<html>
    <head>
      <title>Level2</title>
      <script type="text/javascript">#{scriptEmbed}</script>
      <style type type="text/css">#{cssResetEmbed}</style>
    </head>
    <body>#{cnt}</body>
  </html>|]

{-# LANGUAGE UndecidableInstances #-}

module App.Page.Dashboard where

-- import App.Colors as Colors
-- import App.View.DataRow qualified as View
-- import App.View.Icons qualified as Icons
-- import App.Worker.Generate
-- import Data.Text (pack)
import App.Effect.Auth
import App.Route
import App.Style qualified as Style
import App.Version
import App.View.Layout
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem
import Effectful.Log
import Effectful.Tasks
import NSO.Data.Datasets
import NSO.Prelude
import Network.Globus (Token, Token' (Access))
import Web.Atomic.CSS
import Web.Hyperbole


page
  :: (Concurrent :> es, IOE :> es, Log :> es, FileSystem :> es, Hyperbole :> es, Auth :> es, Datasets :> es, Tasks :> es)
  => Page es '[Work]
page = do
  -- login <- loginUrl
  appLayout Dashboard mainView
 where
  mainView :: View (Root '[Work]) ()
  mainView = do
    col ~ pad 20 . gap 20 $ do
      col $ do
        el ~ fontSize 24 . bold $ "Level 2"
        el $ text appVersion.value
        link gitVersionURI ~ Style.link $ text gitVersion.value

      col $ do
        el ~ bold . fontSize 18 $ "Admin"

      -- hyper Test testView
      hyper Work $ el "TODO"


-- row $ do
-- case admin.token of
--   Nothing -> link admin.loginUrl ~ Style.btnOutline Danger $ "Needs Globus Login"
--   Just _ -> do
--     el ~ color Success $ "System Access Token Saved!"

data AdminLogin = AdminLogin
  { token :: Maybe (Token Access)
  , loginUrl :: URI
  }


data Work = Work
  deriving (Generic, ViewId)


instance (Concurrent :> es, Tasks :> es) => HyperView Work es where
  data Action Work = Refresh
    deriving (Generic, ViewAction)


  update Refresh = do
    _gens <- send TaskAll
    -- pure $ workView gens
    pure "TODO"

-- workView :: [Task'] -> View Work ()
-- workView gens = do
--   col ~ gap 20 @ onLoad Refresh 1000 $ do
--     col ~ Style.card $ do
--       el ~ Style.cardHeader Colors.Info $ do
--         el ~ bold . fontSize 18 $ "Inversion Generation"
--       taskTable gens
--
--
-- taskTable :: forall task. (WorkerTask task, Eq (Status task), Show (Status task), Show task) => [Task task] -> View Work ()
-- taskTable tasks = do
--   table tasks ~ View.table $ do
--     tcol (View.hd "Task") $ \w -> View.cell $ text $ pack $ show w.task
--     tcol (View.hd "Status") $ \w -> View.cell $ status w.status
--  where
--   status :: Status task -> View Work ()
--   status s
--     | s == idle @task = el "Idle"
--     | otherwise =
--         row ~ gap 5 $ do
--           el ~ width 20 $ Icons.spinnerCircle
--           el ~ color Colors.Info . italic $ text $ pack $ show s

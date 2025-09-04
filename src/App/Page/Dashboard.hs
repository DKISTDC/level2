{-# LANGUAGE UndecidableInstances #-}

module App.Page.Dashboard where

import App.Colors as Colors
import App.Effect.Auth
import App.Route
import App.Style qualified as Style
import App.Version
import App.View.DataRow qualified as View
import App.View.Icons qualified as Icons
import App.View.Layout
import App.Worker.GenAsdf
import App.Worker.GenFits
import Data.Text (pack)
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem
import Effectful.Log
import Effectful.Tasks
import NSO.Data.Datasets
import NSO.Files.Scratch (Scratch)
import NSO.Prelude
import Network.Globus (Token, Token' (..))
import Web.Atomic.CSS
import Web.Hyperbole


page
  :: (Concurrent :> es, Log :> es, FileSystem :> es, Hyperbole :> es, Auth :> es, Datasets :> es, Scratch :> es, Tasks GenFits :> es)
  => Page es '[Work]
page = do
  login <- loginUrl
  mtok <- send AdminToken
  appLayout Dashboard (mainView $ AdminLogin mtok login)
 where
  mainView :: AdminLogin -> View (Root '[Work]) ()
  mainView admin =
    col ~ pad 20 . gap 20 $ do
      col $ do
        el ~ fontSize 24 . bold $ "Level 2"
        el $ text $ cs appVersion

      col $ do
        el ~ bold . fontSize 18 $ "Admin"
        row $ do
          case admin.token of
            Nothing -> link admin.loginUrl ~ Style.btnOutline Danger $ "Needs Globus Login"
            Just _ -> do
              el ~ color Success $ "System Access Token Saved!"

      -- hyper Test testView
      hyper Work $ workView [] []


data AdminLogin = AdminLogin
  { token :: Maybe (Token Access)
  , loginUrl :: URI
  }


data Work = Work
  deriving (Generic, ViewId)


instance (Concurrent :> es, Tasks GenFits :> es, Tasks GenAsdf :> es) => HyperView Work es where
  data Action Work = Refresh
    deriving (Generic, ViewAction)


  update Refresh = do
    fits <- allTasks
    asdf <- allTasks
    pure $ workView fits asdf


allTasks :: forall task es. (Tasks task :> es, WorkerTask task) => Eff es [(task, Status task)]
allTasks = do
  wait <- send TasksWaiting
  work <- send TasksWorking
  pure $ fmap (,idle @task) wait <> work


workView :: [(GenFits, GenFitsStatus)] -> [(GenAsdf, ())] -> View Work ()
workView fits asdf = do
  col ~ gap 20 @ onLoad Refresh 1000 $ do
    col ~ Style.card $ do
      el ~ Style.cardHeader Colors.Info $ do
        el ~ bold . fontSize 18 $ "Fits Generation"
      taskTable fits
    col ~ Style.card $ do
      el ~ Style.cardHeader Colors.Info $ do
        el ~ bold . fontSize 18 $ "Asdf Generation"
      taskTable asdf


taskTable :: forall task. (WorkerTask task, Eq (Status task), Show (Status task), Show task) => [(task, Status task)] -> View Work ()
taskTable tasks = do
  table tasks ~ View.table $ do
    tcol (View.hd "Task") $ \w -> View.cell $ text $ pack $ show $ fst w
    tcol (View.hd "Status") $ \w -> View.cell $ status $ snd w
 where
  status :: Status task -> View Work ()
  status s
    | s == idle @task = el "Idle"
    | otherwise =
        row ~ gap 5 $ do
          el ~ width 20 $ Icons.spinnerCircle
          el ~ color Colors.Info . italic $ text $ pack $ show s

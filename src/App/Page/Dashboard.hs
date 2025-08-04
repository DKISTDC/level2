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
import App.Worker.GenWorker
import Data.Text (pack)
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem
import Effectful.Log
import Effectful.Tasks
import NSO.Data.Datasets
import NSO.Data.Scratch (Scratch)
import NSO.Prelude
import Network.Globus (Token, Token' (..))
import Web.Atomic.CSS
import Web.Hyperbole


page
  :: (Concurrent :> es, Log :> es, FileSystem :> es, Hyperbole :> es, Auth :> es, Datasets :> es, Scratch :> es, Tasks GenFits :> es)
  => Eff es (Page '[Work])
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


instance (Concurrent :> es, Tasks GenFits :> es) => HyperView Work es where
  data Action Work = Refresh
    deriving (Generic, ViewAction)


  update Refresh = do
    wt <- send TasksWaiting
    wk <- send TasksWorking
    pure $ workView wt wk


workView :: [GenFits] -> [(GenFits, GenFitsStatus)] -> View Work ()
workView waiting working = do
  let allTasks = working <> fmap (,GenWaiting) waiting
  col ~ gap 10 @ onLoad Refresh 1000 $ do
    col ~ Style.card $ do
      el ~ Style.cardHeader Colors.Info $ do
        el ~ bold . fontSize 18 $ "Fits Generation"
      table allTasks ~ View.table $ do
        tcol (View.hd "Task") $ \w -> View.cell $ text $ pack $ show $ fst w
        tcol (View.hd "Status") $ \w -> View.cell $ status $ snd w
 where
  status GenWaiting =
    el "Waiting"
  status s =
    row ~ gap 5 $ do
      el ~ width 20 $ Icons.spinnerCircle
      el ~ color Colors.Info . italic $ text $ pack $ show s

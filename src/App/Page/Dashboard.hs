{-# LANGUAGE UndecidableInstances #-}

module App.Page.Dashboard where

import App.Colors as Colors
import App.Effect.Auth
import App.Effect.Scratch (Scratch)
import App.Globus
import App.Route
import App.Style qualified as Style
import App.Version
import App.View.DataRow qualified as View
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
import NSO.Prelude
import Web.Hyperbole


-- import NSO.Fits.Generate.FetchL1
-- import NSO.Types.InstrumentProgram

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
    col (pad 20 . gap 20) $ do
      col id $ do
        el (fontSize 24 . bold) "Level 2"
        el_ $ text $ cs appVersion

      col id $ do
        el (bold . fontSize 18) "Admin"
        row id $ do
          case admin.token of
            Nothing -> link admin.loginUrl (Style.btnOutline Danger) "Needs Globus Login"
            Just _ -> do
              el (color Success) "System Access Token Saved!"

      -- hyper Test testView
      hyper Work $ workView [] []


data AdminLogin = AdminLogin
  { token :: Maybe (Token Access)
  , loginUrl :: Url
  }


-- data Test = Test
--   deriving (Show, Read, ViewId)
--
--
-- instance HyperView Test es where
--   data Action Test
--     = DownloadL1
--     | ScanL1
--     deriving (Show, Read, ViewAction)

-- -- "~/Data/pid_2_95/AOPPO"
-- test :: (Log :> es, FileSystem :> es, Concurrent :> es, Datasets :> es, Globus :> es, Reader (GlobusEndpoint App) :> es) => TMVar (Token Access) -> Test -> TestAction -> Eff es (View Test ())
-- test adtok _ DownloadL1 = do
--   logDebug "TEST"
--   let ip = Id "id.118958.452436" :: Id InstrumentProgram
--   logTrace "IP" ip
--
--   t <- fromMaybe (error "Missing admin token") <$> atomically (tryReadTMVar adtok)
--   d <- fromMaybe (error "Missing canonical dataset") <$> findCanonicalDataset ip
--   (task, fp) <- runWithAccess t $ transferCanonicalDataset d
--   logTrace "Task" task
--   logTrace "File" fp
--
--   pure testView
-- test _ _ ScanL1 = do
--   let dir = Path "/Users/seanhess/Data/pid_2_95/AOPPO"
--   fs <- listL1Frames dir
--   mapM_ (logTrace "frame") $ filter ((== I) . (.stokes)) fs
--   pure testView
--
--
-- testView :: View Test ()
-- testView = col (gap 5) $ do
--   el (bold . fontSize 18) "Test"
--   button DownloadL1 (Style.btn Primary) "Download"
--
--   button ScanL1 (Style.btn Primary) "Scan"

data Work = Work
  deriving (Show, Read, ViewId)


instance (Concurrent :> es, Tasks GenFits :> es) => HyperView Work es where
  data Action Work = Refresh
    deriving (Show, Read, ViewAction)


  update Refresh = do
    wt <- send TasksWaiting
    wk <- send TasksWorking
    pure $ workView wt wk


workView :: [GenFits] -> [(GenFits, GenFitsStatus)] -> View Work ()
workView waiting working =
  col (gap 10 . onLoad Refresh 1000) $ do
    col Style.card $ do
      el (Style.cardHeader Colors.Info) $ do
        el (bold . fontSize 18) "Fits Working"
      table View.table working $ do
        tcol (View.hd "Task") $ \w -> View.cell $ text $ pack $ show $ fst w
        tcol (View.hd "Status") $ \w -> View.cell $ text $ pack $ show $ snd w

    col Style.card $ do
      el (Style.cardHeader Colors.Secondary) $ do
        el (bold . fontSize 18) "Fits Waiting"
      table View.table waiting $ do
        tcol (View.hd "Task") $ \w -> View.cell $ text $ pack $ show w

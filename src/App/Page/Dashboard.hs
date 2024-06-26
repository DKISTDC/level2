module App.Page.Dashboard where

import App.Colors
import App.Effect.Scratch (Scratch)
import App.Globus
import App.Route
import App.Style qualified as Style
import App.Version
import App.View.Layout
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem
import Effectful.Log
import Effectful.Worker
import NSO.Data.Datasets
import NSO.Data.Inversions
import NSO.Prelude
import Web.Hyperbole


-- import NSO.Fits.Generate.FetchL1
-- import NSO.Types.InstrumentProgram

page
  :: (Log :> es, FileSystem :> es, Globus :> es, Hyperbole :> es, Concurrent :> es, Auth :> es, Datasets :> es, Scratch :> es, Worker GenTask :> es)
  => TMVar (Token Access)
  -> Page es Response
page adtok = do
  -- handle $ test adtok
  handle work
  load $ do
    login <- loginUrl
    mtok <- atomically $ tryReadTMVar adtok

    appLayout Dashboard (mainView login mtok)
 where
  mainView :: Url -> Maybe (Token Access) -> View c ()
  mainView login mtok = col (pad 20 . gap 20) $ do
    col id $ do
      el (fontSize 24 . bold) "Level 2"
      el_ $ text $ cs appVersion

    col id $ do
      el (bold . fontSize 18) "Admin"
      row id $ do
        case mtok of
          Nothing -> link login (Style.btnOutline Danger) "Needs Globus Login"
          Just _ -> el (color Success) "System Access Token Saved!"

    -- hyper Test testView
    hyper Work $ workView [] []


data Test = Test
  deriving (Generic, ViewId)


data TestAction
  = DownloadL1
  | ScanL1
  deriving (Generic, ViewAction)


instance HyperView Test where
  type Action Test = TestAction


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
  deriving (Generic, ViewId)


data WorkAction = Refresh
  deriving (Generic, ViewAction)


instance HyperView Work where
  type Action Work = WorkAction


work :: (Concurrent :> es, Worker GenTask :> es) => Work -> WorkAction -> Eff es (View Work ())
work _ Refresh = do
  wt <- send TasksWaiting
  wk <- send TasksWorking
  pure $ workView wt wk


workView :: [GenTask] -> [(GenTask, GenStatus)] -> View Work ()
workView waiting working =
  onLoad Refresh 1000 $ do
    el (bold . fontSize 18) "Fits Working"
    forM_ working $ \(t, s) -> do
      row (gap 5) $ do
        el_ $ text $ cs $ show t
        el_ $ text $ cs $ show s

    el (bold . fontSize 18) "Fits Waiting"
    forM_ waiting $ \t -> do
      el_ $ text $ cs $ show t

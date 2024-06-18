module App.Page.Dashboard where

import App.Colors
import App.Globus
import App.Route
import App.Style qualified as Style
import App.Types
import App.Version
import App.View.Layout
import App.Worker.FitsGenWorker qualified as FitsGenWorker
import App.Worker.TaskChan
import Effectful
import Effectful.Concurrent.STM
import Effectful.FileSystem
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Data.Datasets
import NSO.Fits.Generate.FetchL1
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole


page
  :: (Log :> es, FileSystem :> es, Globus :> es, Hyperbole :> es, Concurrent :> es, Auth :> es, Datasets :> es, Reader (GlobusEndpoint App) :> es)
  => TMVar (Token Access)
  -> TaskChan FitsGenWorker.Task
  -> Page es Response
page adtok fits = do
  handle $ test adtok
  handle $ work fits
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

    hyper Test testView
    hyper Work $ workView $ TaskChanStatus mempty mempty


data Test = Test
  deriving (Generic, ViewId)


data TestAction
  = DownloadL1
  | ScanL1
  deriving (Generic, ViewAction)


instance HyperView Test where
  type Action Test = TestAction


-- "~/Data/pid_2_95/AOPPO"
test :: (Log :> es, FileSystem :> es, Concurrent :> es, Datasets :> es, Globus :> es, Reader (GlobusEndpoint App) :> es) => TMVar (Token Access) -> Test -> TestAction -> Eff es (View Test ())
test adtok _ DownloadL1 = do
  logDebug "TEST"
  let ip = Id "id.118958.452436" :: Id InstrumentProgram
  logTrace "IP" ip

  t <- fromMaybe (error "Missing admin token") <$> atomically (tryReadTMVar adtok)
  d <- fromMaybe (error "Missing canonical dataset") <$> findCanonicalDataset ip
  (task, fp) <- runWithAccess t $ transferCanonicalDataset d
  logTrace "Task" task
  logTrace "File" fp

  pure testView
test _ _ ScanL1 = do
  let dir = Path "/Users/seanhess/Data/pid_2_95/AOPPO"
  fs <- listL1Frames dir
  mapM_ (logTrace "frame") $ filter ((== I) . (.stokes)) fs
  pure testView


testView :: View Test ()
testView = col (gap 5) $ do
  el (bold . fontSize 18) "Test"
  button DownloadL1 (Style.btn Primary) "Download"

  button ScanL1 (Style.btn Primary) "Scan"


data Work = Work
  deriving (Generic, ViewId)


data WorkAction = Refresh
  deriving (Generic, ViewAction)


instance HyperView Work where
  type Action Work = WorkAction


work :: (Concurrent :> es) => TaskChan FitsGenWorker.Task -> Work -> WorkAction -> Eff es (View Work ())
work fits _ Refresh = do
  s <- atomically $ taskChanStatus fits
  pure $ workView s


workView :: TaskChanStatus FitsGenWorker.Task -> View Work ()
workView fits =
  onLoad Refresh 1000 $ do
    el (bold . fontSize 18) "Fits Working"
    forM_ fits.work $ \t -> do
      el_ $ text $ cs $ show t

    el (bold . fontSize 18) "Fits Waiting"
    forM_ fits.wait $ \t -> do
      el_ $ text $ cs $ show t

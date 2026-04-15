{-# LANGUAGE UndecidableInstances #-}

module App.Page.Tasks where

-- import App.Worker.SyncMetadata
import App.Colors
import App.Route
import App.Style qualified as Style
import App.View.Icons qualified as Icons
import App.View.Layout
import Data.List qualified as L
import Data.Text qualified as T
import Effectful.Tasks
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole


-- we need to be able to query all the tasks here!
page
  :: (Hyperbole :> es, Tasks :> es)
  => Page es '[TasksTable, TaskMonitor]
page = do
  -- login <- loginUrl
  ts :: [Task'] <- L.sort <$> send TaskAll
  appLayout Dashboard $ do
    col ~ pad 20 . gap 20 $ do
      col ~ gap 10 $ do
        -- el ~ Style.cardHeader Primary $ "Active Tasks"
        hyper TasksTable $ tasksTable ts


spinner :: View c ()
spinner = do
  el ~ width 24 . height 24 . cls "loader" $ none


data TasksTable = TasksTable
  deriving (Generic, ViewId)


-- refresh it periodically?
-- how to get NEW tasks added?
-- let's start by updating each row
instance (Tasks :> es) => HyperView TasksTable es where
  data Action TasksTable = Refresh
    deriving (Generic, ViewAction)


  type Require TasksTable = '[TaskMonitor]


  update Refresh = do
    ts :: [Task'] <- L.sort <$> send TaskAll
    pure $ tasksTable ts


tasksTable :: [Task'] -> View TasksTable ()
tasksTable ts = do
  col ~ gap 0 @ onLoad Refresh 5000 $ do
    forM_ ts $ \t -> do
      hyper (TaskMonitor t.task) (taskMonitor t)


data TaskMonitor = TaskMonitor TaskId
  deriving (Generic, ViewId)


instance (Tasks :> es) => HyperView TaskMonitor es where
  data Action TaskMonitor = Stream
    deriving (Generic, ViewAction)


  update _ = do
    TaskMonitor t <- viewId
    mt <- taskWatch t $ \tsk -> do
      pushUpdate $ taskRow tsk

    -- BUG: the spinnerCircle doesn't like getting updated. Errors
    pure $ maybe none taskRow mt


taskMonitor :: Task' -> View TaskMonitor ()
taskMonitor t = do
  taskRow t @ onLoad Stream 100


taskRow :: Task' -> View TaskMonitor ()
taskRow t = do
  el ~ pad (T 10) $ do
    col ~ gap 10 . pad 10 . Style.card $ do
      el ~ Style.cardHeader (workClr t.working) $ do
        row ~ gap 10 $ do
          el ~ bold $ text t.task.value
          space
          working t.working
      col ~ gap 10 $ do
        el $ text $ T.take 100 t.status.value
        el $ do
          maybe none (text . cs) t.error
 where
  working = \case
    TaskWaiting -> el "..."
    TaskFailed -> el ~ color Danger . width 20 $ Icons.xMark
    TaskWorking -> spinner

  workClr = \case
    TaskWaiting -> Secondary
    TaskFailed -> Danger
    TaskWorking -> Primary

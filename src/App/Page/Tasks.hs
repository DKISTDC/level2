module App.Page.Tasks where

import App.Colors
import App.Route
import App.Style qualified as Style
import App.View.DataRow (cell, hd)
import App.View.DataRow qualified as View
import App.View.Icons as Icons
import App.View.Layout
import App.Worker.SyncMetadata
import Effectful.Rel8
import Effectful.Tasks
import Effectful.Tasks.TasksRel8 (Task' (..))
import Effectful.Tasks.TasksRel8 qualified as TasksDB
import Effectful.Tasks.WorkerTask (TaskWorking (..))
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole


-- we need to be able to query all the tasks here!
page
  :: (Hyperbole :> es, Rel8 :> es)
  => Page es '[TasksTable]
page = do
  -- login <- loginUrl
  ts :: [Task' Text Identity] <- TasksDB.genericTaskList
  appLayout Dashboard $ do
    col ~ pad 20 . gap 20 $ do
      col ~ Style.card . gap 15 $ do
        -- el ~ Style.cardHeader Primary $ "Active Tasks"
        hyper TasksTable $ tasksTable ts


data TasksTable = TasksTable
  deriving (Generic, ViewId)


instance HyperView TasksTable es where
  data Action TasksTable = Refresh
    deriving (Generic, ViewAction)


  update Refresh = do
    pure "WOOT"


-- should i just display them all in a table, and update each one?
-- if it has errored, I don't need to
-- I would LIKE to display them in two tables: Active, and Error. If it errors, move it to errors
-- but... how? Refresh it! Send a message, push an update. What does that do?
--
-- well, ultimately, I don't want them to be durable... Just - interprocess
-- I'd rather have the guarantee that everything is cleaned up when the process exits. But I can't know that of course
--
-- What about errors? How are they reported? Only to the admin tool? We can have a worker listening for failures. Listening for status changes
--

-- TODO: what about interrupted tasks? They aren't in the queue at all! Should we add them on startup? Probably not.  Should we remove them on close, .... hmm....

tasksTable :: [Task' Text Identity] -> View TasksTable ()
tasksTable ts = do
  table ts ~ View.table $ do
    -- redundant, the task queue is in the id
    -- tcol (hd "Queue") $ \t -> cell $ text . cs $ t.taskQueue
    tcol (hd "Id") $ \t -> cell $ text $ cs t.taskId
    tcol (hd "") $ \t -> cell $ do
      row $ do
        space
        working t.taskWorking
        space
    -- tcol (hd "Error") $ \t -> cell $ text . cs $ t.taskStatus
    tcol (hd "Status") $ \t -> cell $ text . cs $ t.taskStatus
 where
  working = \case
    TaskWaiting -> el "..."
    TaskFailed -> el ~ color Danger . width 20 $ Icons.xMark
    TaskWorking -> el ~ width 20 $ Icons.spinnerCircle

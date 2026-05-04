{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}

module Effectful.Tasks
  ( -- * Queue
    Queue (..)
  , queueAdd
  , queueAddAll
  , queueNext

    -- ** TChan/IO Task Queues
  , QueueChan
  , runQueueIO
  , initQueueIO
  , queueChanNew

    -- ** AMQP Task Queues
  , QueueAMQP
  , runQueueAMQP
  , initQueueAMQP

    -- * Tasks
  , Tasks (..)
  , Task (..)
  , Task' (..)
  , TaskId (..)
  , TaskStatus (..)
  , WorkerTask (..)
  , TaskWorking (..)
  , taskWatchStatus
  , taskWatch
  , taskWaitWorking
  , taskSetStatus
  , taskModStatus
  , taskDone
  , taskLookup
  , TaskStore (..)
  , initTaskStore
  , runTasksIO

    -- * Worker
  , startWorker
  , Reader
  , TaskFail (..)

    -- * Reporting
  , ReportTaskStatus (..)
  , ReportedTask (..)
  , runReportTaskNoop
  , runReportTaskAMQP
  , startReportListener
  ) where

import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Tasks.Memory (Task' (..), TaskId (..), TaskStatus (..), TaskStore, TaskStoreError (..), initTaskStore, parseStatus, parseTask, taskId, taskStatus)
import Effectful.Tasks.Memory qualified as TaskStore
import Effectful.Tasks.WorkerTask
import NSO.Prelude
import Network.AMQP (ExchangeOpts (..), QueueOpts (..), newExchange, newQueue)
import Network.AMQP.Types (FieldTable (..), FieldValue (..))
import Network.AMQP.Worker (Key, Message (..), Route)
import Network.AMQP.Worker qualified as AMQP
import Network.AMQP.Worker.Connection as AMQP (Connection (..))
import Network.AMQP.Worker.Key (keyText, toBindKey)
import Network.AMQP.Worker.Queue qualified as AMQP


data Queue t :: Effect where
  QueueInsert :: t -> Queue t m ()
  QueueGetNext :: Queue t m t


type instance DispatchOf (Queue t) = 'Dynamic


runQueueIO
  :: forall t a es
   . (IOE :> es, Concurrent :> es, WorkerTask t, Tasks :> es)
  => QueueChan t
  -> Eff (Queue t : es) a
  -> Eff es a
runQueueIO chan = interpret $ \_ -> \case
  QueueGetNext -> do
    atomically $ readTQueue chan.queue
  QueueInsert t -> do
    atomically $ writeTQueue chan.queue t


runQueueAMQP
  :: forall t a es
   . (IOE :> es, Concurrent :> es, WorkerTask t, Tasks :> es, FromJSON t, ToJSON t)
  => QueueAMQP t
  -> Eff (Queue t : es) a
  -> Eff es a
runQueueAMQP q = interpret $ \_ -> \case
  QueueGetNext -> do
    Message _body t <- AMQP.takeMessage q.connection q.queue
    pure t
  QueueInsert t -> do
    AMQP.publish q.connection q.key t


queueNext :: forall t es. (Queue t :> es, Tasks :> es, WorkerTask t) => Eff es t
queueNext = do
  t <- send QueueGetNext
  send $ TaskSetStatus (taskId t) (taskStatus $ idle @t)
  pure t


queueAdd :: (Queue t :> es, Tasks :> es, WorkerTask t) => t -> Eff es ()
queueAdd t = do
  -- Add a task unless one is active
  act <- taskIsActive t
  unless act $ do
    send $ QueueInsert t
    send $ TaskSave $ newTask t


queueAddAll :: (Queue t :> es, Tasks :> es, WorkerTask t) => [t] -> Eff es ()
queueAddAll =
  mapM_ queueAdd


taskDone :: forall t es. (Tasks :> es, WorkerTask t) => t -> Eff es ()
taskDone t = do
  send $ TaskRemove (taskId t)


taskIsActive :: forall t es. (Tasks :> es, WorkerTask t) => t -> Eff es Bool
taskIsActive t = do
  mts <- send $ TaskLookup (taskId t)
  pure $ maybe False isActive mts


taskLookup :: forall t es. (Tasks :> es, WorkerTask t) => t -> Eff es (Maybe (Task t))
taskLookup t = do
  mts <- send $ TaskLookup (taskId t)
  maybe (pure Nothing) (fmap Just . parseTask) mts


newTask :: forall t. (WorkerTask t) => t -> Task'
newTask t =
  Task' (queue @t) (taskId t) (taskStatus $ idle @t) TaskWaiting Nothing


taskFailed :: forall t es. (WorkerTask t, Tasks :> es, Log :> es) => t -> String -> Eff es ()
taskFailed t e = do
  log Err $ "Task Failed: " <> show t
  send $ TaskSaveError (taskId t) e


data Tasks :: Effect where
  TaskSetStatus :: TaskId -> TaskStatus -> Tasks m ()
  TaskSaveError :: TaskId -> String -> Tasks m ()
  TaskRemove :: TaskId -> Tasks m ()
  TaskSave :: Task' -> Tasks m ()
  TaskLookup :: TaskId -> Tasks m (Maybe Task')
  TaskAll :: Tasks m [Task']
  TaskWatchDelay :: Tasks m ()
type instance DispatchOf Tasks = 'Dynamic


-- To run Tasks, you *might* want to run a reporter. Report to the relevant AMQP queue when you run operations
runTasksIO
  :: forall a es
   . (IOE :> es, Concurrent :> es, ReportTaskStatus :> es)
  => TaskStore
  -> Eff (Tasks : es) a
  -> Eff es a
runTasksIO store = reinterpret (runReader store) $ \_ -> \case
  TaskSetStatus t s -> do
    TaskStore.updateStatus t s
    send $ ReportTaskStatus $ ReportedStatus t s
  TaskSaveError t e -> do
    TaskStore.saveError t e
    send $ ReportTaskStatus $ ReportedError t e
  TaskRemove t -> do
    TaskStore.removeTask t
    send $ ReportTaskStatus $ ReportedRemove t
  TaskSave t -> do
    TaskStore.saveTask t
    send $ ReportTaskStatus $ ReportedSave t
  TaskLookup t ->
    TaskStore.lookupTask t
  TaskAll ->
    TaskStore.allTasks
  TaskWatchDelay -> do
    -- just so users don't need Concurrent
    threadDelay $ 500 * 1000


data ReportTaskStatus :: Effect where
  ReportTaskStatus :: ReportedTask -> ReportTaskStatus m ()
type instance DispatchOf ReportTaskStatus = 'Dynamic


runReportTaskNoop
  :: forall a es
   . (IOE :> es)
  => Eff (ReportTaskStatus : es) a
  -> Eff es a
runReportTaskNoop = interpret $ \_ -> \case
  ReportTaskStatus _ -> pure ()


data ReportedTask
  = ReportedStatus TaskId TaskStatus
  | ReportedError TaskId String
  | ReportedRemove TaskId
  | ReportedSave Task'
  deriving (Generic, Show, ToJSON, FromJSON)


runReportTaskAMQP
  :: forall a es
   . (IOE :> es)
  => QueueAMQP ReportedTask
  -> Eff (ReportTaskStatus : es) a
  -> Eff es a
runReportTaskAMQP q = interpret $ \_ -> \case
  ReportTaskStatus rt ->
    AMQP.publish q.connection q.key rt


isActive :: Task' -> Bool
isActive t =
  t.working == TaskWorking || t.working == TaskWaiting


data QueueChan t = QueueChan
  { queue :: TQueue t
  }


queueChanNew :: STM (QueueChan t)
queueChanNew = do
  q <- newTQueue
  pure $ QueueChan q


initQueueIO :: (Concurrent :> es) => Eff es (QueueChan t)
initQueueIO = atomically queueChanNew


data QueueAMQP t = QueueAMQP
  { connection :: AMQP.Connection
  , queue :: AMQP.Queue t
  , key :: Key Route t
  }


initQueueAMQP :: (IOE :> es) => Key Route t -> AMQP.Connection -> Eff es (QueueAMQP t)
initQueueAMQP k c = do
  let args = FieldTable [("x-queue-type", FVString "quorum")]
  let qopts = newQueue{queueName = taskQueueName, queueHeaders = args}
  let eopts = newExchange{exchangeName = exchange c, exchangeType = "topic"}
  AMQP.bindQueue' c eopts qopts k (FieldTable mempty)
  let q = AMQP.Queue (toBindKey k) qopts.queueName
  pure $ QueueAMQP{connection = c, queue = q, key = k}
 where
  -- we use a format like catalog.messages.m, name the queue the same, but with .q
  taskQueueName = T.replace ".m" ".q" $ keyText k


taskSetStatus :: forall t es. (WorkerTask t, Tasks :> es) => t -> Status t -> Eff es ()
taskSetStatus t s = do
  send $ TaskSetStatus (taskId t) (taskStatus s)


taskModStatus :: forall t es. (WorkerTask t, Tasks :> es) => t -> (Status t -> Status t) -> Eff es ()
taskModStatus t f = do
  mt <- send $ TaskLookup (taskId t)
  case mt of
    Nothing -> throwIO $ MissingTask (taskId t)
    Just tsk -> do
      s <- parseStatus @t (taskId t) tsk.status
      taskSetStatus t (f s)


-- | call onStatus every time status updates, until the task disappears
taskWatchStatus :: forall t es. (Tasks :> es, WorkerTask t) => t -> (Status t -> Eff es ()) -> Eff es ()
taskWatchStatus t onStatus = do
  let tid = taskId t
  _ <- taskWatch tid $ \tsk -> do
    s <- parseStatus @t tid tsk.status
    onStatus s
  pure ()


-- can it return the *last* task
taskWatch :: forall es. (Tasks :> es) => TaskId -> (Task' -> Eff es ()) -> Eff es (Maybe Task')
taskWatch t onStatus = do
  checkNext Nothing
 where
  checkNext :: Maybe Task' -> Eff es (Maybe Task')
  checkNext !mold = do
    mt <- send $ TaskLookup t
    case mt of
      Nothing -> pure Nothing
      Just tsk -> do
        when (Just tsk /= mold) $ do
          onStatus tsk

        -- stop if failed
        if tsk.working == TaskFailed
          then pure $ Just tsk
          else do
            send TaskWatchDelay
            checkNext mt


-- | Wait for a task to start, then continue
taskWaitWorking :: forall t es. (Tasks :> es, WorkerTask t) => t -> Eff es ()
taskWaitWorking t = do
  checkNext
 where
  checkNext :: Eff es ()
  checkNext = do
    ms <- send $ TaskLookup (taskId t)
    if isWorking ms
      then pure ()
      else do
        send TaskWatchDelay
        checkNext

  isWorking :: Maybe Task' -> Bool
  isWorking Nothing = False
  isWorking (Just tsk) = tsk.working == TaskWorking


startWorker :: forall t es. (Concurrent :> es, WorkerTask t, Queue t :> es, Tasks :> es, Log :> es) => (t -> Eff (Error TaskFail : Reader t : es) ()) -> Eff es ()
startWorker work = do
  forever $ do
    t <- send QueueGetNext
    res <- logContext (show t) $ runReader t $ runErrorNoCallStack @TaskFail $ work t
    case res of
      Left (TaskFail e) -> taskFailed t e
      Right _ -> taskDone t -- will not run if TaskFail is called


startReportListener :: (IOE :> es, Log :> es, Tasks :> es) => QueueAMQP ReportedTask -> Eff es ()
startReportListener q = do
  AMQP.worker q.connection q.queue $ \m -> do
    log Debug $ show m.value
    case m.value of
      ReportedStatus t s ->
        send $ TaskSetStatus t s
      ReportedError t e ->
        send $ TaskSaveError t e
      ReportedRemove t ->
        send $ TaskRemove t
      ReportedSave t ->
        send $ TaskSave t


data TaskFail
  = TaskFail String
  deriving (Show, Eq)

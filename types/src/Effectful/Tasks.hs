{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Tasks
  ( -- * Queue
    Queue (..)
  , queueAdd
  , queueAddAll
  , queueNext

    -- ** TChan/IO Task Queues
  , QueueChan
  , runTaskQueueIO
  , runQueueIO
  , initQueueIO
  , queueChanNew

    -- ** AMQP Task Queues
  , QueueAMQP
  , runTaskQueueAMQP
  , runQueueAMQP
  , initQueueAMQP

    -- * Tasks
  , Tasks (..)
  , Task (..)
  , WorkerTask (..)
  , taskWatchStatus
  , taskWaitWorking
  , taskSetStatus
  , taskDone
  , runTasksDB
  , runTasksIO
  , initTaskStore

    -- * Worker
  , startWorker
  , Reader
  , TaskFail (..)
  , DBType
  , DBEq
  , ReadShow (..)
  ) where

import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Rel8
import Effectful.Tasks.Memory (TaskStore, initTaskStore)
import Effectful.Tasks.Memory qualified as TaskStore
import Effectful.Tasks.TasksRel8 (Serial, task)
import Effectful.Tasks.TasksRel8 qualified as TaskDB
import Effectful.Tasks.WorkerTask
import NSO.Prelude
import Network.AMQP.Worker (Key, Message (..), Route)
import Network.AMQP.Worker qualified as AMQP
import Network.AMQP.Worker.Connection as AMQP (Connection (..))
import Network.AMQP.Worker.Key (keyText)
import Rel8 (DBEq, DBType, ReadShow (..))


-- this should be centered around how to get the next task, etc
-- requires an implementation of Tasks to operate
data Queue t :: Effect where
  QueueInsert :: t -> Queue t m ()
  QueueGetNext :: Queue t m t


type instance DispatchOf (Queue t) = 'Dynamic


runTaskQueueIO
  :: forall t a es
   . (IOE :> es, Concurrent :> es, WorkerTask t, Serial t, Serial (Status t), Rel8 :> es)
  => QueueChan t
  -> Eff (Queue t : Tasks t : es) a
  -> Eff es a
runTaskQueueIO chan = runTasksDB . runQueueIO chan


runTaskQueueAMQP
  :: forall t a es
   . (IOE :> es, Concurrent :> es, WorkerTask t, Serial t, Serial (Status t), FromJSON t, ToJSON t, Rel8 :> es)
  => QueueAMQP t
  -> Eff (Queue t : Tasks t : es) a
  -> Eff es a
runTaskQueueAMQP q = runTasksDB . runQueueAMQP q


runQueueIO
  :: forall t a es
   . (IOE :> es, Concurrent :> es, WorkerTask t, Tasks t :> es)
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
   . (IOE :> es, Concurrent :> es, WorkerTask t, Tasks t :> es, FromJSON t, ToJSON t)
  => QueueAMQP t
  -> Eff (Queue t : es) a
  -> Eff es a
runQueueAMQP q = interpret $ \_ -> \case
  QueueGetNext -> do
    Message _body t <- AMQP.takeMessage q.connection q.queue
    pure t
  QueueInsert t -> do
    AMQP.publish q.connection q.key t


queueNext :: forall t es. (Queue t :> es, Tasks t :> es, WorkerTask t) => Eff es t
queueNext = do
  t <- send QueueGetNext
  send $ TaskSetStatus t (idle @t)
  pure t


queueAdd :: (Queue t :> es, Tasks t :> es) => t -> Eff es ()
queueAdd t = do
  -- Add a task unless one is active
  act <- send $ TaskIsActive t
  unless act $ do
    send $ QueueInsert t
    send $ TaskInsert t


queueAddAll :: (Queue t :> es, Tasks t :> es) => [t] -> Eff es ()
queueAddAll =
  mapM_ queueAdd


taskDone :: forall t es. (Tasks t :> es) => t -> Eff es ()
taskDone t = do
  send $ TaskRemove t


data Tasks t :: Effect where
  TaskGetStatus :: t -> Tasks t m (Status t)
  TaskSetStatus :: t -> Status t -> Tasks t m ()
  TaskModStatus :: t -> (Status t -> Status t) -> Tasks t m ()
  TaskSaveError :: t -> String -> Tasks t m ()
  TaskRemove :: t -> Tasks t m ()
  TaskInsert :: t -> Tasks t m ()
  TaskLookup :: t -> Tasks t m (Maybe (Task t))
  TaskIsActive :: t -> Tasks t m Bool
  TaskAll :: Tasks t m [Task t]
  TaskWatchDelay :: t -> Tasks t m ()


type instance DispatchOf (Tasks t) = 'Dynamic


runTasksIO
  :: forall t a es
   . (IOE :> es, Concurrent :> es, Show t, Read t, Show (Status t), Read (Status t), Eq t, WorkerTask t)
  => TaskStore
  -> Eff (Tasks t : es) a
  -> Eff es a
runTasksIO store = reinterpret (runReader store) $ \_ -> \case
  TaskGetStatus t -> do
    mt <- TaskStore.lookupTask t
    pure $ maybe (idle @t) (.status) mt
  TaskSetStatus t s ->
    TaskStore.updateStatus t s
  TaskModStatus t f ->
    TaskStore.modifyStatus t f
  TaskSaveError t e ->
    TaskStore.saveError t e
  TaskRemove t ->
    TaskStore.removeStatus t
  TaskInsert t ->
    TaskStore.insertTask t
  TaskLookup t ->
    TaskStore.lookupTask t
  TaskIsActive t -> do
    mts <- TaskStore.lookupTask t
    pure $ maybe False isActive mts
  TaskAll ->
    TaskStore.allTasks
  TaskWatchDelay _ -> do
    -- just so users don't need Concurrent
    threadDelay $ 500 * 1000


runTasksDB
  :: forall t a es
   . (IOE :> es, Concurrent :> es, Eq t, Serial t, Serial (Status t), WorkerTask t, Rel8 :> es)
  => Eff (Tasks t : es) a
  -> Eff es a
runTasksDB = interpret $ \_ -> \case
  TaskGetStatus t -> do
    mt <- fmap task <$> TaskDB.lookupTask' t
    pure $ case mt of
      Nothing -> idle @t
      Just tsk -> tsk.status
  TaskSetStatus t s -> do
    TaskDB.updateStatus t s
  TaskLookup t -> do
    -- TaskDB.filterTasks (tasksById t)
    fmap task <$> TaskDB.lookupTask' t
  TaskInsert t -> do
    TaskDB.insertTask t
  TaskModStatus t m -> do
    TaskDB.modifyStatus t m
  TaskRemove t -> do
    TaskDB.removeTask t
  TaskAll -> do
    TaskDB.queryTasks
  TaskIsActive t -> do
    ts <- TaskDB.filterTasks (TaskDB.tasksById t)
    pure $ any isActive ts
  TaskSaveError t e -> do
    TaskDB.saveError t e
  TaskWatchDelay _ -> do
    -- just so users don't need Concurrent
    threadDelay $ 500 * 1000


isActive :: Task t -> Bool
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
  q <- AMQP.queueNamed c taskQueueName k
  pure $ QueueAMQP{connection = c, queue = q, key = k}
 where
  taskQueueName = T.replace ".m" ".q" $ keyText k


taskSetStatus :: forall t es. (Tasks t :> es, Reader t :> es) => Status t -> Eff es ()
taskSetStatus s = do
  t <- ask @t
  send $ TaskSetStatus t s


-- | call onStatus every time status updates, until the task disappears
taskWatchStatus :: forall t es. (Eq (Status t), Tasks t :> es) => (Status t -> Eff es ()) -> t -> Eff es ()
taskWatchStatus onStatus t = do
  checkNext Nothing
 where
  checkNext :: Maybe (Status t) -> Eff es ()
  checkNext !mold = do
    mt <- send $ TaskLookup t
    case mt of
      Nothing -> pure ()
      Just tsk -> do
        when (Just tsk.status /= mold) $ do
          onStatus tsk.status

        send $ TaskWatchDelay t
        checkNext $ (.status) <$> mt


-- | Wait for a task to start, then continue
taskWaitWorking :: forall t es. (Tasks t :> es) => t -> Eff es ()
taskWaitWorking t = do
  checkNext
 where
  checkNext :: Eff es ()
  checkNext = do
    ms <- send $ TaskLookup t
    if isWorking ms
      then pure ()
      else do
        send $ TaskWatchDelay t
        checkNext

  isWorking :: Maybe (Task t) -> Bool
  isWorking Nothing = False
  isWorking (Just tsk) = tsk.working == TaskWorking


startWorker :: forall t es. (Show t, Serial t, Serial (Status t), Concurrent :> es, WorkerTask t, Queue t :> es, Tasks t :> es, Log :> es) => (t -> Eff (Error TaskFail : Reader t : es) ()) -> Eff es ()
startWorker work = do
  forever $ do
    t <- send QueueGetNext
    res <- logContext (show t) $ runReader t $ runErrorNoCallStack @TaskFail $ work t
    case res of
      Left (TaskFail e) -> send $ TaskSaveError t e
      Right _ -> taskDone t -- will not run if TaskFail is called


data TaskFail
  = TaskFail String
  deriving (Show, Eq)

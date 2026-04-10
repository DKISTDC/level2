{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Tasks where

-- ( taskChanNew
-- , taskAdd
-- , tasksAdd
-- , TaskChan (..)
-- , taskWatchStatus
-- , taskWaitWorking
-- , Tasks (..)
-- , Queue (..)
-- , runTasks
-- , startWorker
-- , Task (..)
-- , WorkerTask (..)
-- , TaskFail (..)
-- ) where

import Control.Monad (forever)
import Data.Typeable (Typeable)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.Exception
import Effectful.Log
import Effectful.Rel8
import Effectful.Tasks.TasksRel8 as TaskDB
import Effectful.Tasks.WorkerTask
import NSO.Prelude


-- this should be centered around how to get the next task, etc
-- requires an implementation of Tasks to operate
data Queue t :: Effect where
  QueueAdd :: t -> Queue t m ()
  QueueNext :: Queue t m t
  QueueDone :: t -> Queue t m ()


type instance DispatchOf (Queue t) = 'Dynamic


runQueueIO
  :: forall t a es
   . (IOE :> es, Concurrent :> es, WorkerTask t, Tasks t :> es)
  => QueueChan t
  -> Eff (Queue t : es) a
  -> Eff es a
runQueueIO chan = interpret $ \_ -> \case
  QueueNext -> do
    t <- atomically $ readTQueue chan.queue
    send $ TaskSetStatus t (idle @t)
    pure t
  QueueDone t -> do
    send $ TaskRemove t
  QueueAdd t -> do
    -- Add a task unless one is active
    act <- send $ TaskIsActive t
    unless act $ do
      atomically $ writeTQueue chan.queue t
      send $ TaskInsert t


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
    ts <- TaskDB.filterTasks (tasksById t)
    pure $ any isActive ts
  TaskSaveError t e -> do
    TaskDB.saveError t e
  TaskWatchDelay _ -> do
    -- just so users don't need Concurrent
    threadDelay $ 500 * 1000
 where
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


-- taskAdd :: (Tasks t :> es) => t -> Eff es ()
-- taskAdd = send . TaskAdd
--
--
-- tasksAdd :: (Tasks t :> es) => [t] -> Eff es ()
-- tasksAdd =
--   mapM_ (send . TaskAdd)

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


-- -- \| Read status, used in both lookup and reading
-- readStatus :: (Eq t, WorkerTask t) => t -> STM (Status t)
-- readStatus t = do
--   work <- readTVar chan.work
--   pure $ fromMaybe (idle @t) $ lookup t work

-- delete :: (Eq t) => t -> [(t, s)] -> [(t, s)]
-- delete t = filter ((/= t) . fst)
--
-- insert :: (Eq t) => t -> s -> [(t, s)] -> [(t, s)]
-- insert t s tss =
--   (t, s) : delete t tss
--
-- adjust :: (Eq t) => t -> (s -> s) -> [(t, s)] -> [(t, s)]
-- adjust t f = fmap mapKey
--  where
--   mapKey ts =
--     if fst ts == t
--       then second f ts
--       else ts
--
-- notMember :: (Eq t) => t -> [(t, s)] -> Bool
-- notMember t ts = notElem t $ map fst ts

-- TODO: separate worker / tasks runner. Keep runner in context
-- BUG: log context doesn't go away! It exits, maybe catch and rethrow? Maybe I should ditch logContext and active tasks... sad...
-- either that or don't use inside the workers. give it a log context of the task
startWorker :: forall t es. (Serial t, Serial (Status t), Concurrent :> es, WorkerTask t, Queue t :> es, Tasks t :> es, Log :> es) => (t -> Eff (Error TaskFail : es) ()) -> Eff es ()
startWorker work = do
  forever $ do
    t <- send QueueNext
    res <- logContext (show t) $ runErrorNoCallStack @TaskFail $ work t
    case res of
      Left (TaskFail e) -> send $ TaskSaveError t e
      Right _ -> send $ QueueDone t -- will not run if TaskFail is called


data TaskFail
  = TaskFail String
  deriving (Show, Eq)

{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Tasks
  ( taskChanNew
  , taskAdd
  , tasksAdd
  , TaskChan (..)
  , taskWatchStatus
  , taskWaitWorking
  , Tasks (..)
  , runTasks
  , startWorker
  , Task (..)
  , WorkerTask (..)
  , TaskFail (..)
  ) where

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


data TaskChan t = TaskChan
  { queue :: TQueue t
  }


taskChanNew :: STM (TaskChan t)
taskChanNew = do
  q <- newTQueue
  pure $ TaskChan q


taskAdd :: (Tasks t :> es) => t -> Eff es ()
taskAdd = send . TaskAdd


tasksAdd :: (Tasks t :> es) => [t] -> Eff es ()
tasksAdd =
  mapM_ (send . TaskAdd)


-- taskNext :: forall t. (Eq t, WorkerTask t) => TaskChan t -> STM t
-- taskNext chan = _

-- taskDone :: (Eq t) => TaskChan t -> t -> STM ()
-- taskDone chan t = do
--   modifyTVar chan.work $ delete t

-- taskSetStatus :: (Eq t, WorkerTask t) => TaskChan t -> t -> Status t -> STM ()
-- taskSetStatus chan t s = do
--   modifyTVar chan.work $ insert t s

-- taskSave :: (Tasks t :> es, Eq t, WorkerTask t) => t -> Status t -> Eff es ()
-- taskSave t s = do
--   send $ TaskSetStatus t s
--   send $ TaskSave t s

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


-- taskModStatus :: (Eq t, WorkerTask t) => TaskChan t -> t -> (Status t -> Status t) -> STM ()
-- taskModStatus chan t m = do
--   modifyTVar chan.work $ adjust t m

-- taskLookupStatus :: forall t. (Eq t, WorkerTask t) => TaskChan t -> t -> STM (Maybe (Status t))
-- taskLookupStatus chan t = do

-- taskChanWaiting :: TaskChan t -> STM [t]
-- taskChanWaiting chan =
--   readTVar chan.wait
--
--
-- taskChanWorking :: TaskChan t -> STM [(t, Status t)]
-- taskChanWorking chan =
--   readTVar chan.work

-- -- | Waits until status changes, then returns
-- taskWaitStatus :: TaskChan t -> t -> STM (Status t)
-- taskWaitStatus = _

data Tasks t :: Effect where
  TaskGetStatus :: t -> Tasks t m (Status t)
  TaskSetStatus :: t -> Status t -> Tasks t m ()
  TaskModStatus :: t -> (Status t -> Status t) -> Tasks t m ()
  TaskLookup :: t -> Tasks t m (Maybe (Task t))
  TaskWatchDelay :: t -> Tasks t m ()
  TaskAdd :: t -> Tasks t m ()
  TaskNext :: Tasks t m t
  TaskDone :: t -> Tasks t m ()
  TasksAll :: Tasks t m [Task t]


-- TaskSave :: t -> Status t -> Tasks t m ()

type instance DispatchOf (Tasks t) = 'Dynamic


runTasks
  :: forall t a es
   . (IOE :> es, Concurrent :> es, Eq t, Serial t, Serial (Status t), WorkerTask t, Rel8 :> es)
  => TaskChan t
  -> Eff (Tasks t : es) a
  -> Eff es a
runTasks chan = interpret $ \_ -> \case
  TaskGetStatus t -> do
    mt <- fmap task <$> TaskDB.lookupTask' t
    pure $ case mt of
      Nothing -> idle @t
      Just tsk -> tsk.status
  TaskSetStatus t s -> do
    TaskDB.updateStatus t s
  TaskLookup t -> do
    fmap task <$> TaskDB.lookupTask' t
  TaskModStatus t m -> do
    TaskDB.modifyStatus t m
  TaskNext -> do
    t <- atomically $ readTQueue chan.queue
    TaskDB.updateStatus t (idle @t)
    pure t
  TaskDone t -> do
    TaskDB.removeTask t
  TasksAll -> do
    TaskDB.queryTasks
  TaskAdd t -> do
    -- Add a task unless one exists Waiting or Working
    ts <- TaskDB.filterTasks (tasksById t)
    unless (any isActive ts) $ do
      atomically $ writeTQueue chan.queue t
      TaskDB.insertTask t
  TaskWatchDelay _ -> do
    -- just so users don't need Concurrent
    threadDelay $ 500 * 1000
 where
  isActive :: Task t -> Bool
  isActive t =
    t.working == TaskWorking || t.working == TaskWaiting


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
startWorker :: forall t es. (Serial t, Serial (Status t), Concurrent :> es, WorkerTask t, Tasks t :> es, Log :> es, Rel8 :> es) => (t -> Eff (Error TaskFail : es) ()) -> Eff es ()
startWorker work = do
  forever $ do
    t <- send TaskNext
    res <- logContext (show t) $ runErrorNoCallStack @TaskFail $ work t
    case res of
      Left (TaskFail e) -> TaskDB.saveError t e
      Right _ -> send $ TaskDone t -- will not run if TaskFail is called


data TaskFail
  = TaskFail String
  deriving (Show, Eq)

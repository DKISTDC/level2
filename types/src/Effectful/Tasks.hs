{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Tasks where

import Control.Monad (forever)
import Data.Bifunctor (second)
import Data.List qualified as L
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.NonDet
import Effectful.Rel8
import Effectful.Tasks.TasksRel8 as TaskDB
import Effectful.Tasks.WorkerTask
import NSO.Prelude


data TaskChan t = TaskChan
  { queue :: TQueue t
  }


taskChanNew :: STM (TaskChan t)
taskChanNew = do
  queue <- newTQueue
  pure $ TaskChan queue


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
    mts <- send $ TaskLookupStatus t
    let ms = mts >>= status
    case ms of
      Nothing -> pure ()
      Just s -> do
        when (Just s /= mold) $ do
          onStatus s

        send $ TaskWatchDelay t
        checkNext ms

  status :: TaskStatus t -> Maybe (Status t)
  status = \case
    Missing -> Nothing
    Waiting -> Nothing
    Working s -> Just s
    Complete -> Nothing


-- | Wait for a task to start, then continue
taskWaitStatus :: forall t es. (Eq (Status t), Tasks t :> es) => t -> Eff es ()
taskWaitStatus t = do
  checkNext
 where
  checkNext :: Eff es ()
  checkNext = do
    ms <- send $ TaskLookupStatus t
    case ms of
      Just _ -> pure ()
      Nothing -> do
        send $ TaskWatchDelay t
        checkNext


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
  TaskGetStatus :: t -> Tasks t m (TaskStatus t)
  TaskSetStatus :: t -> Status t -> Tasks t m ()
  TaskModStatus :: t -> (Status t -> Status t) -> Tasks t m ()
  TaskLookupStatus :: t -> Tasks t m (Maybe (TaskStatus t))
  TaskWatchDelay :: t -> Tasks t m ()
  TaskAdd :: t -> Tasks t m ()
  -- TasksAdd :: [t] -> Tasks t m ()
  TaskNext :: Tasks t m t
  TaskDone :: t -> Tasks t m ()
  TasksAll :: Tasks t m [Task t]


-- TaskSave :: t -> Status t -> Tasks t m ()

type instance DispatchOf (Tasks t) = 'Dynamic


runTasks
  :: forall t a es
   . (Concurrent :> es, Eq t, Serial t, Serial (Status t), WorkerTask t, Rel8 :> es)
  => TaskChan t
  -> Eff (Tasks t : es) a
  -> Eff es a
runTasks chan = interpret $ \_ -> \case
  TaskGetStatus t -> do
    TaskDB.lookupTaskStatus t
  TaskSetStatus t s -> do
    TaskDB.updateStatus t s
  TaskLookupStatus t -> do
    s <- TaskDB.lookupTaskStatus t
    case s of
      Missing -> pure Nothing
      other -> pure $ Just other
  TaskModStatus t m -> do
    TaskDB.modifyStatus t m
  TaskNext -> do
    t <- atomically $ readTQueue chan.queue
    TaskDB.updateStatus t (idle @t)
    pure t
  TaskDone t -> do
    TaskDB.markComplete t
  TasksAll -> do
    TaskDB.queryTasks
  TaskAdd t -> do
    ts <- TaskDB.queryTasks
    -- Idempotent. A task that exactly matches an existing one will not be added twice
    -- TODO: what about regenerating things? It'll have the same task if we don't activl
    -- WARNING: currently TaskDB doesn't clean up, it won't re-queue them, because they already exist
    unless (t `elem` fmap (.task) ts) $ do
      atomically $ writeTQueue chan.queue t
      TaskDB.insertTask t
  -- TaskSave t s -> atomically $ do
  --   modifyTVar chan.saved $ insert t s
  -- -- just so users don't need Concurrent
  TaskWatchDelay _ -> do
    threadDelay $ 500 * 1000
 where
  idleStatus :: [t] -> t -> Maybe (Status t)
  idleStatus wait t
    | t `elem` wait = Just $ idle @t
    | otherwise = Nothing


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

startWorker :: forall t es. (Concurrent :> es, WorkerTask t, Tasks t :> es) => (t -> Eff es ()) -> Eff es ()
startWorker work = do
  forever $ do
    task <- send TaskNext
    work task
    send $ TaskDone task

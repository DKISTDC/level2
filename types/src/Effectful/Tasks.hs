{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Tasks where

import Control.Monad (forever)
import Data.Bifunctor (second)
import Data.List qualified as L
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.NonDet
import NSO.Prelude


class WorkerTask t where
  type Status t :: Type
  idle :: Status t


data TaskChan t = TaskChan
  { wait :: TVar [t]
  , work :: TVar [(t, Status t)]
  , saved :: TVar [(t, Status t)]
  , queue :: TQueue t
  }


taskChanNew :: STM (TaskChan t)
taskChanNew = do
  wait <- newTVar mempty
  work <- newTVar mempty
  saved <- newTVar mempty
  queue <- newTQueue
  pure $ TaskChan{wait, work, queue, saved}


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

taskSave :: (Tasks t :> es, Eq t, WorkerTask t) => t -> Status t -> Eff es ()
taskSave t s = do
  send $ TaskSetStatus t s
  send $ TaskSave t s


taskWatchStatus :: forall t es. (Eq (Status t), Tasks t :> es) => (Status t -> Eff es ()) -> t -> Eff es ()
taskWatchStatus onStatus t = do
  checkNext Nothing
 where
  checkNext :: Maybe (Status t) -> Eff es ()
  checkNext mold = do
    ms <- send $ TaskLookupStatus t
    case ms of
      Nothing -> pure ()
      Just s -> do
        when (ms /= mold) $ do
          onStatus s
        send $ TaskWatchDelay t
        checkNext ms


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
  TaskLookupStatus :: t -> Tasks t m (Maybe (Status t))
  TaskWatchDelay :: t -> Tasks t m ()
  TaskAdd :: t -> Tasks t m ()
  -- TasksAdd :: [t] -> Tasks t m ()
  TaskNext :: Tasks t m t
  TaskDone :: t -> Tasks t m ()
  TasksWaiting :: Tasks t m [t]
  TasksWorking :: Tasks t m [(t, Status t)]
  TaskSave :: t -> Status t -> Tasks t m ()


type instance DispatchOf (Tasks t) = 'Dynamic


runTasks
  :: forall t a es
   . (Concurrent :> es, Eq t, WorkerTask t)
  => TaskChan t
  -> Eff (Tasks t : es) a
  -> Eff es a
runTasks chan = interpret $ \_ -> \case
  TaskGetStatus t -> do
    atomically $ readStatus t
  TaskSetStatus t s -> do
    atomically $ modifyTVar chan.work $ insert t s
  TaskLookupStatus t -> do
    atomically $ do
      wait <- readTVar chan.wait
      work <- readTVar chan.work
      pure $ L.lookup t work <|> idleStatus wait t
  TaskModStatus t m -> atomically $ do
    modifyTVar chan.work $ adjust t m
  TaskNext -> atomically $ do
    t <- readTQueue chan.queue
    modifyTVar chan.wait $ filter (/= t)
    modifyTVar chan.work $ insert t (idle @t)
    pure t
  TaskDone t -> atomically $ do
    modifyTVar chan.work $ delete t
  TasksWaiting -> do
    readTVarIO chan.wait
  TasksWorking -> do
    readTVarIO chan.work

  -- Idempotent. A task that exactly matches an existing one will not be added twice
  TaskAdd t -> atomically $ do
    wait <- readTVar chan.wait
    work <- readTVar chan.work
    when (t `notElem` wait && t `notMember` work) $ do
      writeTVar chan.wait $ t : wait
      writeTQueue chan.queue t
  TaskSave t s -> atomically $ do
    modifyTVar chan.saved $ insert t s
  -- just so users don't need Concurrent
  TaskWatchDelay _ -> do
    threadDelay $ 500 * 1000
 where
  idleStatus :: [t] -> t -> Maybe (Status t)
  idleStatus wait t
    | t `elem` wait = Just $ idle @t
    | otherwise = Nothing

  -- \| Read status, used in both lookup and reading
  readStatus :: (Eq t, WorkerTask t) => t -> STM (Status t)
  readStatus t = do
    work <- readTVar chan.work
    pure $ fromMaybe (idle @t) $ lookup t work

  delete :: (Eq t) => t -> [(t, s)] -> [(t, s)]
  delete t = filter ((/= t) . fst)

  insert :: (Eq t) => t -> s -> [(t, s)] -> [(t, s)]
  insert t s tss =
    (t, s) : delete t tss

  adjust :: (Eq t) => t -> (s -> s) -> [(t, s)] -> [(t, s)]
  adjust t f = fmap mapKey
   where
    mapKey ts =
      if fst ts == t
        then second f ts
        else ts

  notMember :: (Eq t) => t -> [(t, s)] -> Bool
  notMember t ts = notElem t $ map fst ts


startWorker :: forall t es. (Concurrent :> es, WorkerTask t, Tasks t :> es) => (t -> Eff es ()) -> Eff es ()
startWorker work = do
  forever $ do
    task <- send TaskNext
    work task
    send $ TaskDone task

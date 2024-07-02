{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Tasks where

import Control.Monad (forever)
import Data.Bifunctor (second)
import Data.Kind (Type)
import Data.List qualified as L
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import NSO.Prelude


class WorkerTask t where
  type Status t :: Type
  idle :: Status t


data TaskChan t = TaskChan
  { wait :: TVar [t]
  , work :: TVar [(t, Status t)]
  , queue :: TQueue t
  }


taskChanNew :: STM (TaskChan t)
taskChanNew = do
  wait <- newTVar mempty
  work <- newTVar mempty
  queue <- newTQueue
  pure $ TaskChan{wait, work, queue}


taskAdd :: (Eq t) => TaskChan t -> t -> STM ()
taskAdd chan t = do
  wait <- readTVar chan.wait
  work <- readTVar chan.work
  when (t `notElem` wait && t `notMember` work) $ do
    writeTVar chan.wait $ t : wait
    writeTQueue chan.queue t


taskNext :: forall t. (Eq t, WorkerTask t) => TaskChan t -> STM t
taskNext chan = do
  t <- readTQueue chan.queue
  modifyTVar chan.wait $ filter (/= t)
  modifyTVar chan.work $ insert t (idle @t)
  pure t


taskDone :: (Eq t) => TaskChan t -> t -> STM ()
taskDone chan t = do
  modifyTVar chan.work $ delete t


taskSetStatus :: (Eq t, WorkerTask t) => TaskChan t -> t -> Status t -> STM ()
taskSetStatus chan t s = do
  modifyTVar chan.work $ insert t s


taskModStatus :: (Eq t, WorkerTask t) => TaskChan t -> t -> (Status t -> Status t) -> STM ()
taskModStatus chan t m = do
  modifyTVar chan.work $ adjust t m


taskStatus :: forall t. (Eq t, WorkerTask t) => TaskChan t -> t -> STM (Status t)
taskStatus chan t = do
  work <- readTVar chan.work
  pure $ fromMaybe (idle @t) $ lookup t work


taskChanWaiting :: TaskChan t -> STM [t]
taskChanWaiting chan =
  readTVar chan.wait


taskChanWorking :: TaskChan t -> STM [(t, Status t)]
taskChanWorking chan =
  readTVar chan.work


data Tasks t :: Effect where
  TaskGetStatus :: t -> Tasks t m (Status t)
  TaskSetStatus :: t -> Status t -> Tasks t m ()
  TaskModStatus :: t -> (Status t -> Status t) -> Tasks t m ()
  TaskAdd :: t -> Tasks t m ()
  TasksAdd :: [t] -> Tasks t m ()
  TaskNext :: Tasks t m t
  TaskDone :: t -> Tasks t m ()
  TasksWaiting :: Tasks t m [t]
  TasksWorking :: Tasks t m [(t, Status t)]
type instance DispatchOf (Tasks t) = 'Dynamic


runTasks
  :: forall t a es
   . (Concurrent :> es, Eq t, WorkerTask t)
  => TaskChan t
  -> Eff (Tasks t : es) a
  -> Eff es a
runTasks chan = interpret $ \_ -> \case
  TaskGetStatus t -> do
    atomically $ taskStatus chan t
  TaskSetStatus t s -> do
    atomically $ taskSetStatus chan t s
  TaskModStatus t m -> do
    atomically $ taskModStatus chan t m
  TaskNext -> do
    atomically $ taskNext chan
  TaskDone t -> do
    atomically $ taskDone chan t
  TasksWaiting -> do
    atomically $ taskChanWaiting chan
  TasksWorking -> do
    atomically $ taskChanWorking chan
  TaskAdd t -> do
    atomically $ taskAdd chan t
  TasksAdd ts -> do
    atomically $ mapM_ (taskAdd chan) ts


startWorker :: forall t es. (Concurrent :> es, WorkerTask t, Tasks t :> es) => (t -> Eff es ()) -> Eff es ()
startWorker work = do
  forever $ do
    task <- send TaskNext
    work task
    -- manually call task done?
    send $ TaskDone task


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

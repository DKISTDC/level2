{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Worker where

import Control.Monad (forever)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import NSO.Prelude


class WorkerTask t where
  type Status t :: Type
  idle :: Status t


data TaskChan t = TaskChan
  { wait :: TVar (Set t)
  , work :: TVar (Map t (Status t))
  , queue :: TQueue t
  }


taskChanNew :: (Ord t) => STM (TaskChan t)
taskChanNew = do
  wait <- newTVar mempty
  work <- newTVar mempty
  queue <- newTQueue
  pure $ TaskChan{wait, work, queue}


taskAdd :: (Ord t) => TaskChan t -> t -> STM ()
taskAdd chan t = do
  wait <- readTVar chan.wait
  work <- readTVar chan.work
  when (Set.notMember t wait && Map.notMember t work) $ do
    writeTVar chan.wait $ Set.insert t wait
    writeTQueue chan.queue t


taskNext :: forall t. (Ord t, WorkerTask t) => TaskChan t -> STM t
taskNext chan = do
  t <- readTQueue chan.queue
  modifyTVar chan.wait $ Set.delete t
  modifyTVar chan.work $ Map.insert t (idle @t)
  pure t


taskDone :: (Ord t) => TaskChan t -> t -> STM ()
taskDone chan t = do
  modifyTVar chan.work $ Map.delete t


taskSetStatus :: (Ord t, WorkerTask t) => TaskChan t -> t -> Status t -> STM ()
taskSetStatus chan t s = do
  modifyTVar chan.work $ Map.insert t s


taskStatus :: forall t. (Ord t, WorkerTask t) => TaskChan t -> t -> STM (Status t)
taskStatus chan t = do
  work <- readTVar chan.work
  pure $ fromMaybe (idle @t) $ Map.lookup t work


taskChanWaiting :: TaskChan t -> STM (Set t)
taskChanWaiting chan =
  readTVar chan.wait


taskChanWorking :: TaskChan t -> STM (Map t (Status t))
taskChanWorking chan =
  readTVar chan.work


data Worker t :: Effect where
  TaskGetStatus :: t -> Worker t m (Status t)
  TaskSetStatus :: t -> Status t -> Worker t m ()
  TaskNext :: Worker t m t
  TaskDone :: t -> Worker t m ()
  TasksWaiting :: Worker t m [t]
  TasksWorking :: Worker t m [(t, Status t)]
type instance DispatchOf (Worker t) = 'Dynamic


runWorker
  :: (Concurrent :> es, Ord t, WorkerTask t)
  => TaskChan t
  -> Eff (Worker t : es) a
  -> Eff es a
runWorker chan = interpret $ \_ -> \case
  TaskGetStatus t -> do
    atomically $ taskStatus chan t
  TaskSetStatus t s -> do
    atomically $ taskSetStatus chan t s
  TaskNext -> do
    atomically $ taskNext chan
  TaskDone t -> do
    atomically $ taskDone chan t
  TasksWaiting -> do
    s <- atomically $ taskChanWaiting chan
    pure $ Set.toList s
  TasksWorking -> do
    s <- atomically $ taskChanWorking chan
    pure $ Map.toList s


startWorker :: (Concurrent :> es, Ord t, WorkerTask t) => TaskChan t -> (t -> Eff (Worker t : es) ()) -> Eff es ()
startWorker chan work = runWorker chan $ do
  forever $ do
    task <- send TaskNext
    work task
    send $ TaskDone task

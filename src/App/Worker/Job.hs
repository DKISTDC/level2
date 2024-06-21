module App.Worker.Job where

import App.Globus as Globus
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful.Concurrent.STM
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Prelude


data TaskChan t = TaskChan
  { wait :: TVar (Set t)
  , work :: TVar (Set t)
  , queue :: TQueue t
  }


data TaskChanStatus t = TaskChanStatus
  { wait :: Set t
  , work :: Set t
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
  when (Set.notMember t wait && Set.notMember t work) $ do
    writeTVar chan.wait $ Set.insert t wait
    writeTQueue chan.queue t


taskNext :: (Ord t) => TaskChan t -> STM t
taskNext chan = do
  t <- readTQueue chan.queue
  modifyTVar chan.wait $ Set.delete t
  modifyTVar chan.work $ Set.insert t
  pure t


taskDone :: (Ord t) => TaskChan t -> t -> STM ()
taskDone chan t = do
  modifyTVar chan.work $ Set.delete t


taskChanWaiting :: TaskChan t -> STM (Set t)
taskChanWaiting chan =
  readTVar chan.wait


taskChanWorking :: TaskChan t -> STM (Set t)
taskChanWorking chan =
  readTVar chan.work


taskChanStatus :: TaskChan t -> STM (TaskChanStatus t)
taskChanStatus chan = do
  wait <- taskChanWaiting chan
  work <- taskChanWorking chan
  pure $ TaskChanStatus{wait, work}


waitForGlobusAccess :: (Concurrent :> es, Log :> es) => TMVar (Token Access) -> Eff (Reader (Token Access) : es) () -> Eff es ()
waitForGlobusAccess advar work = do
  logDebug "Waiting for Admin Globus Access Token"
  acc <- atomically $ readTMVar advar
  Globus.runWithAccess acc work

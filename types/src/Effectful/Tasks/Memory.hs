{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Tasks.Memory where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as M
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Exception
import Effectful.Reader.Dynamic
import Effectful.Tasks.WorkerTask
import NSO.Prelude
import Text.Read (readMaybe)
import Web.Hyperbole.Data.Param


newtype TaskId = TaskId {value :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, FromParam, ToParam)


newtype TaskStatus = TaskStatus {value :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)


data Task' = Task'
  { queue :: TaskQueue
  , task :: TaskId
  , status :: TaskStatus
  , working :: TaskWorking
  , error :: Maybe String
  }
  deriving (Generic, Eq, ToJSON, FromJSON, Show)
instance Ord Task' where
  t1 <= t2 =
    (t1.working, t1.task) <= (t2.working, t2.task)


newtype TaskStore = TaskStore (TVar (Map TaskId Task'))


initTaskStore :: (Concurrent :> es) => Eff es TaskStore
initTaskStore = TaskStore <$> newTVarIO mempty


taskId :: (Show t) => t -> TaskId
taskId t = TaskId $ cs $ show t


taskStatus :: (Show s) => s -> TaskStatus
taskStatus t = TaskStatus $ cs $ show t


saveTask :: forall es. (Concurrent :> es, Reader TaskStore :> es) => Task' -> Eff es ()
saveTask t = do
  TaskStore var <- ask
  atomically $ modifyTVar var (M.insert t.task t)


updateStatus :: forall es. (Concurrent :> es, Reader TaskStore :> es) => TaskId -> TaskStatus -> Eff es ()
updateStatus t s = do
  TaskStore var <- ask
  atomically $ do
    modifyTVar var (M.update (Just . setStatus) t)
 where
  setStatus :: Task' -> Task'
  setStatus ts =
    Task'{queue = ts.queue, task = ts.task, status = s, working = TaskWorking, error = Nothing}


saveError :: forall es. (Concurrent :> es, Reader TaskStore :> es) => TaskId -> String -> Eff es ()
saveError t e = do
  TaskStore var <- ask
  atomically $ modifyTVar var (M.update (\ts -> Just ts{error = Just e, working = TaskFailed}) t)


removeTask :: forall es. (Concurrent :> es, Reader TaskStore :> es) => TaskId -> Eff es ()
removeTask t = do
  TaskStore var <- ask
  atomically $ modifyTVar var (M.delete t)


lookupTask :: forall es. (Concurrent :> es, Reader TaskStore :> es) => TaskId -> Eff es (Maybe Task')
lookupTask t = do
  TaskStore var <- ask
  atomically $ do
    m <- readTVar var
    pure $ M.lookup t m


allTasks :: forall es. (Concurrent :> es, Reader TaskStore :> es) => Eff es [Task']
allTasks = do
  TaskStore var <- ask
  M.elems <$> readTVarIO var


-- Parsing --------------------------------------

parseStatus :: (Read (Status t)) => TaskId -> TaskStatus -> Eff es (Status t)
parseStatus tid ss = do
  case readMaybe (cs ss.value) of
    Nothing -> throwIO $ BadStatus tid ss
    Just a -> pure a


parseTaskId :: (Read t) => TaskId -> Eff es t
parseTaskId tid = do
  case readMaybe (cs tid.value) of
    Nothing -> throwIO $ BadTaskId tid
    Just a -> pure a


parseTask :: forall t es. (Read t, Read (Status t)) => Task' -> Eff es (Task t)
parseTask ts = do
  parseTask' ts.task ts.status ts.working


parseTask' :: forall t es. (Read t, Read (Status t)) => TaskId -> TaskStatus -> TaskWorking -> Eff es (Task t)
parseTask' tid ss w = do
  t <- parseTaskId @t tid
  s <- parseStatus @t tid ss
  pure $ Task t s w


data TaskStoreError
  = BadStatus TaskId TaskStatus
  | BadTaskId TaskId
  | MissingTask TaskId
  deriving (Eq, Show, Exception)

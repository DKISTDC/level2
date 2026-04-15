{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Tasks.Memory where

import Data.Map.Strict qualified as M
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Exception
import Effectful.Reader.Dynamic
import Effectful.Tasks.WorkerTask
import NSO.Prelude
import Text.Read (readMaybe)


newtype TaskId = TaskId {value :: Text}
  deriving newtype (Eq, Ord, Show)


data TaskStatus = TaskStatus
  { queue :: TaskQueue
  , task :: TaskId
  , status :: Text
  , working :: TaskWorking
  , error :: Maybe String
  }


data TaskStatusError
  = Todo
  deriving (Show, Eq, Exception)


newtype TaskStore = TaskStore (TVar (Map TaskId TaskStatus))


initTaskStore :: (Concurrent :> es) => Eff es TaskStore
initTaskStore = TaskStore <$> newTVarIO mempty


taskId :: (Show t) => t -> TaskId
taskId t = TaskId $ cs $ show t


saveStatus :: forall es. (Concurrent :> es, Reader TaskStore :> es) => TaskStatus -> Eff es ()
saveStatus ts = do
  TaskStore var <- ask
  atomically $ modifyTVar var (M.insert ts.task ts)


insertTask :: forall t es. (Show t, Show (Status t), WorkerTask t, Concurrent :> es, Reader TaskStore :> es) => t -> Eff es ()
insertTask t = do
  let ts = TaskStatus (queue @t) (taskId t) (cs $ show $ idle @t) TaskWaiting Nothing
  saveStatus ts


updateStatus :: forall t es. (Show t, Show (Status t), WorkerTask t, Concurrent :> es, Reader TaskStore :> es) => t -> Status t -> Eff es ()
updateStatus t s = do
  let ts = TaskStatus (queue @t) (taskId t) (cs $ show s) TaskWorking Nothing
  saveStatus ts


saveError :: forall t es. (Show t, Concurrent :> es, Reader TaskStore :> es) => t -> String -> Eff es ()
saveError t e = do
  TaskStore var <- ask
  let tid :: TaskId = taskId t
  atomically $ modifyTVar var (M.update (\ts -> Just ts{error = Just e}) tid)


modifyStatus :: forall t es. (Show t, Read t, Read (Status t), Show (Status t), Concurrent :> es, Reader TaskStore :> es) => t -> (Status t -> Status t) -> Eff es ()
modifyStatus t f = do
  TaskStore var <- ask
  let tid = taskId t
  mtsk <- lookupTask t
  tsk :: Task t <- maybe (throwIO $ MissingStatus tid) pure mtsk

  atomically $ do
    modifyTVar var (M.update (Just . setStatus (cs . show . f $ tsk.status)) tid)
 where
  setStatus :: Text -> TaskStatus -> TaskStatus
  setStatus s ts =
    TaskStatus{queue = ts.queue, task = ts.task, status = s, working = ts.working, error = ts.error}


removeStatus :: forall t es. (Show t, Concurrent :> es, Reader TaskStore :> es) => t -> Eff es ()
removeStatus t = do
  TaskStore var <- ask
  atomically $ modifyTVar var (M.delete (taskId t))


lookupStatus :: forall es. (Concurrent :> es, Reader TaskStore :> es) => TaskId -> Eff es (Maybe TaskStatus)
lookupStatus t = do
  TaskStore var <- ask
  atomically $ do
    m <- readTVar var
    pure $ M.lookup t m


allTasks :: forall t es. (Read t, Read (Status t), Concurrent :> es, Reader TaskStore :> es) => Eff es [Task t]
allTasks = do
  TaskStore var <- ask
  tss <- M.elems <$> readTVarIO var
  mapM parseTask tss


lookupTask :: forall t es. (Show t, Read t, Read (Status t), Concurrent :> es, Reader TaskStore :> es) => t -> Eff es (Maybe (Task t))
lookupTask t = do
  let tid = taskId t
  ms <- lookupStatus tid
  maybe (pure Nothing) (fmap Just . parseTask) ms


-- Parsing --------------------------------------

parseStatus :: (Read (Status t)) => TaskId -> Text -> Eff es (Status t)
parseStatus tid ss = do
  case readMaybe (cs ss) of
    Nothing -> throwIO $ BadStatus tid ss
    Just a -> pure a


parseTaskId :: (Read t) => TaskId -> Eff es t
parseTaskId tid = do
  case readMaybe (cs tid.value) of
    Nothing -> throwIO $ BadTaskId tid
    Just a -> pure a


parseTask :: forall t es. (Read t, Read (Status t)) => TaskStatus -> Eff es (Task t)
parseTask ts = do
  parseTask' ts.task ts.status ts.working


parseTask' :: forall t es. (Read t, Read (Status t)) => TaskId -> Text -> TaskWorking -> Eff es (Task t)
parseTask' tid ss w = do
  t <- parseTaskId @t tid
  s <- parseStatus @t tid ss
  pure $ Task t s w


data TaskStoreError
  = BadStatus TaskId Text
  | BadTaskId TaskId
  | MissingStatus TaskId
  deriving (Eq, Show, Exception)

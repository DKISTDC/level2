{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Effectful.Tasks.TasksRel8 where

import Data.Typeable (Typeable)
import Effectful
import Effectful.Rel8 as Rel8
import Effectful.Tasks.WorkerTask
import NSO.Prelude
import Rel8 (Column, DBEq, DBType (..), ReadShow (..))


data Task' t f = Task'
  { taskQueue :: Column f TaskQueue
  , taskWorking :: Column f TaskWorking
  , taskId :: Column f t
  , taskStatus :: Column f (Status t)
  , taskError :: Column f (Maybe Text)
  }
  deriving (Generic)


newtype Serialized t = Serialized {unSerialzed :: t}
  deriving (DBType) via ReadShow t
  deriving anyclass (DBEq)


type Serial t = (DBType t, Eq t, DBEq t)
instance (Serial t, Serial (Status t)) => Rel8able (Task' t)


insertTask :: forall t es. (WorkerTask t, Rel8able (Task' t), Rel8 :> es) => t -> Eff es ()
insertTask t =
  run_ $
    insert $
      Insert
        { into = table
        , rows = values [lit $ Task' @t (queue @t) TaskWaiting t (idle @t) Nothing]
        , onConflict = DoNothing
        , returning = NoReturning
        }


updateStatus :: forall t es. (Serial t, Serial (Status t), WorkerTask t, Rel8able (Task' t), Rel8 :> es) => t -> Status t -> Eff es ()
updateStatus t s = do
  run_ $
    update $
      Update
        { target = table
        , updateWhere = \_ row -> isTask t row
        , set = \_ row -> setStatus s row
        , from = each (table @t)
        , returning = NoReturning
        }


saveError :: forall t es. (Serial t, WorkerTask t, Rel8able (Task' t), Rel8 :> es) => t -> String -> Eff es ()
saveError t s =
  run_ $
    update $
      Update
        { target = table
        , updateWhere = \_ row -> isTask t row
        , set = \_ row -> setError s row
        , from = each (table @t)
        , returning = NoReturning
        }


modifyStatus :: forall t es. (Serial t, Serial (Status t), WorkerTask t, Rel8able (Task' t), Rel8 :> es) => t -> (Status t -> Status t) -> Eff es ()
modifyStatus t f = do
  mt <- lookupTask' t
  case mt of
    -- ignore if not found!
    Nothing -> pure ()
    Just tsk -> do
      run_ $
        update $
          Update
            { target = table
            , updateWhere = \_ row -> isTask t row
            , set = \_ row -> setStatus (f tsk.taskStatus) row
            , from = each (table @t)
            , returning = NoReturning
            }


removeTask :: forall t es. (Serial t, Rel8able (Task' t), Rel8 :> es) => t -> Eff es ()
removeTask t = do
  run_ $
    delete $
      Delete
        { from = table @t
        , deleteWhere = \_ row -> isTask t row -- &&. not_ (isActive row)
        , using = pure ()
        , returning = NoReturning
        }


queryTasks :: forall t es. (WorkerTask t, Rel8able (Task' t), Rel8 :> es) => Eff es [Task t]
queryTasks = filterTasks (const $ lit True)


filterTasks :: forall t es. (WorkerTask t, Rel8able (Task' t), Rel8 :> es) => (Task' t Expr -> Expr Bool) -> Eff es [Task t]
filterTasks f = do
  let q = queue @t
  ts <- run $ select $ do
    row :: Task' t Expr <- each table
    where_ (f row &&. row.taskQueue ==. lit q)
    return row
  pure $ fmap task ts


genericTaskList :: forall es. (Rel8 :> es) => Eff es [Task' Text Identity]
genericTaskList = do
  ts <- run $ select $ do
    each table
  pure ts


-- lookupTaskStatus :: forall t es. (Serial t, Rel8able (Task' t), Rel8 :> es) => t -> Eff es (TaskStatus t)
-- lookupTaskStatus t = do
--   ms <- lookupTask' t
--   maybe (pure $ idle @t) pure ms

lookupTask' :: forall t es. (Serial t, Rel8able (Task' t), Rel8 :> es) => t -> Eff es (Maybe (Task' t Identity))
lookupTask' t = do
  ts <- run $ select $ do
    row <- each table
    where_ (isTask t row)
    return row
  pure $ listToMaybe ts


task :: forall t. Task' t Identity -> Task t
task t =
  Task t.taskId t.taskStatus t.taskWorking


-- taskStatus :: forall t. Maybe (Task' t Identity) -> TaskStatus t
-- taskStatus = \case
--   Nothing -> Missing
--   Just t ->
--     case t.taskWorking of
--       TaskWaiting -> Waiting
--       TaskComplete -> Complete
--       TaskWorking -> Working @t t.taskStatus.unSerialzed

table :: TableSchema (Task' t Name)
table =
  -- use an auto-incrementing primary key
  TableSchema
    { name = "tasks"
    , columns =
        Task'
          { taskQueue = "queue"
          , taskId = "task_id"
          , taskStatus = "status"
          , taskWorking = "working"
          , taskError = "error"
          }
    }


-- Query Helpers --------------

setError :: (Serial t) => String -> Task' t Expr -> Task' t Expr
setError e row =
  Task'
    { taskQueue = row.taskQueue
    , taskWorking = lit TaskFailed
    , taskId = row.taskId
    , taskStatus = row.taskStatus
    , taskError = lit $ Just (cs e)
    }


setStatus :: (Serial t, Serial (Status t)) => Status t -> Task' t Expr -> Task' t Expr
setStatus s' row =
  Task'
    { taskQueue = row.taskQueue
    , taskWorking = lit TaskWorking
    , taskId = row.taskId
    , taskStatus = lit s'
    , taskError = lit Nothing
    }


setWorking :: (Serial t) => TaskWorking -> Task' t Expr -> Task' t Expr
setWorking w row =
  Task'
    { taskQueue = row.taskQueue
    , taskWorking = lit w
    , taskId = row.taskId
    , taskStatus = row.taskStatus
    , taskError = lit Nothing
    }


isTask :: (Serial t) => t -> Task' t Expr -> Expr Bool
isTask t row = do
  row.taskId ==. lit t


-- taskNotComplete :: Task' t Expr -> Expr Bool
-- taskNotComplete row = not_ (row.taskWorking ==. lit TaskComplete)

taskAll :: Task' t Expr -> Expr Bool
taskAll = const $ lit True


tasksById :: (Serial t) => t -> Task' t Expr -> Expr Bool
tasksById t row = row.taskId ==. lit t

-- isWorking :: (Serial t) => Task' t Expr -> Expr Bool
-- isWorking row = row.taskWorking ==. lit TaskWorking
--
--
-- isWaiting :: (Serial t) => Task' t Expr -> Expr Bool
-- isWaiting row = row.taskWorking ==. lit TaskWaiting

-- isActive :: (Serial t) => Task' t Expr -> Expr Bool
-- isActive row = isWorking row ||. isWaiting row

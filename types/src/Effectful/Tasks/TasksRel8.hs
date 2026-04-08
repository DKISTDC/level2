{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Effectful.Tasks.TasksRel8 where

import Data.Typeable (Typeable)
import Effectful
import Effectful.Rel8 as Rel8
import Effectful.Tasks.WorkerTask
import NSO.Prelude
import Rel8 (Column, DBEq, DBType (..), ReadShow (..))


data Task t = Task
  { task :: t
  , status :: TaskStatus t
  }


data Task' t f = Task'
  { taskType :: Column f Text
  , taskWorking :: Column f TaskWorking
  , taskData :: Column f (Serialized t)
  , taskStatus :: Column f (Serialized (Status t))
  }
  deriving (Generic)


newtype Serialized t = Serialized {unSerialzed :: t}
  deriving (DBType) via ReadShow t
  deriving anyclass (DBEq)


type Serial t = (Typeable t, Read t, Show t, Eq t)
instance (Serial t, Serial (Status t)) => Rel8able (Task' t)


insertTask :: forall t es. (WorkerTask t, Rel8able (Task' t), Rel8 :> es) => t -> Eff es ()
insertTask t =
  run_ $
    insert $
      Insert
        { into = table
        , rows = values [lit $ Task' @t (taskType @t) TaskWaiting (Serialized t) (Serialized $ idle @t)]
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
            , set = \_ row -> setStatus (f tsk.taskStatus.unSerialzed) row
            , from = each (table @t)
            , returning = NoReturning
            }


markComplete :: forall t es. (Serial t, Serial (Status t), WorkerTask t, Rel8able (Task' t), Rel8 :> es) => t -> Eff es ()
markComplete t = do
  run_ $
    update $
      Update
        { target = table
        , updateWhere = \_ row -> isTask t row
        , set = \_ row -> setWorking TaskComplete row
        , from = each (table @t)
        , returning = NoReturning
        }


removeTask :: forall t es. (Serial t, Rel8able (Task' t), Rel8 :> es) => t -> Eff es ()
removeTask t = do
  run_ $
    delete $
      Delete
        { from = table @t
        , deleteWhere = \_ row -> isTask t row
        , using = pure ()
        , returning = NoReturning
        }


queryTasks :: forall t es. (WorkerTask t, Rel8able (Task' t), Rel8 :> es) => Eff es [Task t]
queryTasks = do
  let typ = taskType @t
  ts <- run $ select $ do
    row <- each table
    where_ (row.taskType ==. lit typ)
    return row
  pure $ fmap task ts


lookupTaskStatus :: forall t es. (Serial t, Rel8able (Task' t), Rel8 :> es) => t -> Eff es (TaskStatus t)
lookupTaskStatus t = do
  taskStatus <$> lookupTask' t


lookupTask' :: forall t es. (Serial t, Rel8able (Task' t), Rel8 :> es) => t -> Eff es (Maybe (Task' t Identity))
lookupTask' t = do
  ts <- run $ select $ do
    row <- each table
    where_ (isTask t row)
    return row
  pure $ listToMaybe ts


task :: forall t. Task' t Identity -> Task t
task t =
  Task t.taskData.unSerialzed (taskStatus $ Just t)


taskStatus :: forall t. Maybe (Task' t Identity) -> TaskStatus t
taskStatus = \case
  Nothing -> Missing
  Just t ->
    case t.taskWorking of
      TaskWaiting -> Waiting
      TaskComplete -> Complete
      TaskWorking -> Working @t t.taskStatus.unSerialzed


table :: TableSchema (Task' t Name)
table =
  -- use an auto-incrementing primary key
  TableSchema
    { name = "tasks"
    , columns =
        Task'
          { taskType = "task_type"
          , taskData = "task_data"
          , taskStatus = "task_status"
          , taskWorking = "task_working"
          }
    }


setStatus :: (Serial t, Serial (Status t)) => Status t -> Task' t Expr -> Task' t Expr
setStatus s' row =
  Task'
    { taskType = row.taskType
    , taskWorking = lit TaskWorking
    , taskData = row.taskData
    , taskStatus = lit $ Serialized s'
    }


setWorking :: (Serial t, Serial (Status t)) => TaskWorking -> Task' t Expr -> Task' t Expr
setWorking w row =
  Task'
    { taskType = row.taskType
    , taskWorking = lit w
    , taskData = row.taskData
    , taskStatus = row.taskStatus
    }


isTask :: (Serial t) => t -> Task' t Expr -> Expr Bool
isTask t row = do
  row.taskData ==. lit (Serialized t)

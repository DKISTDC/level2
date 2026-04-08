{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Effectful.Tasks.WorkerTask where

import GHC.Generics
import NSO.Prelude
import Rel8 (DBType, ReadShow (..))


class WorkerTask t where
  type Status t :: Type
  idle :: Status t


  taskType :: Text
  default taskType :: (Generic t, GDatatypeName (Rep t)) => Text
  taskType = gDatatypeName $ from (undefined :: t)


data TaskWorking
  = TaskWaiting
  | TaskWorking
  | TaskComplete
  deriving (Generic, Eq, Read, Show)
  deriving (DBType) via ReadShow TaskWorking


class GDatatypeName f where
  gDatatypeName :: f p -> Text


instance (Datatype d) => GDatatypeName (M1 D d f) where
  gDatatypeName _ =
    cs $ datatypeName (undefined :: M1 D d f p)


data TaskStatus t
  = Missing
  | Waiting
  | Working (Status t)
  | Complete
instance (Eq (Status t)) => Eq (TaskStatus t) where
  Missing == Missing = True
  Waiting == Waiting = True
  Complete == Complete = True
  Working s1 == Working s2 = s1 == s2
  _ == _ = False

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Effectful.Tasks.WorkerTask where

import GHC.Generics
import NSO.Prelude


data Task t = Task
  { task :: t
  , status :: Status t
  , working :: TaskWorking
  }


class (Show t, Read t, Show (Status t), Read (Status t)) => WorkerTask t where
  type Status t :: Type
  type Status t = Bool


  idle :: Status t
  default idle :: (Status t ~ Bool) => Status t
  idle = False


  queue :: TaskQueue
  default queue :: (Generic t, GDatatypeName (Rep t)) => TaskQueue
  queue = TaskQueue $ gDatatypeName $ from (undefined :: t)


newtype TaskQueue = TaskQueue Text
  deriving newtype (IsString)


-- For Generic Read/Show Tasks
instance WorkerTask Text where
  type Status Text = Text
  idle = ""
  queue = ""


data TaskWorking
  = TaskWaiting
  | TaskWorking
  | TaskFailed
  deriving (Generic, Eq, Read, Show)


class GDatatypeName f where
  gDatatypeName :: f p -> Text


instance (Datatype d) => GDatatypeName (M1 D d f) where
  gDatatypeName _ =
    cs $ datatypeName (undefined :: M1 D d f p)

module NSO.DataStore.Datasets
  ( Datasets (..)
  , Filter (..)
  , Modify (..)
  , Id (..)
  , module NSO.Types.Dataset
  , send
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram


-- Put all the operations here?
data Datasets :: Effect where
  Query :: Filter -> Datasets m [Dataset]
  Create :: [Dataset] -> Datasets m ()
  Modify :: Modify -> [Id Dataset] -> Datasets m ()


type instance DispatchOf Datasets = 'Dynamic


data Filter
  = Latest
  | ByExperiment (Id Experiment)
  | ByProgram (Id InstrumentProgram)
  | ById (Id Dataset)


data Modify
  = SetOld

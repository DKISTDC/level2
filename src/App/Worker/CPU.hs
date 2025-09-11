module App.Worker.CPU where

import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import Effectful.Exception
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Prelude


parallelize :: (Concurrent :> es, Log :> es, Reader CPUWorkers :> es, Traversable t, IOE :> es) => t (Eff es a) -> Eff es (t a)
parallelize effs = do
  cpus <- ask
  forConcurrently effs $ \eff -> do
    withCPUs cpus 1 eff


data CPUWorkers = CPUWorkers
  { total :: Int
  , inUse :: TVar Int
  }


cpuWorkers :: (Concurrent :> es, Log :> es) => Int -> Eff es CPUWorkers
cpuWorkers requested = do
  cores <- getNumCapabilities
  let totalWorkers = max 1 (min requested (cores - 1))
  log Debug $ "CPU Workers (" <> show totalWorkers <> ") requested=" <> show requested <> " cores=" <> show cores
  CPUWorkers totalWorkers <$> newTVarIO 0


acquire :: (Concurrent :> es, IOE :> es, Log :> es) => CPUWorkers -> Int -> Eff es ()
acquire cpus n = do
  inUse' <- readTVarIO cpus.inUse
  log Debug $ "Acquire: " <> show n <> " with " <> show inUse' <> " in use"
  atomically $ do
    inUse <- readTVar cpus.inUse
    if inUse + n <= cpus.total
      then modifyTVar' cpus.inUse (+ n)
      else retry


release :: (Concurrent :> es) => CPUWorkers -> Int -> Eff es ()
release cpus n = atomically $ modifyTVar' cpus.inUse $ \inUse ->
  max 0 (inUse - n)


withCPUs :: (Concurrent :> es, IOE :> es, Log :> es) => CPUWorkers -> Int -> Eff es a -> Eff es a
withCPUs cpus n = bracket_ (acquire cpus n) (release cpus n)

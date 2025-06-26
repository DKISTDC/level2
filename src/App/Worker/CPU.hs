module App.Worker.CPU where

import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
import Effectful.Exception
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Prelude


parallelize :: (Concurrent :> es, Log :> es, Reader CPUWorkers :> es, Traversable t) => t (Eff es a) -> Eff es (t a)
parallelize effs = do
  cpus <- ask
  forConcurrently effs $ \eff -> do
    withCPUs cpus 1 eff


newtype CPUWorkers = CPUWorkers (TVar Int)


cpuWorkers :: (Concurrent :> es) => Int -> Eff es CPUWorkers
cpuWorkers n =
  CPUWorkers <$> newTVarIO n


acquire :: (Concurrent :> es) => CPUWorkers -> Int -> Eff es ()
acquire (CPUWorkers tv) n = atomically $ do
  available <- readTVar tv
  if available >= n
    then writeTVar tv (available - n)
    else retry


release :: (Concurrent :> es) => CPUWorkers -> Int -> Eff es ()
release (CPUWorkers tv) n = atomically $ modifyTVar' tv (+ n)


withCPUs :: (Concurrent :> es) => CPUWorkers -> Int -> Eff es a -> Eff es a
withCPUs cpus n = bracket_ (acquire cpus n) (release cpus n)

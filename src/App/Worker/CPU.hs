module App.Worker.CPU where

import Control.Monad (void)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Log
import NSO.Prelude


availableWorkerCPUs :: (Concurrent :> es) => Eff es Int
availableWorkerCPUs = do
  let saveCoresForWebserver = 1
  cores <- getNumCapabilities
  pure $ max 1 $ cores - saveCoresForWebserver


parallelize_ :: (Concurrent :> es, Log :> es) => [Eff es a] -> Eff es ()
parallelize_ effs = do
  cpus <- availableWorkerCPUs
  let numThreads = min 16 cpus
  log Debug $ "Parallelize: " <> show numThreads
  void $ pooledMapConcurrentlyN_ numThreads id effs

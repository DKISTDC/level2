module App.Worker.PuppetMaster where

import App.Worker.FitsGenWorker qualified as FitsGenWorker
import App.Worker.TaskChan
import Data.Set qualified as Set
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Log
import NSO.Data.Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude


-- The Puppeteer checks the status of systems and starts jobs as necessary
manageMinions :: (Concurrent :> es, Inversions :> es, Datasets :> es, Log :> es) => TaskChan FitsGenWorker.Task -> Eff es ()
manageMinions fits = do
  logDebug "GO MY MINIONS"

  -- ts <- getChanContents fits
  -- logTrace "FitsTasks" ts
  ts <- scanNeedsGenerate
  -- mapM_ (logTrace "Inversion") ts

  (wt, wk) <- atomically $ do
    mapM_ (taskAdd fits) ts
    wt <- taskChanWaiting fits
    wk <- taskChanWorking fits
    pure (wt, wk)

  -- logTrace "WORK" (Set.size wk)
  -- logTrace "WAIT" (Set.size wt)

  -- scan for

  -- forM_ [0 .. 10 :: Int] $ \n -> do
  --   writeChan fits $ FitsGenWorker.Task (Id (pack $ show n))

  threadDelay (10 * 1000 * 1000)


scanNeedsGenerate :: (Inversions :> es, Datasets :> es) => Eff es [FitsGenWorker.Task]
scanNeedsGenerate = do
  AllInversions ivs <- send Inversions.All
  pure $ map fitsGenTask $ filter (isGenerate . (.step)) ivs
 where
  isGenerate :: InversionStep -> Bool
  isGenerate = \case
    StepInverted _ -> True
    StepGenerating _ -> True
    -- StepDownloading _ -> True
    _ -> False

  fitsGenTask i = FitsGenWorker.Task i.inversionId i.programId

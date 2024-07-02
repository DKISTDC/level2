module App.Worker.PuppetMaster where

-- import Data.Set qualified as Set

import App.Worker.FitsGenWorker
import Data.Maybe (isNothing)
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Tasks
import NSO.Data.Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude


-- The Puppeteer checks the status of systems and starts jobs as necessary
manageMinions :: (Concurrent :> es, Inversions :> es, Datasets :> es, Log :> es, Tasks GenInversion :> es) => Eff es ()
manageMinions = do
  -- log Debug "GO FORTH MY MINIONS"

  -- ts <- getChanContents fits
  -- logTrace "FitsTasks" ts
  ts <- scanNeedsGenerate
  -- mapM_ (logTrace "Inversion") ts

  send $ TasksAdd ts
  -- (_, _) <- atomically $ do
  --   wt <- taskChanWaiting fits
  --   wk <- taskChanWorking fits
  --   pure (wt, wk)

  -- logTrace "WORK" (Set.size wk)
  -- logTrace "WAIT" (Set.size wt)

  -- scan for

  -- forM_ [0 .. 10 :: Int] $ \n -> do
  --   writeChan fits $ FitsGenWorker.Task (Id (pack $ show n))

  threadDelay (10 * 1000 * 1000)


scanNeedsGenerate :: (Inversions :> es, Datasets :> es) => Eff es [GenInversion]
scanNeedsGenerate = do
  AllInversions ivs <- send Inversions.All
  pure $ map genTask $ filter (\i -> isGenerateStep i.step && isNothing i.invError) ivs
 where
  genTask inv = GenInversion inv.proposalId inv.inversionId

  isGenerateStep :: InversionStep -> Bool
  isGenerateStep = \case
    StepInverted _ -> True
    StepGenerating _ -> True
    StepGenTransfer _ -> True
    -- StepDownloading _ -> True
    _ -> False

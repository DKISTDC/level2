module App.Worker.PuppetMaster where

import App.Worker.GenWorker
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
manageMinions :: (Concurrent :> es, Inversions :> es, Datasets :> es, Log :> es, Tasks GenFits :> es, Tasks GenAsdf :> es) => Eff es ()
manageMinions = do
  -- log Debug "GO FORTH MY MINIONS"

  AllInversions ivs <- send Inversions.All
  let gfs = generateFits ivs
  send $ TasksAdd gfs

  let gas = generateAsdf ivs
  send $ TasksAdd gas

  -- (_, _) <- atomically $ do
  --   wt <- taskChanWaiting fits
  --   wk <- taskChanWorking fits
  --   pure (wt, wk)

  -- logTrace "WORK" (Set.size wk)
  -- logTrace "WAIT" (Set.size wt)

  -- forM_ [0 .. 10 :: Int] $ \n -> do
  --   writeChan fits $ FitsGenWorker.Task (Id (pack $ show n))

  threadDelay (10 * 1000 * 1000)


generateFits :: [Inversion] -> [GenFits]
generateFits ivs = do
  map genTask $ filter (\i -> isFitsGenStep i.step && isNothing i.invError) ivs
 where
  genTask inv = GenFits inv.proposalId inv.inversionId


isFitsGenStep :: InversionStep -> Bool
isFitsGenStep = \case
  StepInverted _ -> True
  StepGenerating _ -> True
  StepGenTransfer _ -> True
  _ -> False


generateAsdf :: [Inversion] -> [GenAsdf]
generateAsdf ivs = do
  map genTask $ filter (\i -> isAsdfGenStep i.step && isNothing i.invError) ivs
 where
  genTask inv = GenAsdf inv.proposalId inv.inversionId


isAsdfGenStep :: InversionStep -> Bool
isAsdfGenStep = \case
  StepGeneratedFits _ -> True
  _ -> False

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

  -- 5 second delay
  threadDelay (5 * 1000 * 1000)


generateFits :: [Inversion] -> [GenFits]
generateFits ivs = do
  map genTask $ filter (\i -> needsFitsGen i.generate && isNothing i.invError) ivs
 where
  genTask inv = GenFits inv.proposalId inv.inversionId

  -- wait, what if it is already running?
  needsFitsGen :: StepGenerate -> Bool
  needsFitsGen = \case
    StepGenerateNone -> False -- these aren't on this step at all
    StepGenerateWaiting -> True
    StepGeneratingFits _ -> True
    _ -> False


generateAsdf :: [Inversion] -> [GenAsdf]
generateAsdf ivs = do
  map genTask $ filter (\i -> isAsdfGenStep i.generate && isNothing i.invError) ivs
 where
  genTask inv = GenAsdf inv.proposalId inv.inversionId

  isAsdfGenStep :: StepGenerate -> Bool
  isAsdfGenStep = \case
    StepGeneratingAsdf _ -> True
    _ -> False

module App.Worker.FitsGenWorker
  ( Task (..)
  , workTask
  , GenerateError
  ) where

import App.Globus (Globus, GlobusEndpoint, TaskStatus (..), Token, Token' (Access))
import App.Globus qualified as Globus
import App.Types
import Control.Monad.Loops
import Data.Diverse.Many
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Data.Datasets
import NSO.Data.Inversions as Inversions
import NSO.Fits.Generate.Error
import NSO.Fits.Generate.FetchL1 as Fetch (L1FrameDir, Path (..), fetchCanonicalDataset, listL1Frames)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion


data Task = Task
  { inversionId :: Id Inversion
  , instrumentProgramId :: Id InstrumentProgram
  }
  deriving (Show, Eq, Ord)


workTask
  :: (Globus :> es, FileSystem :> es, Datasets :> es, Inversions :> es, Reader (GlobusEndpoint App) :> es, Log :> es, Concurrent :> es, Error GenerateError :> es)
  => TMVar (Token Access)
  -> Task
  -> Eff es ()
workTask advar t = do
  logInfo $ "FitsGenTask: " <> show t
  logDebug " - Waiting for admin access token..."
  acc <- atomically $ readTMVar advar
  Globus.runWithAccess acc $ workTaskWithAccess t


workTaskWithAccess
  :: (Reader (Token Access) :> es, Globus :> es, FileSystem :> es, Datasets :> es, Inversions :> es, Reader (GlobusEndpoint App) :> es, Log :> es, Concurrent :> es, Error GenerateError :> es)
  => Task
  -> Eff es ()
workTaskWithAccess t = do
  logDebug " - fetching..."
  frameDir <- waitForL1Transfer t.inversionId t.instrumentProgramId
  l1 <- Fetch.listL1Frames frameDir
  logTrace " - frames" (length l1)
  logDebug " - ready to build fits!"

  send $ Inversions.SetGenerated t.inversionId
  logDebug " - done"


-- DONE: Fetch L1 Files, poll for status
-- TODO: Generate frames... all of em!

waitForL1Transfer :: (Log :> es, Reader (GlobusEndpoint App) :> es, Concurrent :> es, Globus :> es, Reader (Token Access) :> es, Inversions :> es, Datasets :> es, Error GenerateError :> es) => Id Inversion -> Id InstrumentProgram -> Eff es (Path L1FrameDir)
waitForL1Transfer ii ip = do
  inv <- loadInversion ii
  logTrace "WAIT L1" inv
  (taskId, frameDir) <- startTransferIfNeeded inv.step
  send $ Inversions.SetGenerating ii taskId frameDir.filePath
  untilM_ delay (isTransferComplete taskId)
  pure frameDir
 where
  startTransferIfNeeded = \case
    StepGenerating info -> do
      let g = grab @Generate info
      pure (Id g.taskId, Path g.frameDir)
    _ ->
      Fetch.fetchCanonicalDataset ip


isTransferComplete :: (Log :> es, Globus :> es, Reader (Token Access) :> es, Error GenerateError :> es) => Id Globus.Task -> Eff es Bool
isTransferComplete it = do
  task <- Globus.transferStatus it
  case task.status of
    Succeeded -> pure True
    Failed -> throwError $ L1TransferFailed it
    _ -> do
      logTrace "Transfer" $ Globus.taskPercentComplete task
      pure False


loadInversion :: (Inversions :> es, Error GenerateError :> es) => Id Inversion -> Eff es Inversion
loadInversion ii = do
  is <- send $ Inversions.ById ii
  case is of
    [inv] -> pure inv
    _ -> throwError $ MissingInversion ii


delay :: (Concurrent :> es) => Eff es ()
delay = threadDelay $ 2 * 1000 * 1000

module App.Worker.FitsGenWorker
  ( workTask
  , GenerateError
  ) where

import App.Globus (Globus, GlobusEndpoint, Token, Token' (Access))
import App.Globus qualified as Globus
import App.Types
import Control.Monad.Loops
import Data.Diverse.Many
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Worker
import NSO.Data.Datasets
import NSO.Data.Inversions as Inversions
import NSO.Fits.Generate.Error
import NSO.Fits.Generate.FetchL1 as Fetch (L1FrameDir, Path (..), fetchCanonicalDataset, listL1Frames)
import NSO.Prelude
import NSO.Types.InstrumentProgram


workTask
  :: ( Reader (Token Access) :> es
     , Globus :> es
     , FileSystem :> es
     , Datasets :> es
     , Inversions :> es
     , Reader (GlobusEndpoint App) :> es
     , Log :> es
     , Concurrent :> es
     , Error GenerateError :> es
     , Worker GenTask :> es
     )
  => GenTask
  -> Eff es ()
workTask t = do
  logDebug "START"

  send $ TaskSetStatus t GenStarted

  inv <- loadInversion t.inversionId
  (taskId, frameDir) <- startTransferIfNeeded inv.programId inv.step
  send $ Inversions.SetGenerating t.inversionId taskId frameDir.filePath
  send $ TaskSetStatus t GenTransferring

  logDebug " - waiting..."
  untilM_ delay (isTransferComplete taskId)
  send $ Inversions.SetGenTransferred t.inversionId

  logDebug " - done, getting frames..."
  l1 <- Fetch.listL1Frames frameDir
  logTrace " - frames" (length l1)
  logDebug " - ready to build fits!"

  send $ TaskSetStatus t $ GenCreating 0 100

  forM_ [0 .. 40] $ \n -> do
    send $ TaskSetStatus t $ GenCreating n 40
    threadDelay (200 * 1000)

  send $ Inversions.SetGenerated t.inversionId
  logDebug " - done"


-- DONE: Fetch L1 Files, poll for status
-- TODO: Generate frames... all of em!

startTransferIfNeeded :: (Error GenerateError :> es, Reader (Token Access) :> es, Reader (GlobusEndpoint App) :> es, Datasets :> es, Globus :> es) => Id InstrumentProgram -> InversionStep -> Eff es (Id Globus.Task, Path L1FrameDir)
startTransferIfNeeded ip = \case
  StepGenTransfer info -> do
    let t = grab @GenTransfer info
    pure (t.taskId, Path t.frameDir)
  StepGenerating info -> do
    let g = grab @GenTransfer info
    pure (g.taskId, Path g.frameDir)
  _ ->
    Fetch.fetchCanonicalDataset ip


isTransferComplete :: (Log :> es, Globus :> es, Reader (Token Access) :> es, Error GenerateError :> es) => Id Globus.Task -> Eff es Bool
isTransferComplete it = do
  task <- Globus.transferStatus it
  case task.status of
    Globus.Succeeded -> pure True
    Globus.Failed -> throwError $ L1TransferFailed it
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

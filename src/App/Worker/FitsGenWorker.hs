module App.Worker.FitsGenWorker
  ( Task (..)
  , workTask
  , GenerateError
  ) where

import App.Globus (Globus, GlobusEndpoint, TaskStatus (..), Token, Token' (Access))
import App.Globus qualified as Globus
import App.Types
import Control.Monad.Loops
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Data.Datasets
import NSO.Fits.Generate.Error
import NSO.Fits.Generate.FetchL1 as Fetch (fetchCanonicalDataset, listL1Frames)
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
  :: (Globus :> es, FileSystem :> es, Datasets :> es, Reader (GlobusEndpoint App) :> es, Log :> es, Concurrent :> es, Error GenerateError :> es)
  => TVar (Maybe (Token Access))
  -> Task
  -> Eff es ()
workTask advar t = do
  logInfo $ "FitsGenTask: " <> show t
  logDebug " - Waiting for admin access token..."
  acc <- pollUntilJust (readTVarIO advar)
  Globus.runWithAccess acc $ workTaskWithAccess t
 where
  pollUntilJust mm = untilJust $ do
    mval <- mm
    case mval of
      Nothing -> do
        delay
        pure Nothing
      Just val -> pure (Just val)


workTaskWithAccess
  :: (Reader (Token Access) :> es, Globus :> es, FileSystem :> es, Datasets :> es, Reader (GlobusEndpoint App) :> es, Log :> es, Concurrent :> es, Error GenerateError :> es)
  => Task
  -> Eff es ()
workTaskWithAccess t = do
  logDebug " - fetching..."
  (taskId, frameDir) <- Fetch.fetchCanonicalDataset t.instrumentProgramId

  logDebug " - polling"
  untilM_ delay (isTransferComplete taskId)

  l1 <- Fetch.listL1Frames frameDir

  logTrace " - frames" (length l1)
  logDebug " - ready to build fits"


-- DONE: Fetch L1 Files, poll for status
-- TODO: Generate frames... all of em!

isTransferComplete :: (Globus :> es, Reader (Token Access) :> es, Error GenerateError :> es) => Id Globus.Task -> Eff es Bool
isTransferComplete it = do
  task <- Globus.transferStatus it
  case task.status of
    Succeeded -> pure True
    Failed -> throwError $ L1TransferFailed it
    _ -> pure False


delay :: (Concurrent :> es) => Eff es ()
delay = threadDelay $ 2 * 1000 * 1000

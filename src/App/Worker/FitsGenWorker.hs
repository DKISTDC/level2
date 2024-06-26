module App.Worker.FitsGenWorker
  ( workTask
  , GenerateError
  ) where

import App.Globus (Globus, Token, Token' (Access))
import App.Globus qualified as Globus
import App.Scratch as Scratch
import App.Types
import Control.Monad (zipWithM)
import Control.Monad.Loops
import Data.Diverse.Many
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.GenRandom
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Time
import Effectful.Worker
import NSO.Data.Datasets
import NSO.Data.Inversions as Inversions
import NSO.Fits.Generate as Gen
import NSO.Fits.Generate.Error
import NSO.Fits.Generate.FetchL1 as Fetch (L1FrameDir, canonicalL1Frames, fetchCanonicalDataset, readTimestamps)
import NSO.Fits.Generate.Profile (ProfileFrames (..))
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Telescope.Fits qualified as Fits


workTask
  :: forall es
   . ( Reader (Token Access) :> es
     , Globus :> es
     , FileSystem :> es
     , Datasets :> es
     , Inversions :> es
     , Time :> es
     , Reader Scratch :> es
     , Log :> es
     , Concurrent :> es
     , Worker GenTask :> es
     , GenRandom :> es
     )
  => GenTask
  -> Eff es ()
workTask t = do
  res <- runErrorNoCallStack workWithError
  either failed pure res
 where
  workWithError = do
    log Debug "START"

    send $ TaskSetStatus t GenStarted

    inv <- loadInversion t.inversionId
    (taskId, frameDir) <- startTransferIfNeeded inv.programId inv.step
    send $ Inversions.SetGenerating t.inversionId taskId frameDir.filePath
    send $ TaskSetStatus t GenTransferring

    log Debug " - waiting..."
    untilM_ delay (isTransferComplete taskId)
    send $ Inversions.SetGenTransferred t.inversionId

    log Debug " - done, getting frames..."
    u <- Scratch.inversionUploads $ Scratch.inversion t.inversionId
    log Debug $ dump "InvResults" u.invResults
    log Debug $ dump "InvProfile" u.invProfile
    log Debug $ dump "OrigProfile" u.origProfile
    log Debug $ dump "Timestamps" u.timestamps

    qfs <- Gen.readQuantitiesFrames u.invResults
    pfs <- Gen.readFitProfileFrames u.invProfile
    pos <- Gen.readOrigProfileFrames u.origProfile
    ts <- Fetch.readTimestamps u.timestamps
    log Debug $ dump "Frames" (length qfs, length pfs.frames, length pos.frames, length ts)
    l1 <- Fetch.canonicalL1Frames frameDir ts
    gfs <- collateFrames qfs pfs.frames pos.frames l1
    let totalFrames = length gfs

    log Debug $ dump "Ready to Build!" (length gfs)
    send $ TaskSetStatus t $ GenCreating 0 100
    zipWithM_ (workFrame totalFrames pos.wavProfiles pfs.wavProfiles) [0 ..] gfs

    send $ Inversions.SetGenerated t.inversionId
    log Debug " - done"

  workFrame tot wpo wpf n g = do
    send $ TaskSetStatus t $ GenCreating n tot
    now <- currentTime
    (fits, dateBeg) <- Gen.generateL2Fits now t.inversionId wpo wpf g
    log Debug $ dump "write" dateBeg
    Gen.writeL2Frame t.inversionId fits dateBeg
    pure ()

  failed :: GenerateError -> Eff es ()
  failed err = do
    send $ Inversions.SetError t.inversionId (cs $ show err)


startTransferIfNeeded :: (Error GenerateError :> es, Reader (Token Access) :> es, Reader Scratch :> es, Datasets :> es, Globus :> es) => Id InstrumentProgram -> InversionStep -> Eff es (Id Globus.Task, Path' Dir Dataset)
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
      log Debug $ dump "Transfer" $ Globus.taskPercentComplete task
      pure False


loadInversion :: (Inversions :> es, Error GenerateError :> es) => Id Inversion -> Eff es Inversion
loadInversion ii = do
  is <- send $ Inversions.ById ii
  case is of
    [inv] -> pure inv
    _ -> throwError $ MissingInversion ii


delay :: (Concurrent :> es) => Eff es ()
delay = threadDelay $ 2 * 1000 * 1000

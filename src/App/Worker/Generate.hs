module App.Worker.Generate where

import App.Effect.Transfer (Transfer)
import App.Effect.Transfer qualified as Transfer
import App.Worker.CPU (CPUWorkers (..))
import App.Worker.CPU qualified as CPU
import App.Worker.Generate.Asdf qualified as Asdf
import App.Worker.Generate.Error (GenerateError (..), generateFailed, onCaughtError, onCaughtGlobus, runGenerateError)
import App.Worker.Generate.Fits (Skipped)
import App.Worker.Generate.Fits qualified as Fits
import App.Worker.Generate.Inputs (DownloadComplete (..))
import App.Worker.Generate.Inputs qualified as Inputs
import App.Worker.Generate.Level1 (Canonical (..))
import App.Worker.Generate.Level1 qualified as Level1
import Control.Monad.Loops (untilM_)
import Data.Either (isRight)
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock (diffUTCTime)
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception (catch)
import Effectful.GenRandom
import Effectful.Globus (GlobusError (..), Task, TaskStatus (..))
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError)
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.Primary (PrimaryError)
import NSO.Image.Types.Frame (Frames (..))
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (Proposal)
import Network.Globus qualified as Globus


data GenTask = GenTask {proposalId :: Id Proposal, inversionId :: Id Inversion}
  deriving (Eq)


instance Show GenTask where
  show g = "GenTask " <> show g.proposalId <> " " <> show g.inversionId


instance WorkerTask GenTask where
  type Status GenTask = GenStatus
  idle = GenWaiting


data GenStatus
  = GenWaiting
  | GenStarted
  | GenTransferring (Id Task)
  | GenTransferComplete
  | GenFrames {started :: UTCTime, skipped :: Int, complete :: Int, total :: Int, throughput :: Float}
  | GenAsdf
  deriving (Eq, Ord)


instance Show GenStatus where
  show GenWaiting = "GenFits Waiting"
  show GenStarted = "GenFits Started"
  show (GenTransferring _) = "GenFits Transferring"
  show GenTransferComplete = "GenFits Transfer Complete"
  show GenFrames{complete, total} = "GenFits Creating " <> show complete <> " " <> show total
  show GenAsdf = "GenAsdf"


generateTask
  :: forall es
   . ( Reader CPUWorkers :> es
     , Error GlobusError :> es
     , Datasets :> es
     , Inversions :> es
     , Time :> es
     , Scratch :> es
     , Log :> es
     , IOE :> es
     , Concurrent :> es
     , Tasks GenTask :> es
     , GenRandom :> es
     , Transfer :> es
     , IOE :> es
     )
  => GenTask
  -> Eff es ()
generateTask task = do
  -- catch globus static errors!
  res <- runErrorNoCallStack $ workWithError `catchError` (\_ e -> onCaughtGlobus e) `catch` onCaughtError
  either (generateFailed task.inversionId) pure res
 where
  workWithError :: Eff (Error GenerateError : es) ()
  workWithError = runGenerateError $ do
    logContext ("GEN " <> cs task.inversionId.fromId) $ do
      log Info "start"
      send $ TaskSetStatus task GenStarted

      files <- Inputs.inversionFiles task.proposalId task.inversionId
      slice <- Inputs.sliceMeta files

      inv <- Inputs.loadInversion task.inversionId
      dcanon <- Level1.canonicalDataset slice inv.datasets

      down <- downloadL1Frames task inv dcanon
      send $ TaskSetStatus task GenTransferComplete
      Inversions.setGenTransferred inv.inversionId

      frames <- Inputs.loadFrameInputs files dcanon down
      log Info $ dump "Fits Frames" (length frames)

      now <- currentTime
      send $ TaskSetStatus task $ GenFrames{started = now, skipped = 0, complete = 0, total = length frames, throughput = 0}

      -- Generate them in parallel with N = available CPUs
      metas :: Frames (Either Skipped L2FitsMeta) <- CPU.parallelize $ fmap (workFrame task slice) frames

      log Debug $ dump "WRITTEN: " (length $ NE.filter isRight metas.frames)
      Inversions.setGeneratedFits task.inversionId

      send $ TaskSetStatus task GenAsdf
      Asdf.generateAsdf files inv dcanon slice
      Inversions.setGeneratedAsdf inv.inversionId


-- | Generate a single frame
workFrame
  :: ( Tasks GenTask :> es
     , Time :> es
     , GenRandom :> es
     , Log :> es
     , Scratch :> es
     , Error GenerateError :> es
     , Error QuantityError :> es
     , Error ProfileError :> es
     , Error PrimaryError :> es
     )
  => GenTask
  -> SliceXY
  -> L2FrameInputs
  -> Eff es (Either Skipped L2FitsMeta)
workFrame t slice frameInputs = do
  res <- Fits.genFrame t.proposalId t.inversionId slice frameInputs
  case res of
    Left _ -> do
      send $ TaskModStatus @GenTask t addSkipped
    _ -> do
      now <- currentTime
      send $ TaskModStatus @GenTask t (addComplete now)
  pure res
 where
  addComplete :: UTCTime -> GenStatus -> GenStatus
  addComplete now = \case
    GenFrames{started, skipped, complete, total} ->
      GenFrames{started, skipped, complete = complete + 1, total, throughput = throughputPerSecond (complete + 1) now started}
    gs -> gs

  addSkipped :: GenStatus -> GenStatus
  addSkipped = \case
    GenFrames{started, skipped, complete, total, throughput} ->
      GenFrames{started, skipped = skipped + 1, complete, total, throughput}
    gs -> gs

  throughputPerSecond :: Int -> UTCTime -> UTCTime -> Float
  throughputPerSecond complete now started =
    fromIntegral complete / realToFrac (diffUTCTime now started)


downloadL1Frames
  :: (Log :> es, Concurrent :> es, Time :> es, Error GenerateError :> es, Scratch :> es, Transfer :> es, Tasks GenTask :> es)
  => GenTask
  -> Inversion
  -> Canonical Dataset
  -> Eff es DownloadComplete
downloadL1Frames task inv (Canonical ds) = do
  case inv.generate.transfer of
    Just _ -> pure DownloadComplete
    Nothing -> transfer
 where
  transfer = do
    downloadTaskId <- Transfer.scratchDownloadDatasets [ds]
    log Debug $ dump "Download" downloadTaskId

    send $ TaskSetStatus task $ GenTransferring downloadTaskId

    log Debug " - waiting..."
    untilM_ delay (taskComplete downloadTaskId)

    pure DownloadComplete
   where
    taskComplete downloadTaskId = do
      tsk <- Transfer.transferStatus downloadTaskId
      case tsk.status of
        Failed -> throwError $ L1TransferFailed downloadTaskId
        Succeeded -> pure True
        _ -> pure False


delay :: (Concurrent :> es) => Eff es ()
delay = threadDelay $ 2 * 1000 * 1000

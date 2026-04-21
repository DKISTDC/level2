{-# LANGUAGE DerivingVia #-}

module App.Worker.Generate where

import App.Effect.GlobusAccess (GlobusAccess)
import App.Effect.Transfer (Transfer, waitForTransfer)
import App.Effect.Transfer qualified as Transfer
import App.Worker.CPU (CPUWorkers (..))
import App.Worker.CPU qualified as CPU
import App.Worker.Generate.Asdf qualified as Asdf
import App.Worker.Generate.Error (FetchError (..), GenerateError (..), generateFailed, onCaughtError, onCaughtGlobus, runGenerateError)
import App.Worker.Generate.Fits (Skipped)
import App.Worker.Generate.Fits qualified as Fits
import App.Worker.Generate.Inputs qualified as Inputs
import App.Worker.Generate.Level1 (Downloaded (..))
import App.Worker.Generate.Level1 qualified as Level1
import Data.Either (isRight)
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock (diffUTCTime)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Exception (catch)
import Effectful.GenRandom
import Effectful.Globus as Globus (GlobusError (..), Task)
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files (Ingest, Level1, Output, Scratch)
import NSO.Generic
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError)
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.Primary (PrimaryError)
import NSO.Image.Types.Frame (Frames (..))
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (Proposal)


data GenTask = GenTask {proposalId :: Id Proposal, inversionId :: Id Inversion}
  deriving (Generic, Eq)
  deriving (Show, Read) via (NoFields GenTask)


instance WorkerTask GenTask where
  type Status GenTask = GenStatus
  idle = GenWaiting


data GenStatus
  = GenWaiting
  | GenStarted
  | GenTransferring (Id Globus.Task)
  | GenTransferComplete
  | GenFrames {started :: UTCTime, skipped :: Int, complete :: Int, total :: Int, throughput :: Float}
  | GenAsdf
  deriving (Eq, Ord, Show, Read)


-- instance Show GenStatus where
--   show GenWaiting = "GenFits Waiting"
--   show GenStarted = "GenFits Started"
--   show (GenTransferring _) = "GenFits Transferring"
--   show GenTransferComplete = "GenFits Transfer Complete"
--   show GenFrames{complete, skipped, total} = "GenFits Creating " <> show (complete + skipped) <> " " <> show total
--   show GenAsdf = "GenAsdf"

generateTask
  :: forall es
   . ( Reader CPUWorkers :> es
     , Error GlobusError :> es
     , Datasets :> es
     , Inversions :> es
     , Time :> es
     , Log :> es
     , IOE :> es
     , Scratch Ingest :> es
     , Scratch Output :> es
     , Concurrent :> es
     , Tasks :> es
     , GenRandom :> es
     , GlobusAccess Level1 :> es
     , Transfer Level1 Ingest :> es
     , Error TaskFail :> es
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
    logStatus "starting"
    taskSetStatus task GenStarted

    files <- Inputs.inversionFiles task.proposalId task.inversionId
    slice <- Inputs.sliceMeta files

    inv <- Inputs.loadInversion task.inversionId
    log Debug $ dump "Got Inversion" inv.inversionId

    logStatus "downloading"
    down <- downloadL1Frames task inv
    taskSetStatus task GenTransferComplete
    Inversions.setGenTransferred inv.inversionId

    logStatus "canonical dataset"
    dcanon <- Level1.canonicalDataset slice down

    logStatus "loading inputs"
    frames <- Inputs.loadFrameInputs files dcanon down
    log Info $ dump "Fits Frames" (length frames)

    now <- currentTime
    taskSetStatus task $ GenFrames{started = now, skipped = 0, complete = 0, total = length frames, throughput = 0}

    -- Generate them in parallel with N = available CPUs
    logStatus "generating frames"
    metas :: Frames (Either Skipped L2FitsMeta) <- CPU.parallelize $ fmap (workFrame task slice) frames

    log Debug $ dump "WRITTEN: " (length $ NE.filter isRight metas.frames)
    Inversions.setGeneratedFits task.inversionId

    logStatus "generating ASDF"
    taskSetStatus task GenAsdf
    Asdf.generateAsdf files inv dcanon slice
    Inversions.setGeneratedAsdf inv.inversionId


-- | Generate a single frame
workFrame
  :: ( Tasks :> es
     , Time :> es
     , GenRandom :> es
     , Log :> es
     , Scratch Output :> es
     , Error GenerateError :> es
     , Error QuantityError :> es
     , Error ProfileError :> es
     , Error PrimaryError :> es
     , Error FetchError :> es
     )
  => GenTask
  -> SliceXY
  -> L2FrameInputs
  -> Eff es (Either Skipped L2FitsMeta)
workFrame t slice frameInputs = do
  res <- Fits.genFrame t.proposalId t.inversionId slice frameInputs
  case res of
    Left _ -> do
      taskModStatus t addSkipped
    Right _ -> do
      now <- currentTime
      taskModStatus t (addComplete now)
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
  :: (Log :> es, Concurrent :> es, Time :> es, Error GenerateError :> es, Scratch Ingest :> es, Transfer Level1 Ingest :> es, GlobusAccess Level1 :> es, Datasets :> es, Tasks :> es)
  => GenTask
  -> Inversion
  -> Eff es (Downloaded [Id Dataset])
downloadL1Frames task inv = do
  ds <- datasets
  case inv.generate.transfer of
    Just _ -> pure $ Downloaded $ datasetIds ds
    Nothing -> transfer ds
 where
  datasets = Datasets.findIds inv.datasets

  datasetIds = fmap (.datasetId)

  transfer ds = do
    downloadTaskId <- Transfer.scratchDownloadDatasets ds
    log Debug $ dump "Download" downloadTaskId
    taskSetStatus task $ GenTransferring downloadTaskId
    log Debug " - waiting..."
    waitForTransfer @Level1 @Ingest (\_ -> L1TransferFailed downloadTaskId) downloadTaskId
    pure $ Downloaded $ datasetIds ds

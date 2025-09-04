module App.Worker.Generate where

import App.Effect.Transfer (Transfer, runTransfer)
import App.Effect.Transfer qualified as Transfer
import App.Worker.CPU (CPUWorkers (..))
import App.Worker.CPU qualified as CPU
import App.Worker.Generate.Error (FetchError (..), GenerateError (..), generateFailed, onCaughtError, runGenerateError)
import App.Worker.Generate.Fits (Skipped)
import App.Worker.Generate.Fits qualified as Fits
import App.Worker.Generate.Inputs (DownloadComplete (..))
import App.Worker.Generate.Inputs qualified as Inputs
import Control.Exception (Exception)
import Control.Monad.Catch (catch)
import Control.Monad.Loops (untilM_)
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock (diffUTCTime)
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Globus (Globus, Task, TaskStatus (..), Token, Token' (Access))
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch qualified as Scratch
import NSO.Image.Blanca (BlancaError (..))
import NSO.Image.Blanca as Blanca (collateFramesArms)
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError, QuantityImage)
import NSO.Image.Headers.Parse (requireKey, runParseError)
import NSO.Image.Headers.Types (SliceXY (..), VISPArmId (..))
import NSO.Image.L1Input
import NSO.Image.Primary (PrimaryError)
import NSO.Image.Types.Frame (Arms (..), Depth, Frames (..), SlitX)
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (Proposal)
import Network.Globus qualified as Globus
import System.FilePath (takeExtensions)
import Telescope.Asdf qualified as Asdf
import Telescope.Asdf.Error (AsdfError)
import Telescope.Data.Parser (ParseError)
import Telescope.Fits as Fits
import Telescope.Fits.Encoding as Fits


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
  | GenFrames {started :: UTCTime, skipped :: Int, complete :: Int, total :: Int, throughput :: Float}
  | GenAsdf
  deriving (Eq, Ord)


instance Show GenStatus where
  show GenWaiting = "GenFits Waiting"
  show GenStarted = "GenFits Started"
  show (GenTransferring _) = "GenFits Transferring"
  show GenFrames{complete, total} = "GenFits Creating " <> show complete <> " " <> show total
  show GenAsdf = "GenAsdf"


generateTask
  :: forall es
   . ( Reader (Token Access) :> es
     , Reader CPUWorkers :> es
     , Globus :> es
     , Datasets :> es
     , Inversions :> es
     , Time :> es
     , Scratch :> es
     , Log :> es
     , IOE :> es
     , Concurrent :> es
     , Tasks GenTask :> es
     , GenRandom :> es
     , IOE :> es
     )
  => GenTask
  -> Eff es ()
generateTask task = do
  res <- runErrorNoCallStack $ runTransfer $ catch workWithError onCaughtError
  either (generateFailed task.inversionId) pure res
 where
  workWithError :: Eff (Transfer : Error GenerateError : es) ()
  workWithError = runGenerateError $ do
    send $ TaskSetStatus task GenStarted

    (files, slice, inv, datasets) <- Inputs.loadInputs task.proposalId task.inversionId
    down <- downloadL1Frames task inv datasets
    Inversions.setGenTransferred inv.inversionId

    frames <- Inputs.loadFrameInputs task.proposalId task.inversionId files slice inv datasets down

    now <- currentTime
    send $ TaskSetStatus task $ GenFrames{started = now, skipped = 0, complete = 0, total = length frames, throughput = 0}

    -- Generate them in parallel with N = available CPUs
    metas :: Frames (Either Skipped L2FitsMeta) <- CPU.parallelize $ fmap (workFrame task slice) frames

    log Debug $ dump "WRITTEN: " (length $ NE.filter isRight metas.frames)
    Inversions.setGeneratedFits task.inversionId


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
  -> NonEmpty Dataset
  -> Eff es DownloadComplete
downloadL1Frames task inv ds = do
  case inv.generate.transfer of
    Just _ -> pure DownloadComplete
    Nothing -> transfer
 where
  transfer = do
    downloadTaskId <- Transfer.scratchDownloadDatasets $ NE.toList ds
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

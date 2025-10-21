module App.Worker.L1QualityImages where

import App.Effect.Transfer (Transfer)
import App.Effect.Transfer qualified as Transfer
import Control.Monad.Loops (untilM_)
import Data.List qualified as L
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Globus (TaskStatus (..))
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Tasks
import NSO.Data.Datasets as Datasets
import NSO.Data.Spectra as Spectra
import NSO.Files.Scratch as Scratch
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength


data L1QualityImageTask = L1QualityImageTask {proposalId :: Id Proposal, programId :: Id InstrumentProgram}


instance WorkerTask L1QualityImageTask where
  type Status L1QualityImageTask = L1QualityImageStatus
  idle = ImageWaiting


data L1QualityImageStatus
  = ImageWaiting
  | ImageStarted
  | ImageDownloading (Id Globus.Task)
  | ImageGenerating
  | ImageError QualityImageError -- can I just set it to an error state? Will it keep it around?
  | ImageDone
  deriving (Eq, Ord)


l1QualityImageTask :: forall es. (Log :> es, Scratch :> es, Tasks L1QualityImageTask :> es, Concurrent :> es, Transfer :> es, Datasets :> es) => L1QualityImageTask -> Eff es ()
l1QualityImageTask task = do
  res <- runErrorNoCallStack l1QualityImageWork
  case res of
    Left e -> do
      send $ TaskSave task (ImageError e)
    Right _ -> pure ()
 where
  l1QualityImageWork :: Eff (Error QualityImageError : es) ()
  l1QualityImageWork = do
    logContext ("L1Quality " <> cs task.programId.fromId) $ do
      send $ TaskSetStatus task ImageStarted
      fe630 <- findIronLine
      logStatus "downloading"
      downloadTaskId <- Transfer.scratchDownloadDatasets [fe630]
      send $ TaskSetStatus task (ImageDownloading downloadTaskId)
      untilM_ delay (taskComplete downloadTaskId)
      logStatus "generating"
      send $ TaskSetStatus task ImageGenerating
      logStatus "done"

  findIronLine = do
    ds <- Datasets.find (ByProgram task.programId)
    case L.find (Spectra.isLine FeI630) ds of
      Nothing -> throwError $ MissingIron task.programId (fmap (.datasetId) ds)
      Just d -> pure d

  delay = threadDelay $ 2 * 1000 * 1000

  taskComplete downloadTaskId = do
    tsk <- Transfer.transferStatus downloadTaskId
    case tsk.status of
      Failed -> throwError $ L1TransferFailed task.programId downloadTaskId
      Succeeded -> pure True
      _ -> pure False


-- assumes the files have been downloaded
-- -- TODO: uh-oh, I don't have the ability to read bintable stuff!
-- genSpectralPlot :: (Scratch :> es, Error FetchError :> es) => Dataset -> Eff es ()
-- genSpectralPlot d = do
--   frames <- L1.allL1IntensityFrames d.primaryProposalId d.datasetId
--   _ <- L1.readLevel1File (Files.dataset d) (head frames)
--   pure _

data QualityImageError
  = MissingIron (Id InstrumentProgram) [Id Dataset]
  | L1TransferFailed (Id InstrumentProgram) (Id Globus.Task)
  deriving (Show, Eq, Ord)

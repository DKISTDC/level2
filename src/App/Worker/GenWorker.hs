{-# LANGUAGE RecordWildCards #-}

module App.Worker.GenWorker where

import App.Effect.Scratch as Scratch
import App.Globus (Globus, Token, Token' (Access))
import App.Globus qualified as Globus
import App.Worker.CPU qualified as CPU
import App.Worker.Generate as Gen
import Control.Monad (zipWithM)
import Control.Monad.Catch (catch)
import Control.Monad.Loops
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Inversions as Inversions
import NSO.Image.Asdf as Asdf
import NSO.Image.Frame as Frame
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.Profile (Fit, Original, ProfileFit (..), ProfileFrames (..), WavProfiles, decodeProfileFit, decodeProfileOrig)
import NSO.Image.Quantity (decodeQuantitiesFrames)
import NSO.Prelude
import NSO.Types.InstrumentProgram


data GenFits = GenFits {proposalId :: Id Proposal, inversionId :: Id Inversion}
  deriving (Eq)


instance Show GenFits where
  show g = "GenFits " <> show g.proposalId <> " " <> show g.inversionId


instance WorkerTask GenFits where
  type Status GenFits = GenFitsStatus
  idle = GenFitsStatus GenWaiting 0 0


data GenFitsStep
  = GenWaiting
  | GenStarted
  | GenTransferring
  | GenCreating
  deriving (Show, Eq, Ord)


data GenFitsStatus = GenFitsStatus
  { step :: GenFitsStep
  , complete :: Int
  , total :: Int
  }
  deriving (Eq, Ord)


instance Show GenFitsStatus where
  show g = "GenFitsStatus " <> show g.step <> " " <> show g.complete <> " " <> show g.total


fitsTask
  :: forall es
   . ( Reader (Token Access) :> es
     , Globus :> es
     , Datasets :> es
     , Inversions :> es
     , Time :> es
     , Scratch :> es
     , Log :> es
     , Concurrent :> es
     , Tasks GenFits :> es
     , GenRandom :> es
     , IOE :> es
     )
  => GenFits
  -> Eff es ()
fitsTask t = do
  res <- runErrorNoCallStack $ catch workWithError onCaughtError
  either (failed t.inversionId) pure res
 where
  workWithError :: Eff (Error GenerateError : es) ()
  workWithError = runGenerateError $ do
    log Debug "START"

    send $ TaskSetStatus t $ GenFitsStatus GenStarted 0 0

    inv <- loadInversion t.inversionId
    d <- requireCanonicalDataset inv.programId

    taskId <- startTransferIfNeeded d inv
    log Debug $ dump "Task" taskId
    Inversions.setGenerating t.inversionId taskId
    send $ TaskSetStatus t $ GenFitsStatus GenTransferring 0 0

    log Debug " - waiting..."
    untilM_ delay (isTransferComplete taskId)
    Inversions.setGenTransferred t.inversionId

    log Debug " - done, getting frames..."
    let u = Scratch.inversionUploads $ Scratch.blanca t.proposalId t.inversionId
    log Debug $ dump "InvResults" u.invResults
    log Debug $ dump "InvProfile" u.invProfile
    log Debug $ dump "OrigProfile" u.origProfile
    -- log Debug $ dump "Timestamps" u.timestamps

    quantities <- decodeQuantitiesFrames =<< readFile u.invResults
    ProfileFit profileFit slice <- decodeProfileFit =<< readFile u.invProfile
    profileOrig <- decodeProfileOrig =<< readFile u.origProfile

    l1 <- Gen.canonicalL1Frames (Scratch.dataset d) slice
    log Debug $ dump "Frames" (length quantities, length profileFit.frames, length profileOrig.frames, length l1)

    gfs <- Gen.collateFrames quantities profileFit.frames profileOrig.frames l1
    send $ TaskSetStatus t $ GenFitsStatus{step = GenCreating, complete = 0, total = NE.length gfs}

    -- Generate them in parallel with N = available CPUs
    metas <- CPU.parallelize $ fmap (workFrame t slice profileOrig.wavProfiles profileFit.wavProfiles) gfs

    log Debug $ dump "DONE: " (length metas)
    Inversions.setGeneratedFits t.inversionId


-- | Generate a single frame
workFrame
  :: ( Tasks GenFits :> es
     , Time :> es
     , GenRandom :> es
     , Log :> es
     , Scratch :> es
     , Error GenerateError :> es
     )
  => GenFits
  -> SliceXY
  -> WavProfiles Original
  -> WavProfiles Fit
  -> L2FrameInputs
  -> Eff es L2FrameMeta
workFrame t slice wavOrig wavFit frameInputs = runGenerateError $ do
  now <- currentTime
  (frame, dateBeg) <- Frame.generateL2Frame now t.inversionId slice wavOrig wavFit frameInputs
  let fits = Frame.frameToFits frame
  let path = Scratch.outputL2Frame t.proposalId t.inversionId dateBeg
  Scratch.writeFile path $ Frame.encodeL2 fits
  send $ TaskModStatus @GenFits t updateNumFrame
  log Debug path.filePath
  pure $ Frame.frameMeta frame (filenameL2Frame t.inversionId dateBeg)
 where
  updateNumFrame :: GenFitsStatus -> GenFitsStatus
  updateNumFrame GenFitsStatus{..} = GenFitsStatus{complete = complete + 1, ..}


-- workAsdf
--   :: (Time :> es, Error GenerateError :> es, IOE :> es, Scratch :> es)
--   => Inversion
--   -> NonEmpty L2FrameMeta
--   -> Eff es ()
-- workAsdf inv metas = runGenerateError $ do
--   now <- currentTime
--   let datasetIds = fmap Id $ maybe [] (.datasets) (findDownloaded inv.step)
--   let tree = asdfDocument inv.inversionId datasetIds now $ NE.sort metas
--   let path = Scratch.outputL2Asdf inv.proposalId inv.inversionId
--   output <- Asdf.encodeL2 tree
--   Scratch.writeFile path output

startTransferIfNeeded :: (Log :> es, Error GenerateError :> es, Reader (Token Access) :> es, Scratch :> es, Datasets :> es, Globus :> es) => Dataset -> Inversion -> Eff es (Id Globus.Task)
startTransferIfNeeded d inv =
  case inv.generate of
    StepGenerateNone -> Globus.initScratchDataset d
    StepGenerateTransfer taskId -> pure taskId
    StepGeneratedFits gen -> pure gen.transfer
    StepGenerated gen -> pure gen.transfer


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


-- ASDF -----------------------------

data GenAsdf = GenAsdf {proposalId :: Id Proposal, inversionId :: Id Inversion}
  deriving (Eq)


instance Show GenAsdf where
  show g = "GenAsdf " <> show g.proposalId <> " " <> show g.inversionId


instance WorkerTask GenAsdf where
  type Status GenAsdf = ()
  idle = ()


asdfTask
  :: forall es
   . ( Reader (Token Access) :> es
     , Globus :> es
     , Datasets :> es
     , Inversions :> es
     , Time :> es
     , Scratch :> es
     , Log :> es
     , Concurrent :> es
     , Tasks GenAsdf :> es
     , IOE :> es
     )
  => GenAsdf
  -> Eff es ()
asdfTask t = do
  res <- runErrorNoCallStack $ catch workWithError onCaughtError
  either (failed t.inversionId) pure res
 where
  workWithError :: Eff (Error GenerateError : es) ()
  workWithError = runGenerateError $ do
    log Debug "ASDF!"
    log Debug $ dump "Task" t

    inv <- loadInversion t.inversionId
    d <- requireCanonicalDataset inv.programId

    -- TODO: load the L1 fits files
    -- TODO: generate all the stuffs
    let u = Scratch.inversionUploads $ Scratch.blanca t.proposalId t.inversionId

    ProfileFit profileFit slice <- decodeProfileFit =<< readFile u.invProfile
    profileOrig <- decodeProfileOrig =<< readFile u.origProfile

    l1fits <- Gen.canonicalL1Frames (Scratch.dataset d) slice

    (metas :: NonEmpty L2FrameMeta) <- requireMetas slice profileOrig.wavProfiles profileFit.wavProfiles l1fits

    log Debug $ dump "metas" (length metas)

    now <- currentTime
    let datasetIds = downloadedDatasetIds inv
    let tree = asdfDocument inv.inversionId datasetIds now $ NE.sort metas
    let path = Scratch.outputL2Asdf inv.proposalId inv.inversionId
    output <- Asdf.encodeL2 tree
    Scratch.writeFile path output

    log Debug " - Generated ASDF"
    log Debug " - done"
    Inversions.setGeneratedAsdf t.inversionId

  requireMetas slice wpo wpf l1fits = do
    metas <- loadMetas slice wpo wpf l1fits
    case metas of
      (m : ms) -> pure (m :| ms)
      _ -> throwError MissingL2Fits

  loadMetas slice wpo wpf l1fits = do
    paths <- l2FramePaths t.proposalId t.inversionId
    zipWithM loadL2FrameMeta paths l1fits
   where
    loadL2FrameMeta path l1 = do
      fits <- readLevel2Fits t.proposalId t.inversionId path
      Frame.frameMetaFromL2Fits path slice wpo wpf l1 fits


failed :: (Log :> es, Inversions :> es) => Id Inversion -> GenerateError -> Eff es ()
failed iid err = do
  log Err $ dump "GenerateError" err
  Inversions.setError iid (cs $ show err)


onCaughtError :: IOError -> Eff (Error GenerateError : es) a
onCaughtError e = do
  throwError $ GenIOError e

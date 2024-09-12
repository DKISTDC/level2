{-# LANGUAGE RecordWildCards #-}

module App.Worker.FitsGenWorker
  ( workTask
  , GenerateError
  , GenInversion (..)
  , GenStatus (..)
  , GenInvStep (..)
  , Tasks
  , workFrame
  ) where

import App.Effect.Scratch as Scratch
import App.Globus (Globus, Token, Token' (Access))
import App.Globus qualified as Globus
import App.Worker.CPU qualified as CPU
import App.Worker.Generate as Gen
import Control.Monad.Catch (catch)
import Control.Monad.Loops
import Data.Diverse.Many
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
import NSO.Image.Quantities (decodeQuantitiesFrames)
import NSO.Prelude
import NSO.Types.InstrumentProgram


data GenInversion = GenInversion {proposalId :: Id Proposal, inversionId :: Id Inversion}
  deriving (Eq)


instance Show GenInversion where
  show g = "GenInversion " <> show g.proposalId <> " " <> show g.inversionId


instance WorkerTask GenInversion where
  type Status GenInversion = GenStatus
  idle = GenStatus GenWaiting 0 0


data GenInvStep
  = GenWaiting
  | GenStarted
  | GenTransferring
  | GenCreating
  | GenCreatingAsdf
  deriving (Show, Eq, Ord)


data GenStatus = GenStatus
  { step :: GenInvStep
  , complete :: Int
  , total :: Int
  }
  deriving (Eq, Ord)


instance Show GenStatus where
  show g = "GenStatus " <> show g.step <> " " <> show g.complete <> " " <> show g.total


workTask
  :: forall es
   . ( Reader (Token Access) :> es
     , Globus :> es
     , Datasets :> es
     , Inversions :> es
     , Time :> es
     , Scratch :> es
     , Log :> es
     , Concurrent :> es
     , Tasks GenInversion :> es
     , GenRandom :> es
     , IOE :> es
     )
  => GenInversion
  -> Eff es ()
workTask t = do
  res <- runErrorNoCallStack $ catch workWithError onCaughtError
  either failed pure res
 where
  onCaughtError :: IOError -> Eff (Error GenerateError : es) a
  onCaughtError e = do
    throwError $ GenIOError e

  workWithError :: Eff (Error GenerateError : es) ()
  workWithError = runGenerateError $ do
    log Debug "START"

    send $ TaskSetStatus t $ GenStatus GenStarted 0 0

    inv <- loadInversion t.inversionId
    d <- requireCanonicalDataset inv.programId

    taskId <- startTransferIfNeeded d inv.step
    let frameDir = Scratch.dataset d
    log Debug $ dump "Task" taskId
    send $ Inversions.SetGenerating t.inversionId taskId
    send $ TaskSetStatus t $ GenStatus GenTransferring 0 0

    log Debug " - waiting..."
    untilM_ delay (isTransferComplete taskId)
    send $ Inversions.SetGenTransferred t.inversionId

    log Debug " - done, getting frames..."
    let u = Scratch.inversionUploads $ Scratch.blanca t.proposalId t.inversionId
    log Debug $ dump "InvResults" u.invResults
    log Debug $ dump "InvProfile" u.invProfile
    log Debug $ dump "OrigProfile" u.origProfile
    -- log Debug $ dump "Timestamps" u.timestamps

    quantities <- decodeQuantitiesFrames =<< readFile u.invResults
    ProfileFit profileFit slice <- decodeProfileFit =<< readFile u.invProfile
    profileOrig <- decodeProfileOrig =<< readFile u.origProfile

    l1 <- Gen.canonicalL1Frames frameDir slice
    log Debug $ dump "Frames" (length quantities, length profileFit.frames, length profileOrig.frames, length l1)

    gfs <- Gen.collateFrames quantities profileFit.frames profileOrig.frames l1
    send $ TaskSetStatus t $ GenStatus{step = GenCreating, complete = 0, total = NE.length gfs}

    -- Generate them in parallel with N = available CPUs
    metas <- CPU.parallelize $ fmap (workFrame t slice profileOrig.wavProfiles profileFit.wavProfiles) gfs

    log Debug $ dump "DONE: " (length metas)
    send $ SetGeneratedFits t.inversionId
    send $ TaskSetStatus t $ GenStatus{step = GenCreatingAsdf, complete = NE.length gfs, total = NE.length gfs}

    workAsdf inv metas

    log Debug " - Generated ASDF"
    log Debug " - done"
    send $ SetGeneratedAsdf t.inversionId

  failed :: GenerateError -> Eff es ()
  failed err = do
    log Err $ dump "GenerateError" err
    send $ Inversions.SetError t.inversionId (cs $ show err)


-- | Generate a single frame
workFrame
  :: ( Tasks GenInversion :> es
     , Time :> es
     , GenRandom :> es
     , Log :> es
     , Scratch :> es
     , Error GenerateError :> es
     )
  => GenInversion
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
  send $ TaskModStatus @GenInversion t updateNumFrame
  log Debug path.filePath
  pure $ Frame.frameMeta frame (filenameL2Frame t.inversionId dateBeg)
 where
  updateNumFrame :: GenStatus -> GenStatus
  updateNumFrame GenStatus{..} = GenStatus{complete = complete + 1, ..}


workAsdf
  :: (Time :> es, Error GenerateError :> es, IOE :> es, Scratch :> es)
  => Inversion
  -> NonEmpty L2FrameMeta
  -> Eff es ()
workAsdf inv metas = runGenerateError $ do
  now <- currentTime
  let datasetIds = fmap Id $ maybe [] (.datasets) (findDownloaded inv.step)
  let tree = asdfDocument inv.inversionId datasetIds now metas
  let path = Scratch.outputL2Asdf inv.proposalId inv.inversionId
  output <- Asdf.encodeL2 tree
  Scratch.writeFile path output


startTransferIfNeeded :: (Log :> es, Error GenerateError :> es, Reader (Token Access) :> es, Scratch :> es, Datasets :> es, Globus :> es) => Dataset -> InversionStep -> Eff es (Id Globus.Task)
startTransferIfNeeded d = \case
  StepGenTransfer info -> do
    let t = grab @Transfer info
    pure t.taskId
  StepGenerating info -> do
    let g = grab @Transfer info
    pure g.taskId
  _ ->
    Globus.initScratchDataset d


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

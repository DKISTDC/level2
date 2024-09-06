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
import App.Worker.CPU as CPU (parallelize_)
import Control.Monad.Catch (catch)
import Control.Monad.Loops
import Data.Diverse.Many
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
import NSO.Fits.Generate as Gen
import NSO.Fits.Generate.Error
import NSO.Fits.Generate.FetchL1 as Fetch (canonicalL1Frames, requireCanonicalDataset)
import NSO.Fits.Generate.Profile (Fit, Original, ProfileFrames (..), WavProfiles)
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
  workWithError = do
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

    qfs <- Gen.decodeQuantitiesFrames =<< readFile u.invResults
    ProfileFit pfs slice <- Gen.decodeProfileFit =<< readFile u.invProfile
    pos <- Gen.decodeProfileOrig =<< readFile u.origProfile

    l1 <- Fetch.canonicalL1Frames frameDir slice
    log Debug $ dump "Frames" (length qfs, length pfs.frames, length pos.frames, length l1)

    gfs <- collateFrames qfs pfs.frames pos.frames l1
    send $ TaskSetStatus t $ GenStatus{step = GenCreating, complete = 0, total = length gfs}

    -- Generate them in parallel with N = available CPUs
    CPU.parallelize_ $ fmap (workFrame t slice pos.wavProfiles pfs.wavProfiles) gfs

    send $ SetGenerated t.inversionId
    log Debug " - done"

  failed :: GenerateError -> Eff es ()
  failed err = do
    log Err $ dump "GenerateError" err
    send $ Inversions.SetError t.inversionId (cs $ show err)


-- | Generate a single frame
workFrame
  :: (Tasks GenInversion :> es, Time :> es, Error GenerateError :> es, GenRandom :> es, Log :> es, Scratch :> es)
  => GenInversion
  -> SliceXY
  -> WavProfiles Original
  -> WavProfiles Fit
  -> L2FrameInputs
  -> Eff es ()
workFrame t slice wavOrig wavFit g = do
  now <- currentTime
  (frame, dateBeg) <- Gen.generateL2Frame now t.inversionId slice wavOrig wavFit g
  let fits = Gen.frameToFits frame
  let path = Scratch.outputL2Frame t.proposalId t.inversionId dateBeg
  Scratch.writeFile path $ Gen.encodeL2 fits
  send $ TaskModStatus @GenInversion t updateNumFrame
  log Debug path.filePath
 where
  updateNumFrame :: GenStatus -> GenStatus
  updateNumFrame GenStatus{..} = GenStatus{complete = complete + 1, ..}


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

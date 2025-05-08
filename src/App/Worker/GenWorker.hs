module App.Worker.GenWorker where

import App.Effect.Scratch as Scratch
import App.Effect.Transfer qualified as Transfer
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
import Effectful.Globus (Globus, GlobusError, Task, TaskStatus (..), Token, Token' (Access))
import Effectful.Globus qualified as Globus
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
  idle = GenWaiting


data GenFitsStatus
  = GenWaiting
  | GenStarted
  | GenTransferring (Id Task)
  | GenFrames {complete :: Int, total :: Int}
  deriving (Eq, Ord)


instance Show GenFitsStatus where
  show GenWaiting = "GenFits Waiting"
  show GenStarted = "GenFits Started"
  show (GenTransferring _) = "GenFits Transferring"
  show GenFrames{complete, total} = "GenFits Creating " <> show complete <> " " <> show total


fitsTask
  :: forall es
   . ( Reader (Token Access) :> es
     , Globus :> es
     , Error GlobusError :> es
     , Datasets :> es
     , Inversions :> es
     , Time :> es
     , Scratch :> es
     , Log :> es
     , IOE :> es
     , Concurrent :> es
     , Tasks GenFits :> es
     , GenRandom :> es
     , IOE :> es
     )
  => Int
  -> GenFits
  -> Eff es ()
fitsTask numWorkers task = do
  res <- runErrorNoCallStack $ catch workWithError onCaughtError
  either (failed task.inversionId) pure res
 where
  workWithError :: Eff (Error GenerateError : es) ()
  workWithError = runGenerateError $ do
    log Debug "START"

    send $ TaskSetStatus task GenStarted

    inv <- loadInversion task.inversionId
    d <- requireCanonicalDataset inv.programId

    transferL1Frames task d inv

    log Debug " - done, getting frames..."
    let u = Scratch.inversionUploads $ Scratch.blanca task.proposalId task.inversionId
    log Debug $ dump "InvResults" u.quantities
    log Debug $ dump "InvProfile" u.profileFit
    log Debug $ dump "OrigProfile" u.profileOrig
    -- log Debug $ dump "Timestamps" u.timestamps

    quantities <- decodeQuantitiesFrames =<< readFile u.quantities
    ProfileFit profileFit slice <- decodeProfileFit =<< readFile u.profileFit
    profileOrig <- decodeProfileOrig =<< readFile u.profileOrig

    l1 <- Gen.canonicalL1Frames (Scratch.dataset d) slice
    log Debug $ dump "Frames" (length quantities, length profileFit.frames, length profileOrig.frames, length l1)

    gfs <- Gen.collateFrames quantities profileFit.frames profileOrig.frames l1
    send $ TaskSetStatus task $ GenFrames{complete = 0, total = NE.length gfs}

    -- Generate them in parallel with N = available CPUs
    metas <- CPU.parallelizeN numWorkers $ fmap (workFrame task slice profileOrig.wavProfiles profileFit.wavProfiles) gfs

    log Debug $ dump "DONE: " (length metas)
    Inversions.setGeneratedFits task.inversionId


transferL1Frames
  :: (IOE :> es, Log :> es, Error GlobusError :> es, Inversions :> es, Concurrent :> es, Tasks GenFits :> es, Time :> es, Error GenerateError :> es, Reader (Token Access) :> es, Scratch :> es, Globus :> es)
  => GenFits
  -> Dataset
  -> Inversion
  -> Eff es ()
transferL1Frames task d inv = do
  case inv.generate.transfer of
    Just _ -> pure ()
    Nothing -> transfer
 where
  transfer = do
    taskId <- Transfer.initScratchDataset d
    log Debug $ dump "Task" taskId

    send $ TaskSetStatus task $ GenTransferring taskId

    log Debug " - waiting..."
    untilM_ delay (taskComplete taskId)

    Inversions.setGenTransferred task.inversionId
   where
    taskComplete taskId = do
      tsk <- Transfer.transferStatus taskId
      case tsk.status of
        Failed -> throwError $ L1TransferFailed taskId
        Succeeded -> pure True
        _ -> pure False


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
  updateNumFrame GenFrames{complete, total} = GenFrames{complete = complete + 1, total}
  updateNumFrame gs = gs


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

    ProfileFit profileFit slice <- decodeProfileFit =<< readFile u.quantities
    profileOrig <- decodeProfileOrig =<< readFile u.profileOrig

    l1fits <- Gen.canonicalL1Frames (Scratch.dataset d) slice

    (metas :: NonEmpty L2FrameMeta) <- requireMetas slice profileOrig.wavProfiles profileFit.wavProfiles l1fits

    log Debug $ dump "metas" (length metas)

    now <- currentTime
    let tree = asdfDocument inv.inversionId inv.datasets now $ NE.sort metas
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

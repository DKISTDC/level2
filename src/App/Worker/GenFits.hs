{-# LANGUAGE ScopedTypeVariables #-}

module App.Worker.GenFits where

import App.Effect.Transfer (Transfer, runTransfer)
import App.Effect.Transfer qualified as Transfer
import App.Worker.CPU (CPUWorkers (..))
import App.Worker.CPU qualified as CPU
import App.Worker.Generate as Gen
import Control.Monad.Catch (catch)
import Control.Monad.Loops
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Time.Clock (diffUTCTime)
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Globus (Globus, Task, TaskStatus (..), Token, Token' (Access))
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Datasets qualified as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files as Files
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch as Scratch
import NSO.Image.Blanca qualified as Blanca
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (decodeQuantitiesFrames)
import NSO.Image.Headers.Parse (requireKey)
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.L1Input (L1Fits (..), L1Frame)
import NSO.Image.Types.Frame (Arms (..), Frames (..))
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import Telescope.Data.Parser (ParseError)
import Telescope.Fits (Fits (..))


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
  | GenFrames {started :: UTCTime, skipped :: Int, complete :: Int, total :: Int, throughput :: Float}
  deriving (Eq, Ord)


instance Show GenFitsStatus where
  show GenWaiting = "GenFits Waiting"
  show GenStarted = "GenFits Started"
  show (GenTransferring _) = "GenFits Transferring"
  show GenFrames{complete, total} = "GenFits Creating " <> show complete <> " " <> show total


fitsTask
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
     , Tasks GenFits :> es
     , GenRandom :> es
     , IOE :> es
     )
  => GenFits
  -> Eff es ()
fitsTask task = do
  res <- runErrorNoCallStack $ runTransfer $ catch workWithError Gen.onCaughtError
  either (Gen.failed task.inversionId) pure res
 where
  workWithError :: Eff (Transfer : Error GenerateError : es) ()
  workWithError = runGenerateError $ do
    log Debug "START"
    send $ TaskSetStatus task GenStarted

    -- Load Metadata ----------------
    let u = Files.inversionFiles $ Files.blancaInput task.proposalId task.inversionId
    log Debug $ dump "InvResults" u.quantities
    log Debug $ dump "InvProfile" u.profileFit
    log Debug $ dump "OrigProfile" u.profileOrig
    slice <- sliceMeta u

    inv <- Gen.loadInversion task.inversionId
    dss :: [Dataset] <- Datasets.find $ Datasets.ByIds inv.datasets
    ds :: NonEmpty Dataset <- requireDatasets inv.datasets dss

    -- Download Canonical Dataset
    log Debug $ dump "Meta Complete" slice
    downloadL1Frames task inv ds

    log Debug $ dump "Downloaded L1" (fmap Files.dataset ds)
    dc <- requireCanonicalDataset slice $ NE.toList ds

    log Debug $ dump "Canonical Dataset:" dc.datasetId

    quantities <- decodeQuantitiesFrames =<< readFile u.quantities
    log Debug $ dump "Quantities" ()

    fitHDUs <- Blanca.decodeProfileHDUs =<< readFile u.profileFit
    arms <- Blanca.decodeArmWavMeta fitHDUs
    log Debug $ dump "Profile Arms " arms

    profileFit <- Blanca.decodeProfileArms arms fitHDUs
    log Debug $ dump "Profile Fit" (length profileFit.arms)

    origHDUs <- Blanca.decodeProfileHDUs =<< readFile u.profileOrig
    profileOrig <- Blanca.decodeProfileArms arms origHDUs
    log Debug $ dump "Profile Orig" (length profileOrig.arms)

    l1 <- Gen.canonicalL1Frames (Files.dataset dc)
    log Debug $ dump "Frames" (length quantities, armFramesLength profileFit, armFramesLength profileOrig, length l1)

    gfs <- Gen.collateFrames quantities arms profileFit profileOrig l1

    now <- currentTime
    send $ TaskSetStatus task $ GenFrames{started = now, skipped = 0, complete = 0, total = length gfs, throughput = 0}

    -- Generate them in parallel with N = available CPUs
    metas :: Frames (Either Skipped L2FitsMeta) <- CPU.parallelize $ fmap (workFrame task slice) gfs

    log Debug $ dump "WRITTEN: " (length $ NE.filter isRight metas.frames)
    Inversions.setGeneratedFits task.inversionId
   where
    requireDatasets ids = \case
      [] -> throwError $ NoDatasets ids
      (d : ds) -> pure $ d :| ds


downloadL1Frames
  :: (IOE :> es, Log :> es, Inversions :> es, Datasets :> es, Concurrent :> es, Tasks GenFits :> es, Time :> es, Error GenerateError :> es, Scratch :> es, Transfer :> es)
  => GenFits
  -> Inversion
  -> NonEmpty Dataset
  -> Eff es ()
downloadL1Frames task inv ds = do
  case inv.generate.transfer of
    Just _ -> pure ()
    Nothing -> transfer
 where
  transfer = do
    taskId <- Transfer.scratchDownloadDatasets $ NE.toList ds
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


type Skipped = ()


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
  -> L2FrameInputs
  -> Eff es (Either Skipped L2FitsMeta)
workFrame t slice frameInputs = do
  runGenerateError . runErrorNoCallStackWith (throwError . ParseError frameInputs.l1Frame.path.filePath) . runErrorNoCallStack @Skipped $ do
    start <- currentTime
    dateBeg <- requireKey "DATE-BEG" frameInputs.l1Frame.header
    let path = Fits.outputL2Fits t.proposalId t.inversionId dateBeg
    guardAlreadyExists path

    log Debug $ dump "FRAME start" path.filePath
    log Debug $ dump " - inputs.profiles.arms" (length frameInputs.profiles.arms)
    frame <- Fits.generateL2FrameFits start t.inversionId slice frameInputs
    let fits = Fits.frameToFits frame
    -- log Debug $ dump " - fits" fits
    Scratch.writeFile path $ Fits.encodeL2 fits
    log Debug $ dump " - wroteframe" path.filePath

    now <- currentTime
    send $ TaskModStatus @GenFits t (addComplete now)

    pure $ Fits.frameMeta frame (filenameL2Fits t.inversionId dateBeg)
 where
  guardAlreadyExists path = do
    alreadyExists <- Scratch.pathExists path
    when alreadyExists $ do
      inp <- Scratch.readFile path
      res <- runErrorNoCallStack @ParseError $ fitsDecode inp
      case res of
        -- file is bad, do not skip, regenerate
        Left _ -> pure ()
        Right fits -> do
          log Debug $ dump "SKIP frame" (path.filePath, length fits.extensions)
          send $ TaskModStatus @GenFits t addSkipped
          throwError ()

  addComplete :: UTCTime -> GenFitsStatus -> GenFitsStatus
  addComplete now = \case
    GenFrames{started, skipped, complete, total} ->
      GenFrames{started, skipped, complete = complete + 1, total, throughput = throughputPerSecond (complete + 1) now started}
    gs -> gs

  addSkipped :: GenFitsStatus -> GenFitsStatus
  addSkipped = \case
    GenFrames{started, skipped, complete, total, throughput} ->
      GenFrames{started, skipped = skipped + 1, complete, total, throughput}
    gs -> gs

  throughputPerSecond :: Int -> UTCTime -> UTCTime -> Float
  throughputPerSecond complete now started =
    fromIntegral complete / realToFrac (diffUTCTime now started)


delay :: (Concurrent :> es) => Eff es ()
delay = threadDelay $ 2 * 1000 * 1000

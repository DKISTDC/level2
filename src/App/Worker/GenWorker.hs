{-# LANGUAGE ScopedTypeVariables #-}

module App.Worker.GenWorker where

import App.Effect.Transfer qualified as Transfer
import App.Worker.CPU (CPUWorkers (..))
import App.Worker.CPU qualified as CPU
import App.Worker.Generate as Gen
import Control.Monad (zipWithM)
import Control.Monad.Catch (catch)
import Control.Monad.Loops
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Time.Clock (diffUTCTime)
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
import NSO.Data.Datasets qualified as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Scratch as Scratch
import NSO.Image.Asdf as Asdf
import NSO.Image.Blanca qualified as Blanca
import NSO.Image.Files qualified as Files
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Meta qualified as Meta
import NSO.Image.Fits.Quantity (QuantityError, decodeQuantitiesFrames)
import NSO.Image.Headers.Parse (requireKey)
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.Types.Frame (Arms (..), Frames (..))
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Telescope.Data.Parser (ParseError)
import Telescope.Fits (BinTableHDU (..))


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
  => GenFits
  -> Eff es ()
fitsTask task = do
  res <- runErrorNoCallStack $ catch workWithError onCaughtError
  either (failed task.inversionId) pure res
 where
  workWithError :: Eff (Error GenerateError : es) ()
  workWithError = runGenerateError $ do
    log Debug "START"
    send $ TaskSetStatus task GenStarted

    -- Load Metadata ----------------
    let u = Files.inversionUploads $ Files.blanca task.proposalId task.inversionId
    log Debug $ dump "InvResults" u.quantities
    log Debug $ dump "InvProfile" u.profileFit
    log Debug $ dump "OrigProfile" u.profileOrig
    slice <- sliceMeta u

    inv <- loadInversion task.inversionId
    ds <- Datasets.find $ Datasets.ByIds inv.datasets

    -- Download Canonical Dataset
    log Debug $ dump "Meta Complete" slice
    downloadL1Frames task inv ds

    log Debug $ dump "Downloaded L1" (fmap Files.dataset ds)
    dc <- requireCanonicalDataset slice ds

    log Debug $ dump "Canonical Dataset:" dc.datasetId

    quantities <- decodeQuantitiesFrames =<< readFile u.quantities
    log Debug $ dump "Quantities" ()

    fitHDUs <- Blanca.decodeProfileHDUs =<< readFile u.profileFit
    arms <- Blanca.decodeArmWavMeta fitHDUs
    log Debug $ dump "Profile Arms " arms

    profileFit <- Blanca.decodeProfileArms arms fitHDUs
    log Debug $ dump "Profile Fit " profileFit

    origHDUs <- Blanca.decodeProfileHDUs =<< readFile u.profileOrig
    profileOrig <- Blanca.decodeProfileArms arms origHDUs
    log Debug $ dump "Profile Orig" profileOrig

    l1 <- Gen.canonicalL1Frames (Files.dataset dc)
    log Debug $ dump "Frames" (length quantities, armFramesLength profileFit, armFramesLength profileOrig, length l1)

    gfs <- Gen.collateFrames quantities arms profileFit profileOrig l1

    now <- currentTime
    send $ TaskSetStatus task $ GenFrames{started = now, skipped = 0, complete = 0, total = length gfs, throughput = 0}

    -- Generate them in parallel with N = available CPUs
    metas <- CPU.parallelize $ fmap (workFrame task slice) gfs

    log Debug $ dump "SKIPPED: " (length $ NE.filter isNothing metas.frames)
    log Debug $ dump "WRITTEN: " (length $ NE.filter isJust metas.frames)
    Inversions.setGeneratedFits task.inversionId


downloadL1Frames
  :: (IOE :> es, Log :> es, Error GlobusError :> es, Inversions :> es, Datasets :> es, Concurrent :> es, Tasks GenFits :> es, Time :> es, Error GenerateError :> es, Reader (Token Access) :> es, Scratch :> es, Globus :> es)
  => GenFits
  -> Inversion
  -> [Dataset]
  -> Eff es ()
downloadL1Frames task inv ds = do
  case inv.generate.transfer of
    Just _ -> pure ()
    Nothing -> transfer
 where
  transfer = do
    taskId <- Transfer.initScratchDatasets ds
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
  -> L2FrameInputs
  -> Eff es (Maybe L2FitsMeta)
workFrame t slice frameInputs = runGenerateError $ do
  start <- currentTime
  DateTime dateBeg <- requireKey "DATE-BEG" frameInputs.l1Frame.header
  let path = Fits.outputL2Fits t.proposalId t.inversionId dateBeg
  alreadyExists <- Scratch.pathExists path

  if alreadyExists
    then do
      log Debug $ dump "SKIP frame" path.filePath
      send $ TaskModStatus @GenFits t addSkipped
      pure Nothing
    else do
      log Debug $ dump "FRAME start" path.filePath
      log Debug $ dump " - inputs.profiles.arms" (length frameInputs.profiles.arms)
      frame <- Fits.generateL2FrameFits start t.inversionId slice frameInputs
      let fits = Fits.frameToFits frame
      -- log Debug $ dump " - fits" fits
      Scratch.writeFile path $ Fits.encodeL2 fits
      log Debug $ dump " - wroteframe" path.filePath

      now <- currentTime
      send $ TaskModStatus @GenFits t (addComplete now)

      pure $ Just $ Fits.frameMeta frame (filenameL2Fits t.inversionId dateBeg)
 where
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

    -- Load Metadata
    let u = Files.inversionUploads $ Files.blanca t.proposalId t.inversionId
    slice <- sliceMeta u

    inv <- loadInversion t.inversionId
    ds <- Datasets.find $ Datasets.ByIds inv.datasets
    dc <- requireCanonicalDataset slice ds
    log Debug $ dump "Canonical Dataset:" dc.datasetId

    fitHDUs <- Blanca.decodeProfileHDUs =<< readFile u.profileFit
    -- origHDUs <- Blanca.decodeProfileHDUs =<< readFile u.profileOrig

    arms <- Blanca.decodeArmWavMeta fitHDUs
    -- profileFit :: Arms [ProfileImage Fit] <- Blanca.decodeProfileArms arms fitHDUs
    -- profileOrig :: Arms [ProfileImage Original] <- Blanca.decodeProfileArms arms origHDUs

    l1fits <- Gen.canonicalL1Frames (Files.dataset dc)
    l1trans <- Gen.readLevel1Asdf (Files.dataset dc)

    (metas :: Frames L2FitsMeta) <- requireMetas t.proposalId t.inversionId slice arms l1fits

    log Debug $ dump "metas" (length metas)

    now <- currentTime
    let tree = asdfDocument inv.inversionId dc ds now l1trans $ Frames $ NE.sort metas.frames
    let path = Asdf.outputL2AsdfPath inv.proposalId inv.inversionId
    output <- Asdf.encodeL2 tree
    Scratch.writeFile path output

    log Debug " - Generated ASDF"
    log Debug " - done"
    Inversions.setGeneratedAsdf t.inversionId


requireMetas
  :: forall es
   . (Error GenerateError :> es, Scratch :> es, Error ParseError :> es, Error ProfileError :> es, Error QuantityError :> es)
  => Id Proposal
  -> Id Inversion
  -> SliceXY
  -> Arms ArmWavMeta
  -> [BinTableHDU]
  -> Eff es (Frames L2FitsMeta)
requireMetas propId invId slice arms l1fits = do
  metas <- loadMetas
  case metas of
    (m : ms) -> pure $ Frames (m :| ms)
    _ -> throwError MissingL2Fits
 where
  loadMetas :: Eff es [L2FitsMeta]
  loadMetas = do
    paths <- l2FramePaths propId invId
    zipWithM loadL2FitsMeta paths l1fits

  loadL2FitsMeta :: Path' Filename L2FrameFits -> BinTableHDU -> Eff es L2FitsMeta
  loadL2FitsMeta path l1 = do
    fits <- readLevel2Fits propId invId path
    Meta.frameMetaFromL2Fits path slice arms l1 fits


failed :: (Log :> es, Inversions :> es) => Id Inversion -> GenerateError -> Eff es ()
failed iid err = do
  log Err $ dump "GenerateError" err
  Inversions.setError iid (cs $ show err)


onCaughtError :: IOError -> Eff (Error GenerateError : es) a
onCaughtError e = do
  throwError $ GenIOError e

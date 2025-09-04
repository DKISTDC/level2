module App.Worker.Generate.Inputs where

import App.Effect.Transfer (Transfer, runTransfer)
import App.Effect.Transfer qualified as Transfer
import App.Worker.CPU (CPUWorkers (..))
import App.Worker.CPU qualified as CPU
import App.Worker.Generate.Collate (armFramesLength, collateFrames)
import App.Worker.Generate.Error (FetchError (..), GenerateError (..), generateFailed, runGenerateError)
import App.Worker.Generate.Error qualified as Gen
import App.Worker.Generate.Level1 (canonicalL1Frames, fitsDecode, isFits, readLevel1Asdf, requireCanonicalDataset)
import Control.Monad.Catch (catch)
import Control.Monad.Loops
import Data.Either (isRight)
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
import NSO.Image.Blanca (BlancaError)
import NSO.Image.Blanca qualified as Blanca
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError, decodeQuantitiesFrames)
import NSO.Image.Headers.Parse (requireKey, runParseError)
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.L1Input (L1Fits (..))
import NSO.Image.Types.Frame (Arms (..), Frames (..))
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import System.FilePath (takeFileName)
import Telescope.Data.Parser (ParseError)
import Telescope.Fits (Fits (..))
import Telescope.Fits qualified as Fits


loadInversion :: (Inversions :> es, Error GenerateError :> es) => Id Inversion -> Eff es Inversion
loadInversion ii = do
  is <- send $ Inversions.ById ii
  case is of
    [inv] -> pure inv
    _ -> throwError $ MissingInversion ii


loadInputs
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
     , GenRandom :> es
     , Error GenerateError :> es
     , Error FetchError :> es
     , IOE :> es
     )
  => Id Proposal
  -> Id Inversion
  -> Eff es (InversionFiles Identity File, SliceXY, Inversion, NonEmpty Dataset)
loadInputs propId invId = do
  log Debug "Load Inputs"

  -- Load Metadata ----------------
  let u = Files.inversionFiles $ Files.blancaInput propId invId
  log Debug $ dump "InvResults" u.quantities
  log Debug $ dump "InvProfile" u.profileFit
  log Debug $ dump "OrigProfile" u.profileOrig
  slice <- sliceMeta u
  log Debug $ dump "Slice" slice

  inv <- loadInversion invId
  dss :: [Dataset] <- Datasets.find $ Datasets.ByIds inv.datasets
  ds :: NonEmpty Dataset <- requireDatasets inv.datasets dss
  pure (u, slice, inv, ds)
 where
  sliceMeta :: (Error GenerateError :> es, Scratch :> es) => InversionFiles Identity File -> Eff es SliceXY
  sliceMeta u = do
    inp <- Scratch.readFile u.profileFit
    f :: Fits <- Fits.decode inp
    slice :: SliceXY <- runParseError InvalidSliceKeys $ requireSlice f.primaryHDU.header
    pure slice
   where
    requireSlice h = do
      pixelsPerBin <- requireKey "DESR-BIN" h
      fiducialArmId <- requireKey "DESR-FID" h
      pure $ SliceXY{pixelsPerBin, fiducialArmId}

  requireDatasets ids = \case
    [] -> throwError $ NoDatasets ids
    (d : ds) -> pure $ d :| ds


data DownloadComplete = DownloadComplete


loadFrameInputs
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
     , GenRandom :> es
     , Error FetchError :> es
     , Error QuantityError :> es
     , Error BlancaError :> es
     , Error GenerateError :> es
     )
  => Id Proposal
  -> Id Inversion
  -> InversionFiles Identity File
  -> SliceXY
  -> Inversion
  -> NonEmpty Dataset
  -> DownloadComplete
  -> Eff es (Frames L2FrameInputs)
loadFrameInputs propId invId files slice inv datasets _ = do
  log Debug $ dump "Downloaded L1" (fmap Files.dataset datasets)
  dc <- requireCanonicalDataset slice $ NE.toList datasets

  log Debug $ dump "Canonical Dataset:" dc.datasetId

  quantities <- decodeQuantitiesFrames =<< readFile files.quantities
  log Debug $ dump "Quantities" ()

  fitHDUs <- Blanca.decodeProfileHDUs =<< readFile files.profileFit
  arms <- Blanca.decodeArmWavMeta fitHDUs
  log Debug $ dump "Profile Arms " arms

  profileFit <- Blanca.decodeProfileArms arms fitHDUs
  log Debug $ dump "Profile Fit" (length profileFit.arms)

  origHDUs <- Blanca.decodeProfileHDUs =<< readFile files.profileOrig
  profileOrig <- Blanca.decodeProfileArms arms origHDUs
  log Debug $ dump "Profile Orig" (length profileOrig.arms)

  l1 <- canonicalL1Frames (Files.dataset dc)
  log Debug $ dump "Frames" (length quantities, armFramesLength profileFit, armFramesLength profileOrig, length l1)

  collateFrames quantities arms profileFit profileOrig l1

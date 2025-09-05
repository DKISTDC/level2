module App.Worker.Generate.Inputs where

import App.Worker.Generate.Decode
import App.Worker.Generate.Error (FetchError (..), FrameSizes (..), GenerateError (..))
import App.Worker.Generate.Level1 (Canonical (..), canonicalL1Frames)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import NSO.Data.Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files as Files
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch as Scratch
import NSO.Image.Blanca (BlancaError)
import NSO.Image.Blanca qualified as Blanca
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError, QuantityImage (..), decodeQuantitiesFrames)
import NSO.Image.Headers.Parse (requireKey, runParseError)
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.L1Input (L1Fits (..))
import NSO.Image.Types.Frame (Arms (..), Depth, Frames (..), SlitX)
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import Telescope.Fits (Fits (..))
import Telescope.Fits qualified as Fits


loadInversion :: (Inversions :> es, Error GenerateError :> es) => Id Inversion -> Eff es Inversion
loadInversion ii = do
  is <- send $ Inversions.ById ii
  case is of
    [inv] -> pure inv
    _ -> throwError $ MissingInversion ii


inversionFiles :: (Log :> es) => Id Proposal -> Id Inversion -> Eff es (InversionFiles Identity File)
inversionFiles propId invId = do
  let fs = Files.inversionFiles $ Files.blancaInput propId invId
  log Debug $ dump "InvResults" fs.quantities
  log Debug $ dump "InvProfile" fs.profileFit
  log Debug $ dump "OrigProfile" fs.profileOrig
  pure fs


sliceMeta :: (Error GenerateError :> es, Scratch :> es, Log :> es) => InversionFiles Identity File -> Eff es SliceXY
sliceMeta u = do
  f <- readFits u.profileFit
  slice :: SliceXY <- runParseError InvalidSliceKeys $ requireSlice f.primaryHDU.header
  log Debug $ dump "Slice" slice
  pure slice
 where
  requireSlice h = do
    pixelsPerBin <- requireKey "DESR-BIN" h
    fiducialArmId <- requireKey "DESR-FID" h
    pure $ SliceXY{pixelsPerBin, fiducialArmId}


data DownloadComplete = DownloadComplete


loadFrameInputs
  :: forall es
   . ( Log :> es
     , Scratch :> es
     , Error FetchError :> es
     , Error QuantityError :> es
     , Error BlancaError :> es
     , Error GenerateError :> es
     )
  => InversionFiles Identity File
  -> Canonical Dataset
  -> DownloadComplete
  -> Eff es (Frames L2FrameInputs)
loadFrameInputs files (Canonical dc) _ = do
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


collateFrames :: (Error GenerateError :> es) => [Quantities (QuantityImage [SlitX, Depth])] -> Arms ArmWavMeta -> Arms (Frames (ProfileImage Fit)) -> Arms (Frames (ProfileImage Original)) -> [L1Fits] -> Eff es (Frames L2FrameInputs)
collateFrames qs metas pfs pos l1s = do
  unless allFramesEqual $ throwError $ MismatchedFrames frameSizes
  let pframes :: Frames (Arms ArmProfileImages) = Blanca.collateFramesArms metas pfs pos
  frames $ L.zipWith3 L2FrameInputs qs (NE.toList pframes.frames) l1s
 where
  frames [] = throwError $ NoFrames frameSizes
  frames (f : fs) = pure $ Frames $ f :| fs

  allFramesEqual :: Bool
  allFramesEqual =
    frameSizes.fit == frameSizes.quantities
      && frameSizes.original == frameSizes.quantities
      && (frameSizes.l1 - frameSizes.quantities <= 1) -- one frame may be dropped, always at the end
  frameSizes :: FrameSizes
  frameSizes =
    FrameSizes
      { quantities = length qs
      , fit = armFramesLength pfs
      , original = armFramesLength pos
      , l1 = length l1s
      }


armFramesLength :: Arms (Frames (ProfileImage fit)) -> Int
armFramesLength as = length . head $ as.arms

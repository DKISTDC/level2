{-# LANGUAGE ScopedTypeVariables #-}

module App.Worker.Generate.Asdf where

import App.Effect.Transfer (Transfer, runTransfer)
import App.Worker.Generate as Gen
import App.Worker.Generate.Error
import App.Worker.Generate.Level1 (canonicalL1Frames, fitsDecode, isFits, readLevel1Asdf, requireCanonicalDataset)
import Control.Monad (zipWithM)
import Control.Monad.Catch (catch)
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Globus (Globus, Token, Token' (Access))
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Datasets qualified as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files as Files
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch as Scratch
import NSO.Image.Asdf as Asdf
import NSO.Image.Blanca (BlancaError)
import NSO.Image.Blanca qualified as Blanca
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Meta qualified as Meta
import NSO.Image.Fits.Quantity (QuantityError)
import NSO.Image.Headers.Types (SliceXY (..))
import NSO.Image.L1Input (L1Fits (..))
import NSO.Image.Types.Frame (Arms (..), Frames (..))
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import System.FilePath (takeFileName)
import Telescope.Asdf.Error (AsdfError)
import Telescope.Fits (Fits)


generateAsdf
  :: forall es
   . ( Reader (Token Access) :> es
     , Globus :> es
     , Datasets :> es
     , Inversions :> es
     , Time :> es
     , Scratch :> es
     , Log :> es
     , Concurrent :> es
     , IOE :> es
     , Error FetchError :> es
     , Error GenerateError :> es
     , Error BlancaError :> es
     , Error QuantityError :> es
     , Error ProfileError :> es
     , Error AsdfError :> es
     )
  => InversionFiles Identity File
  -> Inversion
  -> NonEmpty Dataset
  -> SliceXY
  -> Eff es ()
generateAsdf files inv datasets slice = do
  logContext ("ASDF " <> cs inv.inversionId.fromId) $ do
    log Debug "started"

    let ds = NE.toList datasets
    dc <- requireCanonicalDataset slice ds
    log Debug $ dump "Canonical Dataset:" dc.datasetId

    fitHDUs <- Blanca.decodeProfileHDUs =<< readFile files.profileFit
    -- origHDUs <- Blanca.decodeProfileHDUs =<< readFile u.profileOrig

    arms <- Blanca.decodeArmWavMeta fitHDUs
    -- profileFit :: Arms [ProfileImage Fit] <- Blanca.decodeProfileArms arms fitHDUs
    -- profileOrig :: Arms [ProfileImage Original] <- Blanca.decodeProfileArms arms origHDUs

    log Debug "Got Blanca"
    l1fits <- canonicalL1Frames (Files.dataset dc)
    log Debug "Got Gfits"
    l1asdf <- readLevel1Asdf (Files.dataset dc)
    log Debug "Got L1Asdf"

    (metas :: Frames L2FitsMeta) <- requireMetas inv.proposalId inv.inversionId slice arms l1fits

    log Debug $ dump "metas" (length metas)

    now <- currentTime
    let tree = asdfDocument inv.inversionId dc ds slice.pixelsPerBin now l1asdf $ Frames $ NE.sort metas.frames
    let path = Files.outputL2AsdfPath inv.proposalId inv.inversionId
    output <- Asdf.encodeL2 tree
    Scratch.writeFile path output

    log Debug $ dump "WROTE" (takeFileName path.filePath)
    Inversions.setGeneratedAsdf inv.inversionId


requireMetas
  :: forall es
   . (Error GenerateError :> es, Scratch :> es, Error ProfileError :> es, Error QuantityError :> es)
  => Id Proposal
  -> Id Inversion
  -> SliceXY
  -> Arms ArmWavMeta
  -> [L1Fits]
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
    zipWithM (loadL2FitsMeta propId invId slice arms) paths l1fits


loadL2FitsMeta
  :: (Error GenerateError :> es, Scratch :> es, Error ProfileError :> es, Error QuantityError :> es)
  => Id Proposal
  -> Id Inversion
  -> SliceXY
  -> Arms ArmWavMeta
  -> Path Scratch Filename L2FrameFits
  -> L1Fits
  -> Eff es L2FitsMeta
loadL2FitsMeta propId invId slice arms path l1 = do
  fits <- readLevel2Fits propId invId path
  runErrorNoCallStackWith (throwError . ParseError path.filePath) $ Meta.frameMetaFromL2Fits path slice arms l1 fits


readLevel2Fits :: forall es. (Scratch :> es, Error GenerateError :> es) => Id Proposal -> Id Inversion -> Path Scratch Filename L2FrameFits -> Eff es Fits
readLevel2Fits pid iid path = do
  let dir = Files.outputL2Dir pid iid
  inp <- send $ Scratch.ReadFile $ filePath dir path
  runErrorNoCallStackWith (throwError . ParseError path.filePath) $ fitsDecode inp


l2FramePaths :: (Scratch :> es) => Id Proposal -> Id Inversion -> Eff es [Path Scratch Filename L2FrameFits]
l2FramePaths pid iid = do
  let dir = Files.outputL2Dir pid iid
  fmap (fmap (\p -> Path p.filePath)) $ filter isFits <$> Scratch.listDirectory dir

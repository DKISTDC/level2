module App.Worker.Generate.Level1 where

import App.Worker.Generate.Decode
import App.Worker.Generate.Error (FetchError (..), GenerateError (..))
import Data.List qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import NSO.Data.Datasets as Datasets
import NSO.Files
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch qualified as Scratch
import NSO.Image.GWCS.L1GWCS
import NSO.Image.Headers.Parse (requireKey)
import NSO.Image.Headers.Types (SliceXY (..), VISPArmId (..))
import NSO.Image.L1Input
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import System.FilePath (takeExtensions)
import Telescope.Asdf qualified as Asdf
import Telescope.Asdf.Error (AsdfError)
import Telescope.Data.Parser (ParseError)
import Telescope.Fits as Fits


newtype Canonical a = Canonical {value :: a}


canonicalDataset :: (Datasets :> es, Error FetchError :> es, Error GenerateError :> es, Scratch :> es, Log :> es) => SliceXY -> [Id Dataset] -> Eff es (Canonical Dataset)
canonicalDataset slice ids = do
  dss :: [Dataset] <- Datasets.find $ Datasets.ByIds ids
  when (null dss) $ do
    throwError $ NoDatasets ids
  dc <- requireCanonicalDataset slice dss
  log Debug $ dump "Canonical Dataset:" dc.value.datasetId
  pure dc


requireCanonicalDataset :: (Error FetchError :> es, Error GenerateError :> es, Scratch :> es, Log :> es) => SliceXY -> [Dataset] -> Eff es (Canonical Dataset)
requireCanonicalDataset slice ds = do
  vas :: [VISPArmId] <- mapM datasetVISPArmId ds
  let canon = L.find ((== slice.fiducialArmId) . snd) $ zip ds vas
  case canon of
    Nothing -> throwError (NoCanonicalDataset $ fmap (.datasetId) ds)
    Just (d, _) -> pure $ Canonical d


datasetVISPArmId :: (Error FetchError :> es, Error GenerateError :> es, Scratch :> es, Log :> es) => Dataset -> Eff es VISPArmId
datasetVISPArmId d = do
  f <- sampleIntensityFrame d
  frameArmId f
 where
  sampleIntensityFrame d' = do
    fs <- allL1IntensityFrames d'.primaryProposalId d'.datasetId
    pure $ head fs

  frameArmId f = do
    let path = Files.dataset d
    fits :: L1Fits <- readLevel1File path f
    res <- runErrorNoCallStack @ParseError $ requireKey "VSPARMID" fits.header
    case res of
      Left err -> throwError (FetchParse path.filePath err)
      Right a -> pure a


-- | read all downloaded files in the L1 scratch directory
canonicalL1Frames :: forall es. (Log :> es, Error FetchError :> es, Error GenerateError :> es, Scratch :> es) => Id Proposal -> Id Dataset -> Eff es (NonEmpty L1Fits)
canonicalL1Frames propId dsetId = do
  let fdir = Files.dataset' propId dsetId
  -- VSPARMID, see datasetVISPArmId and requireCanonicalDataset
  fs <- allL1IntensityFrames propId dsetId
  mapM (readLevel1File fdir) fs


allL1IntensityFrames :: (Scratch :> es, Error FetchError :> es) => Id Proposal -> Id Dataset -> Eff es (NonEmpty L1Frame)
allL1IntensityFrames propId dsetId = do
  fnames <- send $ Scratch.ListDirectory (Files.dataset' propId dsetId)
  let fs = L.sort $ mapMaybe runParseFileName $ filter isL1IntensityFile fnames
  case fs of
    [] -> throwError (MissingFrames dsetId)
    (f : fs') -> pure $ f :| fs'
 where
  isL1IntensityFile :: Path Scratch Filename Dataset -> Bool
  isL1IntensityFile (Path f) =
    -- VISP_2023_05_01T19_00_59_515_00630200_V_AOPPO_L1.fits
    isFits (Path f) && "_I_" `L.isInfixOf` f


readLevel1File :: forall es. (Scratch :> es, Error FetchError :> es) => Path Scratch Dir Dataset -> L1Frame -> Eff es L1Fits
readLevel1File dir frame = do
  let path = filePath dir frame.file
  fits <- readFits path
  case fits.extensions of
    [BinTable b] -> pure $ L1Fits path b.header
    _ -> throwError $ MissingL1HDU frame.file.filePath


readLevel1Asdf :: (Scratch :> es, IOE :> es, Error FetchError :> es) => Path Scratch Dir Dataset -> Eff es L1Asdf
readLevel1Asdf dir = do
  files <- Scratch.listDirectory dir
  case filter Files.isAsdf files of
    [asdfFile] -> do
      inp <- send $ Scratch.ReadFile $ filePath dir asdfFile
      res <- runErrorNoCallStack @AsdfError $ Asdf.decode @L1Asdf inp
      case res of
        Left e -> throwError $ L1AsdfParse e
        Right a -> pure a
    _ -> throwError $ MissingL1Asdf dir.filePath


isFits :: Path s Filename a -> Bool
isFits (Path f) =
  takeExtensions f == ".fits"

module App.Worker.Generate where

import Control.Exception (Exception)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import NSO.Data.Datasets as Datasets
import NSO.Data.Scratch (Scratch)
import NSO.Data.Scratch qualified as Scratch
import NSO.Image.Blanca (BlancaError (..))
import NSO.Image.Blanca as Blanca (collateFramesArms)
import NSO.Image.Files (UploadFiles (..))
import NSO.Image.Files qualified as Files
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError, QuantityImage)
import NSO.Image.Headers.Parse (requireKey, runParseError)
import NSO.Image.Headers.Types (SliceXY (..), VISPArmId (..))
import NSO.Image.L1Input
import NSO.Image.Primary (PrimaryError)
import NSO.Image.Types.Axes (Depth, SlitX)
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import Network.Globus qualified as Globus
import System.FilePath (takeExtensions)
import Telescope.Asdf.Error (AsdfError)
import Telescope.Data.Parser (ParseError)
import Telescope.Fits as Fits


collateFrames :: (Error GenerateError :> es) => [Quantities (QuantityImage [SlitX, Depth])] -> Arms ArmWavMeta -> Arms (NonEmpty (ProfileImage Fit)) -> Arms (NonEmpty (ProfileImage Original)) -> [BinTableHDU] -> Eff es (NonEmpty L2FrameInputs)
collateFrames qs metas pfs pos ts = do
  unless allFramesEqual $ throwError $ MismatchedFrames frameSizes
  let frameArms :: NonEmpty (Arms (Profile ProfileImage)) = Blanca.collateFramesArms metas pfs pos
  frames $ L.zipWith3 L2FrameInputs qs (NE.toList frameArms) ts
 where
  frames [] = throwError $ NoFrames frameSizes
  frames (f : fs) = pure $ f :| fs

  allFramesEqual :: Bool
  allFramesEqual =
    frameSizes.fit == frameSizes.quantities
      && frameSizes.original == frameSizes.quantities
      && (frameSizes.l1 - frameSizes.quantities <= 1) -- one frame may be dropped
  frameSizes :: FrameSizes
  frameSizes =
    FrameSizes
      { quantities = length qs
      , fit = armFramesLength pfs
      , original = armFramesLength pos
      , l1 = length ts
      }


armFramesLength :: Arms (NonEmpty (ProfileImage fit)) -> Int
armFramesLength as = length . head $ as.arms


data FrameSizes = FrameSizes {quantities :: Int, fit :: Int, original :: Int, l1 :: Int}
  deriving (Show, Eq)


requireCanonicalDataset :: (Error FetchError :> es, Scratch :> es, Log :> es) => SliceXY -> [Dataset] -> Eff es Dataset
requireCanonicalDataset slice ds = do
  vas :: [VISPArmId] <- mapM datasetVISPArmId ds
  let canon = L.find ((== slice.fiducialArmId) . snd) $ zip ds vas
  maybe (throwError (NoCanonicalDataset $ fmap (.datasetId) ds)) (pure . fst) canon


datasetVISPArmId :: (Error FetchError :> es, Scratch :> es, Log :> es) => Dataset -> Eff es VISPArmId
datasetVISPArmId d = do
  f <- sampleIntensityFrame d
  frameArmId f
 where
  sampleIntensityFrame d' = do
    fs <- allL1IntensityFrames (Files.dataset d')
    case fs of
      [] -> throwError (MissingFrames d'.datasetId)
      (f : _) -> pure f

  frameArmId f = do
    hdu :: BinTableHDU <- readLevel1File (Files.dataset d) f
    res <- runErrorNoCallStack @ParseError $ requireKey "VSPARMID" hdu.header
    case res of
      Left err -> throwError (FetchParse err)
      Right a -> pure a


-- | read all downloaded files in the L1 scratch directory
canonicalL1Frames :: forall es. (Log :> es, Error FetchError :> es, Scratch :> es) => Path' Dir Dataset -> Eff es [BinTableHDU]
canonicalL1Frames fdir = do
  -- VSPARMID, see datasetVISPArmId and requireCanonicalDataset
  fs <- allL1IntensityFrames fdir
  mapM (readLevel1File fdir) fs


allL1IntensityFrames :: (Scratch :> es) => Path' Dir Dataset -> Eff es [L1Frame]
allL1IntensityFrames dir = do
  fs <- send $ Scratch.ListDirectory dir
  pure $ L.sort $ mapMaybe runParseFileName $ filter isL1IntensityFile fs
 where
  isL1IntensityFile :: Path' Filename Dataset -> Bool
  isL1IntensityFile (Path f) =
    -- VISP_2023_05_01T19_00_59_515_00630200_V_AOPPO_L1.fits
    isFits (Path f) && "_I_" `L.isInfixOf` f


readLevel1File :: forall es. (Scratch :> es, Log :> es, Error FetchError :> es) => Path' Dir Dataset -> L1Frame -> Eff es BinTableHDU
readLevel1File dir frame = do
  inp <- send $ Scratch.ReadFile $ filePath dir frame.file
  fits <- Fits.decode inp
  case fits.extensions of
    [BinTable b] -> pure b
    _ -> throwError $ MissingL1HDU frame.file.filePath


readLevel2Fits :: forall es. (Scratch :> es) => Id Proposal -> Id Inversion -> Path' Filename L2FrameFits -> Eff es Fits
readLevel2Fits pid iid path = do
  let dir = Files.outputL2Dir pid iid
  inp <- send $ Scratch.ReadFile $ filePath dir path
  Fits.decode inp


l2FramePaths :: (Scratch :> es) => Id Proposal -> Id Inversion -> Eff es [Path' Filename L2FrameFits]
l2FramePaths pid iid = do
  let dir = Files.outputL2Dir pid iid
  fmap (fmap (\p -> Path p.filePath)) $ filter isFits <$> Scratch.listDirectory dir


data FetchError
  = NoCanonicalDataset [Id Dataset]
  | MissingFrames (Id Dataset)
  | MissingL1HDU FilePath
  | FetchParse ParseError
  deriving (Show, Exception, Eq)


isFits :: Path' Filename a -> Bool
isFits (Path f) =
  takeExtensions f == ".fits"


sliceMeta :: (Error GenerateError :> es, Scratch :> es) => UploadFiles File -> Eff es SliceXY
sliceMeta u = do
  inp <- Scratch.readFile u.profileFit
  f :: Fits <- decode inp
  slice :: SliceXY <- runParseError InvalidSliceKeys $ requireSlice f.primaryHDU.header
  pure slice
 where
  requireSlice h = do
    pixelsPerBin <- requireKey "DESR-BIN" h
    fiducialArmId <- requireKey "DESR-FID" h
    pure $ SliceXY{pixelsPerBin, fiducialArmId}


-- decodeProfileFit :: (Error ProfileError :> es) => BS.ByteString -> Eff es ProfileFit
-- decodeProfileFit inp = do
--   f <- decode inp
--   profile <- profileFrames f
--   slice <- runParseError InvalidSliceKeys $ requireSlice f.primaryHDU.header
--   pure $ ProfileFit{profile, slice}

-- isFrameUsed :: NonEmpty DateBegTimestamp -> BinTableHDU -> Bool
-- isFrameUsed dbts hdu = fromMaybe False $ do
--   String s <- Fits.lookup "DATE-BEG" hdu.header
--   d <- iso8601ParseM $ T.unpack s <> "Z"
--   pure $ DateBegTimestamp d `elem` dbts

-- readTimestamps :: (Scratch :> es, Log :> es, Error GenerateError :> es) => Path Timestamps -> Eff es (NonEmpty DateBegTimestamp)
-- readTimestamps f = do
--   inp <- send $ Scratch.ReadFile f
--   dts <- parseTimestampsFile inp
--   case dts of
--     [] -> throwError $ ZeroValidTimestamps f.filePath
--     (t : ts) -> pure $ t :| ts

-- parseTimestampsFile :: (Error GenerateError :> es) => ByteString -> Eff es [DateBegTimestamp]
-- parseTimestampsFile inp = do
--   mapM parseTimestamp $ filter (not . T.null) $ T.splitOn "\n" $ cs inp
--  where
--   parseTimestamp t = do
--     case iso8601ParseM $ T.unpack $ t <> "Z" of
--       Nothing -> throwError $ InvalidTimestamp t
--       Just u -> pure $ DateBegTimestamp u

----------------------------------------------------------------
-- Generate Error
-----------------------------------------------------------------

data GenerateError
  = L1TransferFailed (Id Globus.Task)
  | L1FetchError FetchError
  | MissingInversion (Id Inversion)
  | ProfileError ProfileError
  | QuantityError QuantityError
  | PrimaryError PrimaryError
  | ParseError ParseError
  | AsdfError AsdfError
  | BlancaError BlancaError
  | MismatchedFrames FrameSizes
  | NoFrames FrameSizes
  | GenIOError IOError
  | MissingL2Fits
  | InvalidSliceKeys ParseError
  deriving (Show, Exception)


type GenerateErrors es = (Error ParseError : Error ProfileError : Error QuantityError : Error FetchError : Error PrimaryError : Error AsdfError : Error BlancaError : es)


runGenerateError
  :: (Error GenerateError :> es)
  => Eff (GenerateErrors es) a
  -> Eff es a
runGenerateError =
  runErrorNoCallStackWith @BlancaError (throwError . BlancaError)
    . runErrorNoCallStackWith @AsdfError (throwError . AsdfError)
    . runErrorNoCallStackWith @PrimaryError (throwError . PrimaryError)
    . runErrorNoCallStackWith @FetchError (throwError . L1FetchError)
    . runErrorNoCallStackWith @QuantityError (throwError . QuantityError)
    . runErrorNoCallStackWith @ProfileError (throwError . ProfileError)
    . runErrorNoCallStackWith @ParseError (throwError . ParseError)

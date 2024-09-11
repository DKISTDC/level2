module App.Worker.Generate where

import App.Effect.Scratch (Scratch)
import App.Effect.Scratch qualified as Scratch
import Control.Exception (Exception)
import Data.List qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import NSO.Data.Datasets
import NSO.Data.Spectra (identifyLine)
import NSO.Image.Frame as Frame
import NSO.Image.Headers.Types (Depth, SliceXY (..), SlitX)
import NSO.Image.L1Input
import NSO.Image.Primary (PrimaryError)
import NSO.Image.Profile (Fit, Original, ProfileError, ProfileFrame)
import NSO.Image.Quantities (Quantities, QuantityError, QuantityImage)
import NSO.Prelude
import NSO.Types.InstrumentProgram (InstrumentProgram)
import NSO.Types.Inversion (Inversion)
import Network.Globus qualified as Globus
import System.FilePath (takeExtensions)
import Telescope.Asdf.Error (AsdfError)
import Telescope.Fits as Fits


collateFrames :: (Error GenerateError :> es) => [Quantities (QuantityImage [SlitX, Depth])] -> [ProfileFrame Fit] -> [ProfileFrame Original] -> [BinTableHDU] -> Eff es (NonEmpty L2FrameInputs)
collateFrames qs pfs pos ts
  | allFramesEqual = frames $ L.zipWith4 L2FrameInputs qs pfs pos ts
  | otherwise = throwError $ MismatchedFrames frameSizes
 where
  frames [] = throwError $ NoFrames frameSizes
  frames (f : fs) = pure $ f :| fs

  allFramesEqual :: Bool
  allFramesEqual =
    all (== length qs) $ allSizes frameSizes

  frameSizes :: FrameSizes
  frameSizes =
    FrameSizes
      { quantities = length qs
      , fit = length pfs
      , original = length pos
      , l1 = length ts
      }

  allSizes :: FrameSizes -> [Int]
  allSizes fs = [fs.quantities, fs.fit, fs.original, fs.l1]


data GenerateError
  = L1TransferFailed (Id Globus.Task)
  | L1FetchError FetchError
  | MissingInversion (Id Inversion)
  | ProfileError ProfileError
  | QuantityError QuantityError
  | PrimaryError PrimaryError
  | AsdfError AsdfError
  | MismatchedFrames FrameSizes
  | NoFrames FrameSizes
  | GenIOError IOError
  deriving (Show, Eq, Exception)


data FrameSizes = FrameSizes {quantities :: Int, fit :: Int, original :: Int, l1 :: Int}
  deriving (Show, Eq)


runGenerateError
  :: (Error GenerateError :> es)
  => Eff (Error ProfileError : Error QuantityError : Error FetchError : Error PrimaryError : Error AsdfError : es) a
  -> Eff es a
runGenerateError =
  runErrorNoCallStackWith @AsdfError (throwError . AsdfError)
    . runErrorNoCallStackWith @PrimaryError (throwError . PrimaryError)
    . runErrorNoCallStackWith @FetchError (throwError . L1FetchError)
    . runErrorNoCallStackWith @QuantityError (throwError . QuantityError)
    . runErrorNoCallStackWith @ProfileError (throwError . ProfileError)


requireCanonicalDataset :: (Error FetchError :> es, Datasets :> es) => Id InstrumentProgram -> Eff es Dataset
requireCanonicalDataset ip = do
  md <- findCanonicalDataset ip
  maybe (throwError (NoCanonicalDataset ip)) pure md
 where
  findCanonicalDataset :: (Datasets :> es) => Id InstrumentProgram -> Eff es (Maybe Dataset)
  findCanonicalDataset ip' = do
    ds <- send $ Query (ByProgram ip')
    pure $ L.find isCanonicalDataset ds


isCanonicalDataset :: Dataset -> Bool
isCanonicalDataset d =
  identifyLine d == Just FeI


-- | read all downloaded files in the L1 scratch directory
canonicalL1Frames :: forall es. (Log :> es, Error FetchError :> es, Scratch :> es) => Path' Dir Dataset -> SliceXY -> Eff es [BinTableHDU]
canonicalL1Frames fdir slice = do
  fs <- allL1Frames fdir
  frs <- mapM (readLevel1File fdir) fs
  pure $ drop slice.frameBeg $ take slice.frameEnd frs
 where
  allL1Frames :: Path' Dir Dataset -> Eff es [L1Frame]
  allL1Frames dir = do
    fs <- send $ Scratch.ListDirectory dir
    pure $ mapMaybe runParseFileName $ filter isL1IntensityFile fs

  -- VISP_2023_05_01T19_00_59_515_00630200_V_AOPPO_L1.fits
  isL1IntensityFile :: Path' Filename Dataset -> Bool
  isL1IntensityFile (Path f) =
    takeExtensions f == ".fits" && "_I_" `L.isInfixOf` f


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

readLevel1File :: forall es. (Scratch :> es, Log :> es, Error FetchError :> es) => Path' Dir Dataset -> L1Frame -> Eff es BinTableHDU
readLevel1File dir frame = do
  inp <- send $ Scratch.ReadFile $ filePath dir frame.file
  fits <- Fits.decode inp
  case fits.extensions of
    [BinTable b] -> pure b
    _ -> throwError $ MissingL1HDU frame.file.filePath


data FetchError
  = NoCanonicalDataset (Id InstrumentProgram)
  | MissingL1HDU FilePath
  deriving (Show, Exception, Eq)

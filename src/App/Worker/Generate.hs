module App.Worker.Generate where

import App.Effect.Scratch (Scratch)
import App.Effect.Scratch qualified as Scratch
import Control.Exception (Exception)
import Control.Monad (replicateM, void)
import Data.List qualified as L
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..), picosecondsToDiffTime)
import Data.Void (Void)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import NSO.Data.Datasets
import NSO.Data.Spectra (identifyLine)
import NSO.Image.Frame as Frame
import NSO.Image.Headers.Types (Depth, SliceXY (..), SlitX)
import NSO.Image.Primary (PrimaryError)
import NSO.Image.Profile (Fit, Original, ProfileError, ProfileFrame)
import NSO.Image.Quantities (Quantities, QuantityError, QuantityImage)
import NSO.Prelude
import NSO.Types.InstrumentProgram (InstrumentProgram)
import NSO.Types.Inversion (Inversion)
import Network.Globus qualified as Globus
import System.FilePath (takeExtensions)
import Telescope.Fits as Fits
import Text.Megaparsec hiding (ParseError, Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Read (readMaybe)


collateFrames :: (Error GenerateError :> es) => [Quantities (QuantityImage [SlitX, Depth])] -> [ProfileFrame Fit] -> [ProfileFrame Original] -> [BinTableHDU] -> Eff es [L2FrameInputs]
collateFrames qs pfs pos ts
  | allFramesEqual = pure $ L.zipWith4 L2FrameInputs qs pfs pos ts
  | otherwise = throwError $ MismatchedFrames frameSizes
 where
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
  | MismatchedFrames FrameSizes
  | GenIOError IOError
  deriving (Show, Eq, Exception)


data FrameSizes = FrameSizes {quantities :: Int, fit :: Int, original :: Int, l1 :: Int}
  deriving (Show, Eq)


runGenerateError
  :: (Error GenerateError :> es)
  => Eff (Error ProfileError : Error QuantityError : Error FetchError : Error PrimaryError : es) a
  -> Eff es a
runGenerateError =
  runErrorNoCallStackWith @PrimaryError (throwError . PrimaryError)
    . runErrorNoCallStackWith @FetchError (throwError . L1FetchError)
    . runErrorNoCallStackWith @QuantityError (throwError . QuantityError)
    . runErrorNoCallStackWith @ProfileError (throwError . ProfileError)


data L1Frame = L1Frame
  { file :: Path' Filename L1Frame
  , timestamp :: UTCTime
  , wavelength :: Wavelength Nm
  , stokes :: Stokes
  }
  deriving (Show)


newtype DateBegTimestamp = DateBegTimestamp {time :: UTCTime}
  deriving newtype (Eq, Show)


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


-- Filename Parser ----------------------------------------

type Parser = Parsec Void FilePath
type ParseErr = ParseErrorBundle FilePath Void


runParseFileName :: Path' Filename Dataset -> Maybe L1Frame
runParseFileName (Path f) =
  case runParser parseL1FileName f f of
    Left _ -> Nothing
    Right (u, w, s) -> pure $ L1Frame (Path f) u w s


-- is this a full path or a filename?
parseL1FileName :: Parser (UTCTime, Wavelength Nm, Stokes)
parseL1FileName = do
  -- VISP_2023_05_01T19_16_36_463_00630200_I_AOPPO_L1.fits
  void $ string "VISP"
  dt <- sep >> datetime
  wl <- sep >> wavelength
  s <- sep >> stokes
  pure (dt, wl, s)
 where
  date :: Parser Day
  date = do
    y <- decimal
    void $ string "_"
    m <- decimal
    void $ string "_"
    d <- decimal
    pure $ fromGregorian y m d

  time :: Parser DiffTime
  time = do
    h <- decimal
    void $ string "_"
    m <- decimal
    void $ string "_"
    s <- decimal
    void $ string "_"
    ms <- decimal
    let msTotal = ((h * 3600) + (m * 60) + s) * s2ms + ms :: Integer
    let picos = msTotal * ms2ps
    pure $ picosecondsToDiffTime picos
   where
    s2ms = 1000
    ms2ps = 1000000000

  datetime :: Parser UTCTime
  datetime = do
    d <- date
    void $ string "T"
    t <- time
    pure $ UTCTime d t

  stokes :: Parser Stokes
  stokes =
    (I <$ string "I")
      <|> (Q <$ string "Q")
      <|> (U <$ string "U")
      <|> (V <$ string "V")

  sep :: Parser ()
  sep = void $ string "_"

  wavelength :: Parser (Wavelength Nm)
  wavelength = do
    -- 00630200
    nms <- replicateM 5 digitChar
    pms <- replicateM 3 digitChar
    case (readMaybe nms, readMaybe pms) of
      (Just nm, Just pm) -> pure $ Wavelength $ nm + (pm / 1000)
      _ -> fail "Could not parse wavelength"


data FetchError
  = NoCanonicalDataset (Id InstrumentProgram)
  | MissingL1HDU FilePath
  deriving (Show, Exception, Eq)

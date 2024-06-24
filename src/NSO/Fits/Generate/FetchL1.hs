module NSO.Fits.Generate.FetchL1 where

import App.Globus as Globus
import App.Types
import Control.Monad (replicateM, void)
import Data.ByteString (ByteString)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..), picosecondsToDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Void (Void)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString
import Effectful.Reader.Dynamic
import NSO.Data.Datasets
import NSO.Data.Spectra (identifyLine)
import NSO.Fits.Generate.Error
import NSO.Fits.Generate.Headers.LiftL1
import NSO.Prelude
import NSO.Types.InstrumentProgram
import System.FilePath (takeExtensions, (</>))
import Telescope.Fits
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Read (readMaybe)


data L1Frame = L1Frame
  { file :: Path L1Frame
  , timestamp :: UTCTime
  , wavelength :: Wavelength Nm
  , stokes :: Stokes
  }
  deriving (Show)


data L1FrameDir


newtype DateBegTimestamp = DateBegTimestamp {text :: Text}
  deriving newtype (Eq, Show)


fetchCanonicalDataset
  :: (Datasets :> es, Error GenerateError :> es, Reader (Token Access) :> es, Reader (GlobusEndpoint App) :> es, Globus :> es)
  => Id InstrumentProgram
  -> Eff es (Id Task, Path L1FrameDir)
fetchCanonicalDataset ip = do
  md <- findCanonicalDataset ip
  d <- maybe (throwError (NoCanonicalDataset ip)) pure md
  transferCanonicalDataset d


findCanonicalDataset :: (Datasets :> es) => Id InstrumentProgram -> Eff es (Maybe Dataset)
findCanonicalDataset ip' = do
  ds <- send $ Query (ByProgram ip')
  pure $ L.find isCanonicalDataset ds


isCanonicalDataset :: Dataset -> Bool
isCanonicalDataset d =
  identifyLine d == Just FeI


transferCanonicalDataset :: (Globus :> es, Reader (Token Access) :> es, Reader (GlobusEndpoint App) :> es) => Dataset -> Eff es (Id Task, Path L1FrameDir)
transferCanonicalDataset d = do
  -- wait, we have to wait until the transfer finishes before scanning!
  (t, dir) <- Globus.initTransferDataset d
  pure (t, Path dir)


canonicalL1Frames :: (Error GenerateError :> es, FileSystem :> es, Reader (GlobusEndpoint App) :> es) => Path L1FrameDir -> [DateBegTimestamp] -> Eff es [BinTableHDU]
canonicalL1Frames fdir ts = do
  fs <- allL1Frames fdir
  ufs <- usedL1Frames fs
  mapM (readLevel1File . (.file)) ufs
 where
  -- VISP_2023_05_01T19_00_59_515_00630200_V_AOPPO_L1.fits
  allL1Frames :: (FileSystem :> es, Reader (GlobusEndpoint App) :> es) => Path L1FrameDir -> Eff es [L1Frame]
  allL1Frames (Path dir) = do
    g <- ask @(GlobusEndpoint App)
    fs <- fmap Path <$> listDirectory (g.mount </> dir)
    pure $ mapMaybe runParseFileName $ filter isL1File fs

  isL1File (Path f) = takeExtensions f == ".fits"

  usedL1Frames fs = do
    let ufs = filter (isFrameUsed ts) fs
    case ufs of
      [] -> throwError $ InvalidTimestamps (length ts)
      _ -> pure ufs


isFrameUsed :: [DateBegTimestamp] -> L1Frame -> Bool
isFrameUsed dbts f =
  frameTimestamp `elem` dbts
 where
  -- well, we want to associate it exactly, but we aren't looking at the header anymore
  frameTimestamp :: DateBegTimestamp
  frameTimestamp = DateBegTimestamp $ cs $ iso8601Show f.timestamp


readTimestamps :: (FileSystem :> es) => Path' Abs Timestamps -> Eff es [DateBegTimestamp]
readTimestamps (Path f) = do
  parseTimestampsFile <$> readFile f


parseTimestampsFile :: ByteString -> [DateBegTimestamp]
parseTimestampsFile inp =
  map DateBegTimestamp $ T.splitOn "\n" $ cs inp


readLevel1File :: (FileSystem :> es, Error GenerateError :> es) => Path L1Frame -> Eff es BinTableHDU
readLevel1File f = do
  inp <- readFile f.filePath
  fits <- decode inp
  case fits.extensions of
    [BinTable b] -> pure b
    _ -> throwError $ LiftL1 $ MissingL1HDU f.filePath


-- Filename Parser ----------------------------------------

type Parser = Parsec Void FilePath
type ParseErr = ParseErrorBundle FilePath Void


runParseFileName :: Path L1Frame -> Maybe L1Frame
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

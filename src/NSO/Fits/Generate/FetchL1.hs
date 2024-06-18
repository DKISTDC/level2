module NSO.Fits.Generate.FetchL1 where

import App.Globus as Globus
import App.Types
import Control.Monad (replicateM, void)
import Data.List qualified as L
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..), picosecondsToDiffTime)
import Data.Void (Void)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Reader.Dynamic
import NSO.Data.Datasets
import NSO.Data.Spectra (identifyLine)
import NSO.Fits.Generate.Error
import NSO.Prelude
import NSO.Types.InstrumentProgram
import System.FilePath (takeExtensions)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Read (readMaybe)


newtype Path a = Path {filePath :: FilePath}
  deriving newtype (Show, Read, Eq)


data L1Frame = L1Frame
  { file :: FilePath
  , timestamp :: UTCTime
  , wavelength :: Wavelength Nm
  , stokes :: Stokes
  }
  deriving (Show)


data L1FrameDir


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


-- TODO: maybe we want to return the folder as well? Some easy way to look it up, abstractly
-- also we need to know when it finishes...
transferCanonicalDataset :: (Globus :> es, Reader (Token Access) :> es, Reader (GlobusEndpoint App) :> es) => Dataset -> Eff es (Id Task, Path L1FrameDir)
transferCanonicalDataset d = do
  -- wait, we have to wait until the transfer finishes before scanning!
  (t, dir) <- Globus.initTransferDataset d
  pure (t, Path dir)


-- VISP_2023_05_01T19_00_59_515_00630200_V_AOPPO_L1.fits
listL1Frames :: (FileSystem :> es) => Path L1FrameDir -> Eff es [L1Frame]
listL1Frames (Path dir) = do
  fs <- fmap Path <$> listDirectory dir
  pure $ mapMaybe runParseFileName $ filter isL1File fs
 where
  isL1File (Path f) = takeExtensions f == ".fits"


-- Filename Parser ----------------------------------------

type Parser = Parsec Void FilePath
type ParseErr = ParseErrorBundle FilePath Void


runParseFileName :: Path L1Frame -> Maybe L1Frame
runParseFileName (Path f) =
  case runParser parseL1FileName f f of
    Left _ -> Nothing
    Right (u, w, s) -> pure $ L1Frame f u w s


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

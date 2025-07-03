{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Image.Headers where

import App.Version (appVersion)
import Data.List qualified as L
import Data.Text (pack)
import Data.Text qualified as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Writer.Static.Local
import GHC.Generics
import NSO.Image.Headers.Doc as Doc
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Prelude
import NSO.Types.Common (DateTime (..), Id (..))
import NSO.Types.Inversion (Inversion)
import Telescope.Fits as Fits
import Telescope.Fits.Header as Fits
import Telescope.Fits.Header.Class (GFromHeader (..), GToHeader (..))


headerSpecVersion :: Text
headerSpecVersion = "L2." <> pack appVersion


-- NOPE: PROV_URL - create a provenance URL page
-- LATER: DATAMEAN
-- LATER: DATAMEDN
-- LATER: DATARMS
-- LATER: DATAKURT
-- LATER: DATASKEW
-- NOPE: DATAP<pp>
-- NOPE: CONTINUE - if a url is too long. Or make sure they aren't too long :)
-- DONE: Doubles vs Floats - fits-parse

data Observation = Observation
  { origin :: Key (Constant "National Solar Observatory") "The organization or institution responsible for creating the FITS file."
  , telescop :: Key (Constant "Daniel K. Inouye Solar Telescope") "The telescope used to acquire the data associated with the header."
  , obsrvtry :: Key (Constant "Haleakala High Altitude Observatory Site") "A character string identifying the physical entity, located in a defined location, which provides the resources necessary for the installation of an instrument"
  , network :: Key (Constant "NSF-DKIST") "Organizational entity of a series of instruments with similar characteristics or goals that operates in some coordinated manner or produces data with some common purpose"
  , instrume :: Instrument
  , object :: Object
  , telapse :: Key Seconds "TELAPSE = DATE-END - DATE-BEG. Not always equal to the exposure time as multiple exposures could be combined"
  , dateBeg :: Key DateTime "Start date and time of light exposure for the frame"
  , dateEnd :: Key DateTime "End date and time of light exposure for the frame"
  , dateAvg :: Key DateTime "Date/Time of the midpoint of the frame. (DATE-END - DATE_BEG) / 2"
  , timesys :: Key (Constant "UTC") "Time scale of the time related keywords"
  , solarnet :: Key (Constant "1.0") "SOLARNET compliance: 1.0: Fully compliant 0.5: Partially compliant"
  }
  deriving (Generic, HeaderDoc)
  deriving (ToHeader, FromHeader) via (DashedKeys Observation)


-- Dashed Keys --- all keys use KEYBAB-CASE instead of SCREAM_CASE
newtype DashedKeys a = DashedKeys a


instance (Generic a, GToHeader (Rep a)) => ToHeader (DashedKeys a) where
  toHeader (DashedKeys a) =
    let Header rs = gToHeader $ from a
     in Header $ fmap toDashes rs


instance (Generic a, GFromHeader (Rep a)) => FromHeader (DashedKeys a) where
  parseHeader h =
    let Header rs = h
     in DashedKeys . to <$> gParseHeader (Header $ fmap toDashes rs)


toDashes :: HeaderRecord -> HeaderRecord
toDashes = \case
  (Keyword (KeywordRecord k v mc)) ->
    Keyword $ KeywordRecord (dashKey k) v mc
  hr -> hr
 where
  dashKey = T.replace "_" "-"


data Datacenter = Datacenter
  { dsetid :: Key (Id Inversion) "Unique ID of the dataset to which the frame belongs"
  , fileId :: Key Text "Unique ID of this FITS file"
  , framevol :: Key MB "Size of the frame on disk."
  , proctype :: Key (Constant "L2") "Controlled value list representing the degree of processing the frame has undergone since receipt at the DKIST data center."
  , filename :: Key Text "Name of current file"
  , level :: Key (Constant 2) "The level of the current data"
  , headvers :: Key Text "Version of this spec used during processing"
  , headUrl :: Key Url "Link to documentation for the headers of this frame"
  , infoUrl :: Key Url "Link to documentation for this frame"
  , dkistver :: Key Text "Version of the DKIST FITS Header format from the summit"
  , -- , provUrl :: Key Url "Link to provenance for this file"
    obsprId :: Key Text "Unique ID dynamically generated at the time the ObservingProgram was submitted for execution. This unique ID shall contain the ObservingProgramID (16 characters) as base ID, a subsequent period (‘.’), and a unique suffix containing of up to 17 characters, for a total of up to 34 characters"
  , experId :: Key Text "Unique ID of the experiment associated with the generation of these data"
  , propId :: Key Text "Unique ID of the proposal associated with the experiment associated with the generation of these data"
  , dspId :: Key Text "Unique ID of the DataSetParameters used to configure the InstrumentProgram associated with these data"
  , ipId :: Key Text "Unique ID dynamically generated at he time the InstrumentProgram was submitted for execution. This unique ID shall contain the InstrumentProgramID (16 characters) as base ID, a subsequent period (‘.’), and a unique suffix containing of up to 17 characters, for a total of up to 34 characters"
  , hlsvers :: Key Text "Version of HLS (DKIST summit) software that produced the L0 observe frames from which this frame was produced"
  , npropos :: Key Int "Number of proposals that contributed to the input data used to make this output. Must be > 0"
  , nexpers :: Key Int "Number of experiments that contributed to the input data used to make this output. Must be > 0"
  }
  deriving (Generic, ToHeader, FromHeader)


-- | contains an variable number of contributing proposals and experiments
data ContribExpProp = ContribExpProp [KeywordRecord]


instance ToHeader ContribExpProp where
  toHeader (ContribExpProp krs) =
    Header $ fmap Keyword $ filter isPropOrExpr krs
instance FromHeader ContribExpProp where
  -- TEST: is this working?
  parseHeader h =
    pure $ ContribExpProp $ filter isPropOrExpr $ keywords h
instance HeaderDoc ContribExpProp where
  -- LATER: documentation for ContribExpProp
  headerDoc = []


isPropOrExpr :: KeywordRecord -> Bool
isPropOrExpr kr =
  "PROPID" `T.isPrefixOf` kr.keyword
    || "EXPRID" `T.isPrefixOf` kr.keyword


-- data StatisticsHeader = StatisticsHeader
--   { datamin :: Key Float "The minimum data value"
--   , datamax :: Key Float "The maximum data value"
--   -- , datamean :: Key Float "The average data value"
--   -- , datamedn :: Key Float "The median data value"
--   -- , datarms :: Key Float "The RMS deviation from the mean"
--   -- , datakurt :: Key Float "The kurtosis"
--   -- , dataskew :: Key Float "The skewness"
--   }
--   deriving (Generic, HeaderDoc, HeaderKeywords)

-- TelescopeHeader -------------------------------------------------

data Telescope = Telescope
  { tazimuth :: Key Degrees "Raw Telescope azimuth angle"
  , elevAng :: Key Degrees "Raw Telescope elevation angle"
  , teltrack :: Teltrack
  , telscan :: Maybe Telscan
  , ttbltrck :: Ttbltrck
  , ttblangl :: Key Degrees "Telescope Coude table angle"
  , dateref :: Key DateTime "Time coordinate zero point"
  , rotcomp :: Maybe (Key Int "Solar rotation compensation: 1: On 2: Off")
  , obsVr :: Key Mps "Observer’s outward velocity w.r.t. the Sun"
  }
  deriving (Generic, ToHeader, FromHeader)


data Obsgeo = Obsgeo
  { obsgeoX :: Key Meters "Observer’s fixed geographic X coordinate"
  , obsgeoY :: Key Meters "Observer’s fixed geographic Y coordinate"
  , obsgeoZ :: Key Meters "Observer’s fixed geographic Z coordinate"
  }
  deriving (Generic)
  deriving (ToHeader, FromHeader) via (DashedKeys Obsgeo)


data DKISTHeader = DKISTHeader
  { ocsCtrl :: EnumKey "auto/manual" "Control mode the telescope was operated in: ‘Auto’: Data were acquired as part of a regular, automatic execution of an Observing Program ‘Manual’: Data were acquired executing either a part of an or a complete Observing Program manually"
  , fidoCfg :: Key Text "The DKIST FIDO configuration in the following format: [M9aStatus,CL2,CL2a,CL3,CL3a,CL4]"
  , dshealth :: EnumKey "good/bad/ill/unknown" "Worst health status of the data source (e.g. instrument arm) during data acquisition Good, Ill, Bad, Unknown"
  , lightlvl :: Key Float "Value of the telescope light level at start of data acquisition"
  }
  deriving (Generic, ToHeader, FromHeader)


data AdaptiveOptics = AdaptiveOptics
  { atmosR0 :: Key Float "Value of Fried’s parameter at start of data acquisition"
  , aoLock :: Key Bool "Lock status of HOAO during data acquisition. False: HOAO was unlocked for some duration of data acquisition True: HOAO was locked for the complete duration of data acquisition"
  }
  deriving (Generic, ToHeader, FromHeader)


newtype Teltrack = Teltrack Text
  deriving newtype (ToKeyword, FromKeyword)
instance KeywordInfo Teltrack where
  keytype = "Teltrack"
  description = "Tracking Mode of the Telescope"
  allowed = fmap String ["None", "Fixed Solar Rotation Tracking", "Standard Differential Rotation Tracking", "Custom Differential Rotation Tracking"]


newtype Telscan = Telscan Text
  deriving newtype (ToKeyword, FromKeyword)
instance KeywordInfo Telscan where
  keytype = "Telscan"
  description = "Scanning Mode of the Telescope"
  allowed = fmap String ["None", "Random", "Raster", "Spiral"]


newtype Ttbltrck = Ttbltrck Text
  deriving newtype (ToKeyword, FromKeyword)
instance KeywordInfo Ttbltrck where
  keytype = "Ttbltrck"
  description = "Coude table tracking mode."
  allowed = fmap String ["(stepped) parallactic", "fixed angle on sun", "fixed difference-angle btwn", "coude and tel. azimuth", "fixed coude table angle"]


-- GENERATE ------------------------------------------------------------

observationHeader :: (Error ParseError :> es) => Header -> Eff es Observation
observationHeader l1 = do
  dateBeg <- requireKey "DATE-BEG" l1
  dateEnd <- requireKey "DATE-END" l1
  dateAvg <- requireKey "DATE-AVG" l1
  telapse <- requireKey "TELAPSE" l1
  object <- requireKey "OBJECT" l1
  instrument <- requireKey "INSTRUME" l1

  pure $
    Observation
      { telapse = Key (Seconds telapse)
      , instrume = Instrument instrument
      , timesys = Key Constant
      , telescop = Key Constant
      , obsrvtry = Key Constant
      , network = Key Constant
      , object = Object object
      , solarnet = Key Constant
      , origin = Key Constant
      , dateBeg = Key dateBeg
      , dateEnd = Key dateEnd
      , dateAvg = Key dateAvg
      }


datacenterHeader :: (Error ParseError :> es, GenRandom :> es) => Header -> Id Inversion -> Eff es Datacenter
datacenterHeader l1 i = do
  DateTime dateBeg <- requireKey "DATE-BEG" l1
  dkistver <- requireKey "DKISTVER" l1
  obsprId <- Key <$> requireKey "OBSPR_ID" l1
  experId <- Key <$> requireKey "EXPER_ID" l1
  propId <- Key <$> requireKey "PROP_ID" l1
  dspId <- Key <$> requireKey "DSP_ID" l1
  ipId <- Key <$> requireKey "IP_ID" l1
  hlsvers <- Key <$> requireKey "HLSVERS" l1
  npropos <- Key <$> requireKey "NPROPOS" l1
  nexpers <- Key <$> requireKey "NEXPERS" l1
  fileId <- Key . UUID.toText <$> randomValue
  pure $
    Datacenter
      { dsetid = Key i
      , framevol = Key $ MB 0
      , proctype = Key Constant
      , filename = Key $ fitsFrameFilename dateBeg i
      , level = Key Constant
      , headvers = Key headerSpecVersion
      , headUrl = Key (Url $ "https://docs.dkist.nso.edu/projects/data-products/en/" <> headerSpecVersion)
      , infoUrl = Key (Url "https://docs.dkist.nso.edu")
      , fileId
      , dkistver = Key dkistver
      , obsprId
      , experId
      , propId
      , dspId
      , ipId
      , hlsvers
      , npropos
      , nexpers
      }


contribExpProp :: (Error ParseError :> es) => Header -> Eff es ContribExpProp
contribExpProp l1 = do
  pure $ ContribExpProp (keywords l1)


dkistHeader :: (Error ParseError :> es) => Header -> Eff es DKISTHeader
dkistHeader l1 = do
  ocsCtrl <- EnumKey <$> requireKey "OCS_CTRL" l1
  fidoCfg <- Key <$> requireKey "FIDO_CFG" l1
  dshealth <- EnumKey <$> requireKey "DSHEALTH" l1
  lightlvl <- Key <$> requireKey "LIGHTLVL" l1
  pure $
    DKISTHeader
      { ocsCtrl
      , fidoCfg
      , dshealth
      , lightlvl
      }


adaptiveOpticsHeader :: (Error ParseError :> es) => Header -> Eff es AdaptiveOptics
adaptiveOpticsHeader l1 = do
  atmosR0 <- Key <$> requireKey "ATMOS_R0" l1
  aoLock <- Key <$> requireKey "AO_LOCK" l1
  pure $
    AdaptiveOptics
      { atmosR0
      , aoLock
      }


telescopeHeader :: (Error ParseError :> es) => Header -> Eff es Telescope
telescopeHeader l1 = do
  tazimuth <- Key . Degrees <$> requireKey "TAZIMUTH" l1
  elevAng <- Key . Degrees <$> requireKey "ELEV_ANG" l1
  teltrack <- Teltrack <$> requireKey "TELTRACK" l1
  telscan <- fmap Telscan <$> lookupKey "TELSCAN" l1
  ttblangl <- Key . Degrees <$> requireKey "TTBLANGL" l1
  ttbltrck <- Ttbltrck <$> requireKey "TTBLTRCK" l1
  dateref <- Key <$> requireKey "DATEREF" l1
  rotcomp <- fmap Key <$> lookupKey "ROTCOMP" l1
  obsVr <- Key . Mps <$> requireKey "OBS_VR" l1
  pure $ Telescope{..}


obsgeoHeader :: (Error ParseError :> es) => Header -> Eff es Obsgeo
obsgeoHeader l1 = do
  obsgeoX <- Key . Meters <$> requireKey "OBSGEO-X" l1
  obsgeoY <- Key . Meters <$> requireKey "OBSGEO-Y" l1
  obsgeoZ <- Key . Meters <$> requireKey "OBSGEO-Z" l1
  pure $ Obsgeo{obsgeoX, obsgeoY, obsgeoZ}


-- statsHeader :: Results Frame -> Eff es StatisticsHeader
-- statsHeader res = do
--   let datamin = Key (minimum res.array)
--       datamax = Key (maximum res.array)
--   -- datamean = Key (sum res.array / fromIntegral (M.elemsCount res.array))
--   -- datamedn = Key 0
--   -- datarms = Key 0
--   -- datakurt = Key 0
--   -- dataskew = Key 0
--   pure $ StatisticsHeader{..}

-- 2023_10_16T23_55_59_513_00589600_inv_290834_L2.fits
fitsFrameFilename :: UTCTime -> Id Inversion -> Text
fitsFrameFilename start iv =
  addExtension . T.toUpper . T.map toUnderscore $
    T.intercalate
      "_"
      [ iv.fromId
      , T.dropWhileEnd (== 'Z') $ cs $ iso8601Show start
      , "L2"
      ]
 where
  addExtension f = f <> ".fits"

  toUnderscore :: Char -> Char
  toUnderscore '.' = '_'
  toUnderscore ':' = '_'
  toUnderscore '-' = '_'
  toUnderscore c = c


writeHeader :: Eff '[Writer Header] () -> Header
writeHeader = runPureEff . execWriter


addKeywords :: (Writer Header :> es, ToHeader a) => a -> Eff es ()
addKeywords a = tell $ toHeader a


sectionHeader :: (Writer Header :> es) => Text -> Text -> Eff es ()
sectionHeader title desc = do
  tell $ Header [BlankLine, Comment $ center "-" title]
  tell $ Header $ fmap (Comment . center " ") (wrapWords desc)
  tell $ Header [Comment $ T.replicate maxSize "-"]
 where
  maxSize = 70

  center :: Text -> Text -> Text
  center c txt =
    let rest = maxSize - T.length txt - 2
        l = rest `div` 2
        r = rest - l
     in T.replicate l c <> " " <> txt <> " " <> T.replicate r c

  wrapWords :: Text -> [Text]
  wrapWords text = L.reverse $ fmap (T.unwords . L.reverse) $ foldl next [] $ T.words text
   where
    next :: [[Text]] -> Text -> [[Text]]
    next [] w = [[w]]
    next (l : ls) w
      | lineLength l + T.length w + 1 < maxSize = (w : l) : ls
      | otherwise = [w] : l : ls

    lineLength :: [Text] -> Int
    lineLength ws = sum $ fmap (\t -> T.length t + 1) ws

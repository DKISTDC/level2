{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module NSO.Image.Headers where

import App.Version (appVersion)
import Data.List qualified as L
import Data.Text (pack)
import Data.Text qualified as T
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
import NSO.Types.Common (Id (..))
import NSO.Types.Inversion (Inversion)
import Telescope.Fits as Fits
import Telescope.Fits.Header (KeywordRecord (..), LogicalConstant (..), getKeywords, toFloat, toInt, toText)
import Telescope.Fits.Types (HeaderRecord (..))


headerSpecVersion :: Text
headerSpecVersion = "L2." <> pack appVersion


-- DONE: automatic type-based comments
-- DONE: custom comments with custom newtype (cleaner)
-- DONE: comments for bitpix, naxes and other auto-gen keywords.
-- DONE: FILENAME - based on input filename
-- DONE: Support optional lifted L1 headers
-- DONE: WCS
-- DONE: DSETID - the inversion id?
-- DONE: TIMESYS - missing from L1 input?

-- DONE: HEADVERS - current version number for the spec
-- DONE: FILE_ID - UUID for this frame - where do I need to store this?
-- DONE: HEAD_URL - create a url that links to this
-- DONE: INFO_URL - what's the difference? Is there another documentation page? Does it have a different spec?
-- DONE: FRAMEVOL - estimate based on the dimensions, bitpix, and average header size. Waiting on Profile HDUs, etc
-- NOPE: PROV_URL - create a provenance URL page

-- DONE: DATAMIN
-- DONE: DATAMAX
-- LATER: DATAMEAN
-- LATER: DATAMEDN
-- LATER: DATARMS
-- LATER: DATAKURT
-- LATER: DATASKEW
-- NOPE: DATAP<pp>

-- DONE: CHECKSUM - telescope
-- DONE: DATASUM - telescope
-- DONE: PCOUNT - telescope
-- DONE: GCOUNT - telescope
-- TODO: Doubles vs Floats - fits-parse
--
-- NOPE: CONTINUE - if a url is too long. Or make sure they aren't too long :)

data Observation = Observation
  { origin :: Key (Constant "National Solar Observatory") "The organization or institution responsible for creating the FITS file."
  , telescope :: Key (Constant "Daniel K. Inouye Solar Telescope") "The telescope used to acquire the data associated with the header."
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
  deriving (Generic, HeaderDoc, HeaderKeywords)


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
  deriving (Generic, HeaderKeywords)


data ContribExpProp = ContribExpProp [KeywordRecord]
instance HeaderKeywords ContribExpProp where
  headerKeywords (ContribExpProp krs) = filter (isProp `or_` isExpr) krs
   where
    or_ f g a = f a || g a
    isProp kr = "PROPID" `T.isPrefixOf` kr._keyword
    isExpr kr = "EXPRID" `T.isPrefixOf` kr._keyword
instance HeaderDoc ContribExpProp where
  headerDoc = []


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
  , obsgeoX :: Key Meters "Observer’s fixed geographic X coordinate"
  , obsgeoY :: Key Meters "Observer’s fixed geographic Y coordinate"
  , obsgeoZ :: Key Meters "Observer’s fixed geographic Z coordinate"
  , rotcomp :: Maybe (Key Int "Solar rotation compensation: 1: On 2: Off")
  , obsVr :: Key Mps "Observer’s outward velocity w.r.t. the Sun"
  }
  deriving (Generic, HeaderKeywords)


data DKISTHeader = DKISTHeader
  { ocsCtrl :: EnumKey "auto/manual" "Control mode the telescope was operated in: ‘Auto’: Data were acquired as part of a regular, automatic execution of an Observing Program ‘Manual’: Data were acquired executing either a part of an or a complete Observing Program manually"
  , fidoCfg :: Key Text "The DKIST FIDO configuration in the following format: [M9aStatus,CL2,CL2a,CL3,CL3a,CL4]"
  , dshealth :: EnumKey "good/bad/ill/unknown" "Worst health status of the data source (e.g. instrument arm) during data acquisition Good, Ill, Bad, Unknown"
  , lightlvl :: Key Float "Value of the telescope light level at start of data acquisition"
  }
  deriving (Generic, HeaderKeywords)


data AdaptiveOptics = AdaptiveOptics
  { atmosR0 :: Key Float "Value of Fried’s parameter at start of data acquisition"
  , aoLock :: Key Bool "Lock status of HOAO during data acquisition. False: HOAO was unlocked for some duration of data acquisition True: HOAO was locked for the complete duration of data acquisition"
  }
  deriving (Generic, HeaderKeywords)


data Teltrack = Teltrack Text deriving (Generic)
instance KeywordInfo Teltrack where
  keytype = "Teltrack"
  description = "Tracking Mode of the Telescope"
  allowed = fmap String ["None", "Fixed Solar Rotation Tracking", "Standard Differential Rotation Tracking", "Custom Differential Rotation Tracking"]
  keyValue (Teltrack s) = String s
instance HeaderKeywords Teltrack where
  headerKeywords b = [keywordRecord b]


data Telscan = Telscan Text deriving (Generic)
instance KeywordInfo Telscan where
  keytype = "Telscan"
  description = "Scanning Mode of the Telescope"
  allowed = fmap String ["None", "Random", "Raster", "Spiral"]
  keyValue (Telscan s) = String s
instance HeaderKeywords Telscan where
  headerKeywords b = [keywordRecord b]


data Ttbltrck = Ttbltrck Text deriving (Generic)
instance KeywordInfo Ttbltrck where
  keytype = "Ttbltrck"
  description = "Coude table tracking mode."
  allowed = fmap String ["(stepped) parallactic", "fixed angle on sun", "fixed difference-angle btwn", "coude and tel. azimuth", "fixed coude table angle"]
  keyValue (Ttbltrck s) = String s
instance HeaderKeywords Ttbltrck where
  headerKeywords b = [keywordRecord b]


-- GENERATE ------------------------------------------------------------

observationHeader :: (Error ParseKeyError :> es) => Header -> Eff es Observation
observationHeader l1 = do
  dateBeg <- requireKey "DATE-BEG" toDate l1
  dateEnd <- requireKey "DATE-END" toDate l1
  dateAvg <- requireKey "DATE-AVG" toDate l1
  telapse <- requireKey "TELAPSE" toFloat l1
  object <- requireKey "OBJECT" toText l1
  instrument <- requireKey "INSTRUME" toText l1

  pure $
    Observation
      { telapse = Key (Seconds telapse)
      , instrume = Instrument instrument
      , timesys = Key Constant
      , telescope = Key Constant
      , obsrvtry = Key Constant
      , network = Key Constant
      , object = Object object
      , solarnet = Key Constant
      , origin = Key Constant
      , dateBeg = Key dateBeg
      , dateEnd = Key dateEnd
      , dateAvg = Key dateAvg
      }


datacenterHeader :: (Error ParseKeyError :> es, GenRandom :> es) => Header -> Id Inversion -> Eff es Datacenter
datacenterHeader l1 i = do
  dateBeg <- requireKey "DATE-BEG" toDate l1
  dkistver <- requireKey "DKISTVER" toText l1
  obsprId <- Key <$> requireKey "OBSPR_ID" toText l1
  experId <- Key <$> requireKey "EXPER_ID" toText l1
  propId <- Key <$> requireKey "PROP_ID" toText l1
  dspId <- Key <$> requireKey "DSP_ID" toText l1
  ipId <- Key <$> requireKey "IP_ID" toText l1
  hlsvers <- Key <$> requireKey "HLSVERS" toText l1
  npropos <- Key <$> requireKey "NPROPOS" toInt l1
  nexpers <- Key <$> requireKey "NEXPERS" toInt l1
  fileId <- Key . UUID.toText <$> randomValue
  pure $
    Datacenter
      { dsetid = Key i
      , framevol = Key $ MB 0
      , proctype = Key Constant
      , filename = Key $ frameFilename dateBeg i
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


contribExpProp :: (Error ParseKeyError :> es) => Header -> Eff es ContribExpProp
contribExpProp l1 = do
  pure $ ContribExpProp (getKeywords l1)


dkistHeader :: (Error ParseKeyError :> es) => Header -> Eff es DKISTHeader
dkistHeader l1 = do
  ocsCtrl <- EnumKey <$> requireKey "OCS_CTRL" toText l1
  fidoCfg <- Key <$> requireKey "FIDO_CFG" toText l1
  dshealth <- EnumKey <$> requireKey "DSHEALTH" toText l1
  lightlvl <- Key <$> requireKey "LIGHTLVL" toFloat l1
  pure $
    DKISTHeader
      { ocsCtrl
      , fidoCfg
      , dshealth
      , lightlvl
      }


adaptiveOpticsHeader :: (Error ParseKeyError :> es) => Header -> Eff es AdaptiveOptics
adaptiveOpticsHeader l1 = do
  atmosR0 <- Key <$> requireKey "ATMOS_R0" toFloat l1
  aoLock <- Key <$> requireKey "AO_LOCK" toBool l1
  pure $
    AdaptiveOptics
      { atmosR0
      , aoLock
      }
 where
  toBool (Logic T) = Just True
  toBool (Logic F) = Just False
  toBool _ = Nothing


telescopeHeader :: (Error ParseKeyError :> es) => Header -> Eff es Telescope
telescopeHeader l1 = do
  tazimuth <- Key . Degrees <$> requireKey "TAZIMUTH" toFloat l1
  elevAng <- Key . Degrees <$> requireKey "ELEV_ANG" toFloat l1
  teltrack <- Teltrack <$> requireKey "TELTRACK" toText l1
  telscan <- fmap Telscan <$> lookupKey "TELSCAN" toText l1
  ttblangl <- Key . Degrees <$> requireKey "TTBLANGL" toFloat l1
  ttbltrck <- Ttbltrck <$> requireKey "TTBLTRCK" toText l1
  dateref <- Key . DateTime <$> requireKey "DATEREF" toText l1
  obsgeoX <- Key . Meters <$> requireKey "OBSGEO-X" toFloat l1
  obsgeoY <- Key . Meters <$> requireKey "OBSGEO-Y" toFloat l1
  obsgeoZ <- Key . Meters <$> requireKey "OBSGEO-Z" toFloat l1
  rotcomp <- fmap Key <$> lookupKey "ROTCOMP" toInt l1
  obsVr <- Key . Mps <$> requireKey "OBS_VR" toFloat l1
  pure $ Telescope{..}


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

-- VISP_2023_10_16T23_55_59_513_00589600_I_ADDMM_L1.fits
-- = INSTRUMENT DATETIME DATASET L1
-- => DATETIME FRAMEID L2
-- 2023_10_16T23_55_59_513_00589600_inv_290834_L2.fits
frameFilename :: DateTime -> Id Inversion -> Text
frameFilename (DateTime start) iv =
  addExtension $
    T.toUpper $
      T.intercalate
        "_"
        [ T.replace "." "_" iv.fromId
        , start
        , "L2"
        ]
 where
  addExtension f = f <> ".fits"


writeHeader :: Eff (Writer [HeaderRecord] : es) () -> Eff es [HeaderRecord]
writeHeader = execWriter


addKeywords :: (Writer [HeaderRecord] :> es) => [KeywordRecord] -> Eff es ()
addKeywords = tell . fmap Keyword


sectionHeader :: (Writer [HeaderRecord] :> es) => Text -> Text -> Eff es ()
sectionHeader title desc = do
  tell [BlankLine, Comment $ center "-" title]
  tell $ fmap (Comment . center " ") (wrapWords desc)
  tell [Comment $ T.replicate maxSize "-"]
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

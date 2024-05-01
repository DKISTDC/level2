{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module NSO.Fits.Generate.Headers where

import App.Version (appVersion)
import Control.Monad.Catch (Exception)
import Data.Fits (KeywordRecord (..), LogicalConstant (..), getKeywords, toFloat, toInt, toText)
import Data.List qualified as L
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Text (pack, unpack)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Debug.Trace
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Writer.Static.Local
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc as Doc
import NSO.Fits.Generate.Keywords
import NSO.Fits.Generate.Types
import NSO.Prelude
import NSO.Types.Common (Id (..))
import NSO.Types.Inversion (Inversion)
import Telescope.Fits as Fits
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
-- NOPE: PROV_URL - create a provenance URL page

-- DONE: DATAMIN
-- DONE: DATAMAX
-- LATER: DATAMEAN
-- LATER: DATAMEDN
-- LATER: DATARMS
-- LATER: DATAKURT
-- LATER: DATASKEW
-- NOPE: DATAP<pp>

-- TODO: CHECKSUM - telescope
-- TODO: DATASUM - telescope
-- TODO: PCOUNT - telescope
-- TODO: GCOUNT - telescope
-- TODO: Doubles vs Floats - fits-parse

-- TODO: FRAMEVOL - estimate based on the dimensions, bitpix, and average header size

-- LATER: CONTINUE - if a url is too long. Or make sure they aren't too long :)

data ObservationHeader = ObservationHeader
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
  -- TODO: manually add PROPID<rr>, and EXPRID<ee>
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

data TelescopeHeader = TelescopeHeader
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


data Telscan = Telscan Text deriving (Generic)
instance KeywordInfo Telscan where
  keytype = "Telscan"
  description = "Scanning Mode of the Telescope"
  allowed = fmap String ["None", "Random", "Raster", "Spiral"]
  keyValue (Telscan s) = String s


data Ttbltrck = Ttbltrck Text deriving (Generic)
instance KeywordInfo Ttbltrck where
  keytype = "Ttbltrck"
  description = "Coude table tracking mode."
  allowed = fmap String ["(stepped) parallactic", "fixed angle on sun", "fixed difference-angle btwn", "coude and tel. azimuth", "fixed coude table angle"]
  keyValue (Ttbltrck s) = String s


-- WCS --------------------------------------------------------------

data WCSCommon = WCSCommon
  { wcsvalid :: Key (Constant True) "WCI data are correct"
  , wcsname :: Key (Constant "Helioprojective Cartesian") "Helioprojective Cartesian"
  , wcsaxes :: Key (Constant 3) "Number of axes in the Helioprojective Cartesian WCS description" -- MUST precede other WCS keywords
  , lonpole :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  , wcsnamea :: Key (Constant "Equatorial equinox J2000") "Equatorial equinox J2000"
  , wcsaxesa :: Key (Constant 3) "Number of axes in the Equatorial equinox J2000 WCS description" -- MUST precede other WCS keywords
  , lonpolea :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


data WCSAxisKeywords (alt :: WCSAlt) (n :: Nat) = WCSAxisKeywords
  { ctype :: Key Text "A string value labeling axis n of the Helioprojective Coordinate system."
  , cunit :: Key Text "The unit of the value contained in CDELTn"
  , crpix :: Key Float "The value field shall contain a floating point number, identifying the location of a reference point along axis n of the Helioprojective coordinate system, in units of the axis index. This value is based upon a counter that runs from 1 to NAXISn with an increment of 1 per pixel. The reference point value need not be that for the center of a pixel nor lie within the actual data array. Use comments to indicate the location of the index point relative to the pixel. DKIST pointing data will be relative to the boresight of the telescope defined by the WFC Context Viewer that will center an image of the GOS pinhole on its detector using a 10 nm wavelength band centered on 525 nm. The same pinhole image will be used by all instruments as reference for determining the pointing of the instrument in relation to the WFC Context Viewer."
  , crval :: Key Float "The value field shall contain a floating point number, giving the value of the coordinate specified by the CTYPEn keyword at the reference point CRPIXn. DKIST values for this entry will be determined at the wavelength of the WFC Context Viewer, which covers a band of 10 nm centered on 525 nm. The value will not be corrected for differential refraction of Earth’s atmosphere."
  , cdelt :: Key Float "Pixel scale of the world coordinate at the reference point along axis n of the Helioprojective coordinate system. This value must not be zero."
  }
  deriving (Generic)
instance (KnownNat n, KnownValue alt) => HeaderKeywords (WCSAxisKeywords alt n) where
  headerKeywords =
    fmap modKey . genHeaderKeywords . from
   where
    modKey KeywordRecord{_keyword, _value, _comment} = KeywordRecord{_keyword = addA $ addN _keyword, _value, _comment}
    addN k = k <> pack (show (natVal @n Proxy))
    addA k = k <> knownValueText @alt


data WCSAxis (alt :: WCSAlt) (n :: Nat) = WCSAxis
  { keys :: WCSAxisKeywords alt n
  , pcs :: DataAxes (PC alt n)
  }


instance (KnownValue alt, KnownNat n) => HeaderKeywords (WCSAxis alt n) where
  headerKeywords ax =
    headerKeywords ax.keys
      <> headerKeywords ax.pcs


data PC (alt :: WCSAlt) (i :: Nat) (j :: Nat) = PC Float
instance (KnownValue alt, KnownNat i, KnownNat j) => KeywordInfo (PC alt i j) where
  keyword = "PC" <> showN @i Proxy <> "_" <> showN @j Proxy <> knownValueText @alt
   where
    showN :: forall n. (KnownNat n) => Proxy n -> Text
    showN p = pack (show $ natVal p)
  keytype = "PCi_j"
  description = "Linear transformation matrix used with the Helioprojective coordinate system"
  keyValue (PC n) = Float n


type DummyYN = 3
type SlitXN = 2
type DepthN = 1


wcsCommon :: forall es. (Error FitsGenError :> es) => Header -> Eff es WCSCommon
wcsCommon l1 = do
  lonpole <- Degrees <$> requireL1 "LONPOLE" toFloat l1
  lonpolea <- Degrees <$> requireL1 "LONPOLEA" toFloat l1
  pure $
    WCSCommon
      { wcsvalid = Key Constant
      , wcsname = Key Constant
      , wcsaxes = Key Constant
      , lonpole = Key lonpole
      , wcsnamea = Key Constant
      , wcsaxesa = Key Constant
      , lonpolea = Key lonpolea
      }


data DataAxes f = DataAxes
  { dummyY :: f DummyYN
  , slitX :: f SlitXN
  , depth :: f DepthN
  }
  deriving (Generic)
instance (KnownValue alt, KnownNat n) => HeaderKeywords (DataAxes (PC alt n))
instance (KnownValue alt) => HeaderKeywords (DataAxes (WCSAxis alt)) where
  headerKeywords a =
    headerKeywords @(WCSAxis alt DepthN) a.depth
      <> headerKeywords @(WCSAxis alt SlitXN) a.slitX
      <> headerKeywords @(WCSAxis alt DummyYN) a.dummyY


wcsAxes :: forall alt es. (Error FitsGenError :> es, KnownValue alt) => Sz Ix2 -> Header -> Eff es (DataAxes (WCSAxis alt))
wcsAxes sz h = do
  dummyY <- wcsDummyY h
  slitX <- wcsSlitX sz h
  depth <- wcsDepth
  pure $ DataAxes{..}


requireWCS :: forall alt n es. (Error FitsGenError :> es, KnownValue alt) => Int -> Header -> Eff es (WCSAxisKeywords alt n)
requireWCS n l1 = do
  crpix <- Key <$> requireL1 (keyN "CRPIX") toFloat l1
  crval <- Key <$> requireL1 (keyN "CRVAL") toFloat l1
  cdelt <- Key <$> requireL1 (keyN "CDELT") toFloat l1
  cunit <- Key <$> requireL1 (keyN "CUNIT") toText l1
  ctype <- Key <$> requireL1 (keyN "CTYPE") toText l1
  pure $ WCSAxisKeywords{cunit, ctype, crpix, crval, cdelt}
 where
  keyN k = k <> pack (show n) <> knownValueText @alt


requirePCs :: forall alt n es. (Error FitsGenError :> es, KnownValue alt) => Int -> Header -> Eff es (DataAxes (PC alt n))
requirePCs n l1 = do
  dummyY <- PC <$> requireL1 (pcN n 3) toFloat l1
  slitX <- PC <$> requireL1 (pcN n 1) toFloat l1
  pure $ DataAxes{dummyY, slitX, depth = PC 0}
 where
  pcN :: Int -> Int -> Text
  pcN i j = "PC" <> pack (show i) <> "_" <> pack (show j) <> knownValueText @alt


wcsDummyY :: (Error FitsGenError :> es, KnownValue alt) => Header -> Eff es (WCSAxis alt DummyYN)
wcsDummyY l1 = do
  keys <- requireWCS 3 l1
  pcs <- requirePCs 3 l1
  pure $ WCSAxis{keys, pcs}


wcsSlitX :: forall alt es. (Error FitsGenError :> es, KnownValue alt) => Sz Ix2 -> Header -> Eff es (WCSAxis alt SlitXN)
wcsSlitX sz l1 = do
  scaleUp <- upFactor sz

  keys <- requireWCS @alt 1 l1
  pcs <- requirePCs @alt 1 l1
  traceM $ show (keys.crpix, keys.cdelt)
  pure $ WCSAxis{keys = scale scaleUp keys, pcs}
 where
  upFactor :: Sz Ix2 -> Eff es Float
  upFactor (Sz (newx :. _)) = do
    oldx <- requireL1 "ZNAXIS1" toInt l1
    pure $ fromIntegral oldx / fromIntegral newx

  scale up ax =
    WCSAxisKeywords
      { cunit = ax.cunit
      , ctype = ax.ctype
      , crval = ax.crval
      , crpix = scaleCrPix ax.crpix
      , cdelt = scaleCDelt ax.cdelt
      }
   where
    scaleCrPix (Key cp) = Key $ (cp - 1) / up + 1
    scaleCDelt (Key cd) = Key $ cd * up


wcsDepth :: (Monad m) => m (WCSAxis alt DepthN)
wcsDepth = do
  let crpix = Key 12
      crval = Key 0
      cdelt = Key 0.1
      cunit = Key ""
      ctype = Key "TAU--LOG"
      alt = WCSMain
  let keys = WCSAxisKeywords{..}
  let pcs = DataAxes{dummyY = PC 0, slitX = PC 0, depth = PC 1.0}
  pure $ WCSAxis{keys, pcs}


-- GENERATE ------------------------------------------------------------

observationHeader :: (Error FitsGenError :> es) => Header -> Eff es ObservationHeader
observationHeader l1 = do
  dateBeg <- requireL1 "DATE-BEG" toDate l1
  dateEnd <- requireL1 "DATE-END" toDate l1
  dateAvg <- requireL1 "DATE-AVG" toDate l1
  telapse <- requireL1 "TELAPSE" toFloat l1
  object <- requireL1 "OBJECT" toText l1
  instrument <- requireL1 "INSTRUME" toText l1

  pure $
    ObservationHeader
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


datacenterHeader :: (Error FitsGenError :> es, GenRandom :> es) => Header -> Id Inversion -> Eff es Datacenter
datacenterHeader l1 i = do
  dateBeg <- requireL1 "DATE-BEG" toDate l1
  dkistver <- requireL1 "DKISTVER" toText l1
  obsprId <- Key <$> requireL1 "OBSPR_ID" toText l1
  experId <- Key <$> requireL1 "EXPER_ID" toText l1
  propId <- Key <$> requireL1 "PROP_ID" toText l1
  dspId <- Key <$> requireL1 "DSP_ID" toText l1
  ipId <- Key <$> requireL1 "IP_ID" toText l1
  hlsvers <- Key <$> requireL1 "HLSVERS" toText l1
  npropos <- Key <$> requireL1 "NPROPOS" toInt l1
  nexpers <- Key <$> requireL1 "NEXPERS" toInt l1
  fileId <- Key . UUID.toText <$> randomValue
  pure $
    Datacenter
      { dsetid = Key i
      , framevol = Key $ MB 123
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


contribExpProp :: (Error FitsGenError :> es) => Header -> Eff es ContribExpProp
contribExpProp l1 = do
  pure $ ContribExpProp (getKeywords l1)


dkistHeader :: (Error FitsGenError :> es) => Header -> Eff es DKISTHeader
dkistHeader l1 = do
  ocsCtrl <- EnumKey <$> requireL1 "OCS_CTRL" toText l1
  fidoCfg <- Key <$> requireL1 "FIDO_CFG" toText l1
  dshealth <- EnumKey <$> requireL1 "DSHEALTH" toText l1
  lightlvl <- Key <$> requireL1 "LIGHTLVL" toFloat l1
  pure $
    DKISTHeader
      { ocsCtrl
      , fidoCfg
      , dshealth
      , lightlvl
      }


adaptiveOpticsHeader :: (Error FitsGenError :> es) => Header -> Eff es AdaptiveOptics
adaptiveOpticsHeader l1 = do
  atmosR0 <- Key <$> requireL1 "ATMOS_R0" toFloat l1
  aoLock <- Key <$> requireL1 "AO_LOCK" toBool l1
  pure $
    AdaptiveOptics
      { atmosR0
      , aoLock
      }
 where
  toBool (Logic T) = Just True
  toBool (Logic F) = Just False
  toBool _ = Nothing


-- TODO: this belongs in the data, not the primary! ?? Why? Required?
telescopeHeader :: (Error FitsGenError :> es) => Header -> Eff es TelescopeHeader
telescopeHeader l1 = do
  tazimuth <- Key . Degrees <$> requireL1 "TAZIMUTH" toFloat l1
  elevAng <- Key . Degrees <$> requireL1 "ELEV_ANG" toFloat l1
  teltrack <- Teltrack <$> requireL1 "TELTRACK" toText l1
  telscan <- fmap Telscan <$> lookupL1 "TELSCAN" toText l1
  ttblangl <- Key . Degrees <$> requireL1 "TTBLANGL" toFloat l1
  ttbltrck <- Ttbltrck <$> requireL1 "TTBLTRCK" toText l1
  dateref <- Key . DateTime <$> requireL1 "DATEREF" toText l1
  obsgeoX <- Key . Meters <$> requireL1 "OBSGEO-X" toFloat l1
  obsgeoY <- Key . Meters <$> requireL1 "OBSGEO-Y" toFloat l1
  obsgeoZ <- Key . Meters <$> requireL1 "OBSGEO-Z" toFloat l1
  rotcomp <- fmap Key <$> lookupL1 "ROTCOMP" toInt l1
  obsVr <- Key . Mps <$> requireL1 "OBS_VR" toFloat l1
  pure $ TelescopeHeader{..}


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

lookupL1 :: (Monad m) => Text -> (Value -> Maybe a) -> Header -> m (Maybe a)
lookupL1 k fromValue h =
  let mk = Fits.lookup k h
   in case fromValue =<< mk of
        Nothing -> pure Nothing
        Just t -> pure (Just t)


requireL1 :: (Error FitsGenError :> es) => Text -> (Value -> Maybe a) -> Header -> Eff es a
requireL1 k fromValue h =
  let mk = Fits.lookup k h
   in case fromValue =<< mk of
        Nothing -> throwError (MissingL1Key (unpack k))
        Just t -> pure t


toDate :: Value -> Maybe DateTime
toDate v = DateTime <$> toText v


data FitsGenError
  = MissingL1Key String
  | MissingL1HDU FilePath
  deriving (Show, Exception)


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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module NSO.Fits.Generate.Headers where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Fits (KeywordRecord (..), toFloat, toInt, toText)
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Text (pack, unpack)
import Data.Text qualified as T
import Debug.Trace
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


-- DONE: automatic type-based comments
-- DONE: custom comments with custom newtype (cleaner)
-- DONE: comments for bitpix, naxes and other auto-gen keywords.
-- DONE: FILENAME - based on input filename
-- DONE: Support optional lifted L1 headers
--
-- DOING: WCS
--
-- TODO: DSETID - the inversion id?
-- TODO: FRAMEVOL - estimate based on the dimensions, bitpix, and average header size
-- TODO: HEADVERS - current version number for the spec
-- TODO: HEAD_URL - create a url that links to this
-- TODO: INFO_URL - what's the difference? Is there another documentation page? Does it have a different spec?
-- TODO: FILE_ID - UUID for this frame - where do I need to store this?
-- TODO: PROV_URL - create a provenance URL page... public domain?
-- TODO: CHECKSUM
-- TODO: DATASUM
--
--
-- TODO: Add support for Doubles to fits-parse? Or just always assume double...

-- COMMENTS -----------------------------
-- 1. Some units (ktype) need a comment. some don't
-- 2. Some field names need units. some don't
--
-- how hard would it be to make custom ones for each?

data PrimaryHeader = PrimaryHeader
  { telapse :: Key Seconds "TELAPSE = DATE-END - DATE-BEG. Not always equal to the exposure time as multiple exposures could be combined"
  , origin :: Key (Constant "National Solar Observatory") "The organization or institution responsible for creating the FITS file."
  , telescope :: Key (Constant "Daniel K. Inouye Solar Telescope") "The telescope used to acquire the data associated with the header."
  , obsrvtry :: Key (Constant "Haleakala High Altitude Observatory Site") "A character string identifying the physical entity, located in a defined location, which provides the resources necessary for the installation of an instrument"
  , network :: Key (Constant "NSF-DKIST") "Organizational entity of a series of instruments with similar characteristics or goals that operates in some coordinated manner or produces data with some common purpose"
  , object :: Object
  , dsetid :: Key (Id Inversion) "Unique ID of the dataset to which the frame belongs"
  , framevol :: Key MB "Size of the frame on disk."
  , proctype :: Key (Constant "L2") "Controlled value list representing the degree of processing the frame has undergone since receipt at the DKIST data center."
  , solarnet :: Key (Constant "1.0") "SOLARNET compliance: 1.0: Fully compliant 0.5: Partially compliant"
  , filename :: Key Text "Name of current file"
  , level :: Key (Constant 2) "The level of the current data"
  , headvers :: Key Text "Version of this spec used during processing"
  , headUrl :: Key Url "Link to documentation for the headers of this frame"
  , infoUrl :: Key Url "Link to documentation for this frame"
  , fileId :: Key Text "Unique ID of this FITS file"
  , provUrl :: Key Url "Link to provenance for this file"
  , dateBeg :: Key DateTime "Start date and time of light exposure for the frame"
  , dateEnd :: Key DateTime "End date and time of light exposure for the frame"
  , dateAvg :: Key DateTime "Date/Time of the midpoint of the frame. (DATE-END - DATE_BEG) / 2"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


-- TELESCOPE --------------------------------------------------------------------------
--

data TelescopeHeader = TelescopeHeader
  { tazimuth :: Key Degrees "Raw Telescope azimuth angle"
  , elevAng :: Key Degrees "Raw Telescope elevation angle"
  , teltrack :: Teltrack
  , telscan :: Maybe Telscan
  , ttblangl :: Key Degrees "Telescope Coude table angle"
  , ttbltrck :: Ttbltrck
  , dateref :: Key DateTime "Time coordinate zero point"
  , obsgeoX :: Key Meters "Observer’s fixed geographic X coordinate"
  , obsgeoY :: Key Meters "Observer’s fixed geographic Y coordinate"
  , obsgeoZ :: Key Meters "Observer’s fixed geographic Z coordinate"
  , rotcomp :: Maybe (Key Int "Solar rotation compensation: 1: On 2: Off")
  , obsVr :: Key Mps "Observer’s outward velocity w.r.t. the Sun"
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
  description = "Scanning Mode of the Telescope."
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
  , wcsaxes :: Key (Constant 3) "Number of axes in the Helioprojective Cartesian WCS description. Note that when WCSAXES > NAXIS, these extra axes correspond to the image as a whole" -- MUST precede other WCS keywords
  , wcsname :: Key (Constant "Helioprojective Cartesian") "Helioprojective Cartesian"
  , lonpole :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


data WCSAxisKeywords (n :: Nat) = WCSAxisKeywords
  { ctype :: Key Text "A string value labeling axis n of the Helioprojective Coordinate system."
  , cunit :: Key Text "The unit of the value contained in CDELTn"
  , crpix :: Key Float "The value field shall contain a floating point number, identifying the location of a reference point along axis n of the Helioprojective coordinate system, in units of the axis index. This value is based upon a counter that runs from 1 to NAXISn with an increment of 1 per pixel. The reference point value need not be that for the center of a pixel nor lie within the actual data array. Use comments to indicate the location of the index point relative to the pixel. DKIST pointing data will be relative to the boresight of the telescope defined by the WFC Context Viewer that will center an image of the GOS pinhole on its detector using a 10 nm wavelength band centered on 525 nm. The same pinhole image will be used by all instruments as reference for determining the pointing of the instrument in relation to the WFC Context Viewer."
  , crval :: Key Float "The value field shall contain a floating point number, giving the value of the coordinate specified by the CTYPEn keyword at the reference point CRPIXn. DKIST values for this entry will be determined at the wavelength of the WFC Context Viewer, which covers a band of 10 nm centered on 525 nm. The value will not be corrected for differential refraction of Earth’s atmosphere."
  , cdelt :: Key Float "Pixel scale of the world coordinate at the reference point along axis n of the Helioprojective coordinate system. This value must not be zero."
  }
  deriving (Generic)
instance (KnownNat n) => HeaderKeywords (WCSAxisKeywords n) where
  headerKeywords =
    fmap modKey . genHeaderKeywords . from
   where
    modKey KeywordRecord{_keyword, _value, _comment} = KeywordRecord{_keyword = addN _keyword, _value, _comment}
    addN k = k <> pack (show (natVal @n Proxy))


data WCSAxis (n :: Nat) = WCSAxis
  { keys :: WCSAxisKeywords n
  , pcs :: DataAxes (PC n)
  }


instance (KnownNat n) => HeaderKeywords (WCSAxis n) where
  headerKeywords ax =
    headerKeywords ax.keys
      <> headerKeywords ax.pcs


type DummyYN = 3
type SlitXN = 2
type DepthN = 1


wcsCommon :: (MonadThrow m) => Header -> m WCSCommon
wcsCommon l1 = do
  lonpole <- Degrees <$> requireL1 "LONPOLE" toFloat l1
  pure $
    WCSCommon
      { lonpole = Key lonpole
      , wcsvalid = Key Constant
      , wcsaxes = Key Constant
      , wcsname = Key Constant
      }


data DataAxes f = DataAxes
  { dummyY :: f DummyYN
  , slitX :: f SlitXN
  , depth :: f DepthN
  }
  deriving (Generic)
instance (KnownNat n) => HeaderKeywords (DataAxes (PC n))
instance HeaderKeywords (DataAxes WCSAxis) where
  headerKeywords a =
    headerKeywords @(WCSAxis DepthN) a.depth
      <> headerKeywords @(WCSAxis SlitXN) a.slitX
      <> headerKeywords @(WCSAxis DummyYN) a.dummyY


wcsAxes :: (MonadThrow m) => Sz Ix2 -> Header -> m (DataAxes WCSAxis)
wcsAxes sz h = do
  dummyY <- wcsDummyY h
  slitX <- wcsSlitX sz h
  depth <- wcsDepth
  pure $ DataAxes{..}


requireWCS :: (MonadThrow m) => Int -> Header -> m (WCSAxisKeywords n)
requireWCS n l1 = do
  crpix <- Key <$> requireL1 (keyN "CRPIX") toFloat l1
  crval <- Key <$> requireL1 (keyN "CRVAL") toFloat l1
  cdelt <- Key <$> requireL1 (keyN "CDELT") toFloat l1
  cunit <- Key <$> requireL1 (keyN "CUNIT") toText l1
  ctype <- Key <$> requireL1 (keyN "CTYPE") toText l1
  pure $ WCSAxisKeywords{cunit, ctype, crpix, crval, cdelt}
 where
  keyN k = k <> pack (show n)


requirePCs :: (MonadThrow m) => Int -> Header -> m (DataAxes (PC n))
requirePCs n l1 = do
  dummyY <- PC <$> requireL1 (pcN n 3) toFloat l1
  slitX <- PC <$> requireL1 (pcN n 1) toFloat l1
  pure $ DataAxes{dummyY, slitX, depth = PC 0}
 where
  pcN :: Int -> Int -> Text
  pcN i j = "PC" <> pack (show i) <> "_" <> pack (show j)


wcsDummyY :: (MonadThrow m) => Header -> m (WCSAxis DummyYN)
wcsDummyY l1 = do
  keys <- requireWCS 3 l1
  pcs <- requirePCs 3 l1
  pure $ WCSAxis{keys, pcs}


-- TODO: Copy PCs
wcsSlitX :: forall m. (MonadThrow m) => Sz Ix2 -> Header -> m (WCSAxis SlitXN)
wcsSlitX sz l1 = do
  scaleUp <- upFactor sz

  keys <- requireWCS 1 l1
  pcs <- requirePCs 1 l1
  traceM $ show (keys.crpix, keys.cdelt)
  pure $ WCSAxis{keys = scale scaleUp keys, pcs}
 where
  upFactor :: Sz Ix2 -> m Float
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


wcsDepth :: (MonadThrow m) => m (WCSAxis DepthN)
wcsDepth = do
  let crpix = Key 12
      crval = Key 0
      cdelt = Key 0.1
      cunit = Key ""
      ctype = Key "TAU--LOG"
  let keys = WCSAxisKeywords{..}
  let pcs = DataAxes{dummyY = PC 0, slitX = PC 0, depth = PC 1.0}
  pure $ WCSAxis{keys, pcs}


-- GENERATE ------------------------------------------------------------
--

add :: Int -> Int -> Int
add a b = a + b


primaryHeader :: (MonadThrow m) => Header -> Id Inversion -> m PrimaryHeader
primaryHeader l1 i = do
  -- can I avoid typing these keys in twice?
  dateBeg <- requireL1 "DATE-BEG" toDate l1
  dateEnd <- requireL1 "DATE-END" toDate l1
  dateAvg <- requireL1 "DATE-AVG" toDate l1
  telapse <- requireL1 "TELAPSE" toFloat l1
  object <- requireL1 "OBJECT" toText l1
  -- teltrack <- requireL1 "TELTRACK" toText l1

  pure $
    PrimaryHeader
      { telapse = Key (Seconds telapse)
      , telescope = Key Constant
      , obsrvtry = Key Constant
      , network = Key Constant
      , object = Object object
      , dsetid = Key i
      , framevol = Key $ MB 123
      , proctype = Key Constant
      , solarnet = Key Constant
      , filename = Key $ frameFilename dateBeg i
      , level = Key Constant
      , headvers = Key "TODO"
      , headUrl = Key (Url "https://TODO")
      , infoUrl = Key (Url "https://TODO")
      , fileId = Key "TODO"
      , provUrl = Key (Url "https://TODO")
      , origin = Key Constant
      , dateBeg = Key dateBeg
      , dateEnd = Key dateEnd
      , dateAvg = Key dateAvg
      }


-- TODO: this belongs in the data, not the primary!
telescopeHeader :: (MonadThrow m) => Header -> m TelescopeHeader
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


lookupL1 :: (MonadThrow m) => Text -> (Value -> Maybe a) -> Header -> m (Maybe a)
lookupL1 k fromValue h =
  let mk = Fits.lookup k h
   in case fromValue =<< mk of
        Nothing -> pure Nothing
        Just t -> pure (Just t)


requireL1 :: (MonadThrow m) => Text -> (Value -> Maybe a) -> Header -> m a
requireL1 k fromValue h =
  let mk = Fits.lookup k h
   in case fromValue =<< mk of
        Nothing -> throwM (MissingL1Key (unpack k))
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


sectionHeader :: Text -> Text -> [HeaderRecord]
sectionHeader title desc =
  [ BlankLine
  , Comment $ "----------------" <> title <> "------------------"
  , Comment desc
  , Comment "-------------------------------------------------"
  ]


section :: (HeaderKeywords a) => Text -> Text -> a -> [HeaderRecord]
section title desc a =
  sectionHeader title desc <> fmap Keyword (headerKeywords a)

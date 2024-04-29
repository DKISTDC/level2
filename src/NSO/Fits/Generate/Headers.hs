{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module NSO.Fits.Generate.Headers where

import Control.Monad.Catch (Exception)
import Data.Fits (KeywordRecord (..), toFloat, toInt, toText)
import Data.List qualified as L
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Text (pack, unpack)
import Data.Text qualified as T
import Debug.Trace
import Effectful
import Effectful.Error.Static
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


-- DONE: automatic type-based comments
-- DONE: custom comments with custom newtype (cleaner)
-- DONE: comments for bitpix, naxes and other auto-gen keywords.
-- DONE: FILENAME - based on input filename
-- DONE: Support optional lifted L1 headers
-- DONE: WCS

-- DOING: Make sure that the data are valid.. Too sleepy today

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
--

add :: Int -> Int -> Int
add a b = a + b


primaryHeader :: (Error FitsGenError :> es) => Header -> Id Inversion -> Eff es PrimaryHeader
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

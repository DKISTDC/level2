{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module NSO.Fits.Generate.Quantities where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString (ByteString)
import Data.Fits (toFloat)
import Data.Kind (Type)
import Data.Massiv.Array (Index, Ix2 (..), IxN (..), Sz (..))
import Data.Text (pack)
import Data.Time.Format.ISO8601 (iso8601Show)
import Effectful
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.DimArray
import NSO.Fits.Generate.Headers
import NSO.Fits.Generate.Headers.Doc as Doc
import NSO.Fits.Generate.Headers.Keywords
import NSO.Fits.Generate.Headers.LiftL1
import NSO.Fits.Generate.Headers.Types
import NSO.Fits.Generate.Headers.WCS
import NSO.Prelude
import Telescope.Fits
import Telescope.Fits.Types (Axes (..), HeaderRecord (..))


type OpticalDepth =
  DataHDUInfo
    "Log of Optical Depth at 500nm"
    "phys.absorption.opticalDepth"
    'Dimensionless


type Temperature =
  DataHDUInfo
    "Temperature"
    "phys.temperature"
    'Kelvin


type ElectronPressure =
  DataHDUInfo
    "Electron Pressure"
    "phys.electron;phys.pressure"
    'N_m2


type Microturbulence =
  DataHDUInfo
    "Microturbulence"
    "phys.veloc.microTurb"
    'Km_s


type MagStrength =
  DataHDUInfo
    "Magnetic Field Strength"
    "phys.magField"
    Tesla


type Velocity =
  DataHDUInfo
    "Line-of-sight Velocity"
    "spect.dopplerVeloc"
    Km_s


type MagInclination =
  DataHDUInfo
    "Magnetic Field Inclination (w.r.t. line-of-sight)"
    "phys.magField;pos.angDistance"
    Deg


type MagAzimuth =
  DataHDUInfo
    "Magnetic Field Azimuth (w.r.t. line-of-sight)"
    "phys.magField;pos.azimuth"
    Deg


type GeoHeight =
  DataHDUInfo
    "Geometric Height above solar surface (tau ~ 1 at 500nm)"
    "phys.distance"
    Km


type GasPressure =
  DataHDUInfo
    "Gas Pressure"
    "phys.pressure"
    N_m2


type Density =
  DataHDUInfo
    "Density"
    "phys.density"
    Kg_m3


data DataHDUAxes = DataHDUAxes
  { naxis :: Key (Constant "3") "Data HDUs have the shape (y, x, depth)"
  , naxis1 :: Naxis "Optical Depth"
  , naxis2 :: Naxis "Slit X"
  , naxis3 :: NaxisY
  }
  deriving (Generic, HeaderDoc)


-- The DataHDUInfo contains static headers EXTNAME, BTYPE and BUNIT
data DataHDUInfo (extName :: Symbol) (btype :: Symbol) (bunit :: Unit) = DataHDUInfo


instance (KnownSymbol ext, KnownSymbol btype, KnownValue bunit) => HeaderKeywords (DataHDUInfo ext btype bunit) where
  headerKeywords _ =
    [ keywordRecord @(ExtName ext) ExtName
    , keywordRecord @(BType btype) BType
    , keywordRecord @(BUnit bunit) BUnit
    ]


instance (KnownSymbol ext, KnownSymbol btype, KnownValue bunit) => HeaderDoc (DataHDUInfo ext btype bunit) where
  headerDoc =
    [ docKey @(ExtName ext)
    , docKey @(BType btype)
    , docKey @(BUnit bunit)
    ]


-- | A header contains info and common items
data DataHDUHeader info
  = DataHDUHeader
  { info :: info
  , common :: DataHDUCommon
  }


-- The Header Docs need to contain info, axes, and common
instance (HeaderKeywords info) => HeaderKeywords (DataHDUHeader info) where
  headerKeywords (DataHDUHeader info common) =
    headerKeywords @info info
      <> headerKeywords common


-- The Header Docs need to contain info, axes, and common
instance (HeaderDoc info) => HeaderDoc (DataHDUHeader info) where
  headerDoc =
    headerDoc @info
      <> (headerDoc @DataHDUAxes)
      <> (headerDoc @DataHDUCommon)


data DataHDUCommon = DataHDUCommon
  { bzero :: BZero
  , bscale :: BScale
  , datamin :: Key Float "The minimum data value"
  , datamax :: Key Float "The maximum data value"
  , date :: Key DateTime "UTC Date/Time of HDU creation, in the form: YYYY-MM-DDThh:mm:ss[.sss…]"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


quantitiesHDUs :: (Error LiftL1Error :> es) => UTCTime -> Header -> Quantities [SlitX, Depth] -> Eff es [ImageHDU]
quantitiesHDUs now l1 q = execWriter $ do
  opticalDepth
  temperature
  electronPressure
  microTurbulence
  magStrength
  velocity
  magInclination
  magAzimuth
  geoHeight
  gasPressure
  density
 where
  opticalDepth = dataHDU @OpticalDepth now l1 DataHDUInfo q.opticalDepth
  temperature = dataHDU @Temperature now l1 DataHDUInfo q.temperature
  electronPressure = dataHDU @ElectronPressure now l1 DataHDUInfo q.electronPressure
  microTurbulence = dataHDU @Microturbulence now l1 DataHDUInfo q.microTurbulence
  magStrength = dataHDU @MagStrength now l1 DataHDUInfo q.magStrength
  velocity = dataHDU @Velocity now l1 DataHDUInfo q.velocity
  magInclination = dataHDU @MagInclination now l1 DataHDUInfo q.magInclination
  magAzimuth = dataHDU @MagAzimuth now l1 DataHDUInfo q.magAzimuth
  geoHeight = dataHDU @GeoHeight now l1 DataHDUInfo q.geoHeight
  gasPressure = dataHDU @GasPressure now l1 DataHDUInfo q.gasPressure
  density = dataHDU @Density now l1 DataHDUInfo q.density


dataHDU
  :: forall info es
   . (HeaderKeywords info, Writer [ImageHDU] :> es, Error LiftL1Error :> es)
  => UTCTime
  -> Header
  -> info
  -> DimArray [SlitX, Depth]
  -> Eff es ()
dataHDU now l1 info res = do
  let darr = encodeArray res.array
  hd <- writeHeader header
  tell [ImageHDU{header = Header hd, dataArray = addDummyAxis darr}]
 where
  header = do
    sectionHeader "L2 Quantity" "Headers describing the physical quantity"
    dataSection now info res

    sectionHeader "WCS" "WCS Related Keywords"
    wcsSection

  wcsSection = do
    wc <- wcsCommon l1
    wm <- wcsAxes @WCSMain (size res.array) l1
    wa <- wcsAxes @A (size res.array) l1
    addKeywords $ headerKeywords wc
    addKeywords $ headerKeywords wm
    addKeywords $ headerKeywords wa


wcsAxes :: forall alt es. (Error LiftL1Error :> es, KnownValue alt) => Sz Ix2 -> Header -> Eff es (QuantityAxes (WCSAxis alt))
wcsAxes sz h = do
  dummyY <- wcsDummyY h
  slitX <- wcsSlitX sz h
  depth <- wcsDepth
  pure $ QuantityAxes{..}


wcsDepth :: (Monad m) => m (WCSAxis alt DepthN)
wcsDepth = do
  let crpix = Key 12
      crval = Key 0
      cdelt = Key 0.1
      cunit = Key ""
      ctype = Key "TAU--LOG"
  let keys = WCSAxisKeywords{..}
  let pcs = QuantityAxes{dummyY = PC 0, slitX = PC 0, depth = PC 1.0}
  pure $ WCSAxis{keys, pcs}


requirePCs :: forall alt n es. (Error LiftL1Error :> es, KnownValue alt) => Int -> Header -> Eff es (QuantityAxes (PC alt n))
requirePCs n l1 = do
  dummyY <- PC <$> requireL1 (pcN n 3) toFloat l1
  slitX <- PC <$> requireL1 (pcN n 1) toFloat l1
  pure $ QuantityAxes{dummyY, slitX, depth = PC 0}
 where
  pcN :: Int -> Int -> Text
  pcN i j = "PC" <> pack (show i) <> "_" <> pack (show j) <> knownValueText @alt


-- statsSection = do
--   sectionHeader "Statistics" "Statistical information about the data array contained in this FITS file"
--   ss <- statsHeader res
--   addKeywords $ headerKeywords ss

addDummyAxis :: DataArray -> DataArray
addDummyAxis DataArray{bitpix, axes, rawData} =
  let Axes as = axes
   in DataArray{bitpix, rawData, axes = Axes $ as <> [1]}


dataSection
  :: (Writer [HeaderRecord] :> es, Index (IndexOf as), HeaderKeywords info)
  => UTCTime
  -> info
  -> DimArray as
  -> Eff es ()
dataSection now info res = do
  cm <- dataCommon now res
  let dat = DataHDUHeader info cm
  addKeywords $ headerKeywords dat


dataCommon :: (Monad m, Index (IndexOf as)) => UTCTime -> DimArray as -> m DataHDUCommon
dataCommon now res = do
  let date = Key . DateTime . pack . iso8601Show $ now
      datamax = Key $ maximum res.array
      datamin = Key $ minimum res.array
  pure $
    DataHDUCommon
      { bzero = BZero
      , bscale = BScale
      , date
      , datamax
      , datamin
      }


-- Decode Quantities ----------------------------------------------------------------------------------

data Quantities (as :: [Type]) = Quantities
  { opticalDepth :: DimArray as
  , temperature :: DimArray as
  , electronPressure :: DimArray as
  , microTurbulence :: DimArray as
  , magStrength :: DimArray as
  , velocity :: DimArray as
  , magInclination :: DimArray as
  , magAzimuth :: DimArray as
  , geoHeight :: DimArray as
  , gasPressure :: DimArray as
  , density :: DimArray as
  }


decodeQuantitiesFrames :: (MonadIO m, MonadThrow m) => ByteString -> m [Quantities [SlitX, Depth]]
decodeQuantitiesFrames inp = do
  res <- decodeInversion inp
  resultsQuantities res


decodeInversion :: (MonadThrow m) => ByteString -> m (DimArray [Quantity, Depth, FrameY, SlitX])
decodeInversion inp = do
  f <- decode inp
  a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ DimArray a


resultsQuantities :: (MonadThrow m) => DimArray [Quantity, Depth, FrameY, SlitX] -> m [Quantities [SlitX, Depth]]
resultsQuantities res = do
  mapM splitQuantitiesM $ splitFrames res


splitQuantitiesM :: (MonadThrow m) => DimArray [Quantity, Depth, SlitX] -> m (Quantities [SlitX, Depth])
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: DimArray [Quantity, Depth, SlitX] -> Maybe (Quantities [SlitX, Depth])
splitQuantities res = do
  let qs = fmap transposeMajor $ outerList res
  [opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density] <- pure qs
  pure Quantities{..}


-- Frames -----------------------------------------------------------------------

-- | Splits any Data Cube into frames when it is the 3/4 dimension
splitFrames :: forall a b d. DimArray [a, b, FrameY, d] -> [DimArray [a, b, d]]
splitFrames res =
  fmap sliceFrame [0 .. numFrames res - 1]
 where
  numFrames :: DimArray [a, b, FrameY, d] -> Int
  numFrames (DimArray arr) =
    let Sz (_ :> _ :> nf :. _) = size arr
     in nf

  sliceFrame :: Int -> DimArray [a, b, d]
  sliceFrame n = sliceM2 n res

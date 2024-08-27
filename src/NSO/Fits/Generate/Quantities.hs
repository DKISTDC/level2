{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module NSO.Fits.Generate.Quantities where

import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Massiv.Array as M (Index, Ix2 (..), IxN (..), Sz (..), map)
import Data.Maybe (isJust)
import Data.Text (pack)
import Data.Time.Format.ISO8601 (iso8601Show)
import Effectful
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.DataCube
import NSO.Fits.Generate.Error
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
  opticalDepth =
    dataHDU @OpticalDepth now l1 DataHDUInfo q.opticalDepth

  temperature =
    dataHDU @Temperature now l1 DataHDUInfo q.temperature

  electronPressure =
    dataHDU @ElectronPressure now l1 DataHDUInfo $
      convert dyneCmToNm q.electronPressure

  microTurbulence =
    dataHDU @Microturbulence now l1 DataHDUInfo $
      convert cmsToKms q.microTurbulence

  magStrength = dataHDU @MagStrength now l1 DataHDUInfo q.magStrength

  velocity =
    dataHDU @Velocity now l1 DataHDUInfo $
      convert cmsToKms q.velocity

  magInclination =
    dataHDU @MagInclination now l1 DataHDUInfo q.magInclination

  magAzimuth =
    dataHDU @MagAzimuth now l1 DataHDUInfo q.magAzimuth

  geoHeight =
    dataHDU @GeoHeight now l1 DataHDUInfo q.geoHeight

  gasPressure =
    dataHDU @GasPressure now l1 DataHDUInfo $
      convert dyneCmToNm q.gasPressure

  density =
    dataHDU @Density now l1 DataHDUInfo $
      convert gcmToKgm q.density

  convert f da =
    DataCube $ M.map f da.array

  cmsToKms = (/ 100000)

  gcmToKgm = (* 1000)

  dyneCmToNm = (/ 10)


dataHDU
  :: forall info es
   . (HeaderKeywords info, Writer [ImageHDU] :> es, Error LiftL1Error :> es)
  => UTCTime
  -> Header
  -> info
  -> DataCube [SlitX, Depth]
  -> Eff es ()
dataHDU now l1 info res = do
  let darr = encodeDataArray res.array
  hd <- writeHeader header
  tell [ImageHDU{header = Header hd, dataArray = addDummyAxis darr}]
 where
  header = do
    sectionHeader "L2 Quantity" "Headers describing the physical quantity"
    dataSection now info res

    sectionHeader "WCS" "WCS Related Keywords"
    wcsSection

  wcsSection = do
    let bx = binnedX

    wm <- wcsAxes @WCSMain bx l1
    wc <- wcsCommon (isWcsValid wm) l1
    addKeywords $ headerKeywords wc
    addKeywords $ headerKeywords wm

    wca <- wcsCommonA l1
    wa <- wcsAxes @A bx l1
    addKeywords $ headerKeywords wca
    addKeywords $ headerKeywords wa

  binnedX =
    let (Sz (newx :. _)) = size res.array
     in BinnedX newx

  isWcsValid :: QuantityAxes alt -> Bool
  isWcsValid axs =
    isJust axs.dummyY.pcs && isJust axs.slitX.pcs && isJust axs.depth.pcs


data QuantityAxes alt = QuantityAxes
  { depth :: QuantityAxis alt Depth
  , slitX :: QuantityAxis alt X
  , dummyY :: QuantityAxis alt Y
  }
  deriving (Generic)
instance AxisOrder QuantityAxes Y where
  axisN = 3
instance AxisOrder QuantityAxes X where
  axisN = 2
instance AxisOrder QuantityAxes Depth where
  axisN = 1
instance (KnownValue alt) => HeaderKeywords (QuantityAxes alt)


data QuantityAxis alt ax = QuantityAxis
  { keys :: WCSAxisKeywords QuantityAxes alt ax
  , pcs :: Maybe (QuantityPCs alt ax)
  }
  deriving (Generic)
instance (KnownValue alt, AxisOrder QuantityAxes ax) => HeaderKeywords (QuantityAxis alt ax)


data QuantityPCs alt ax = QuantityPCs
  { depth :: PC QuantityAxes alt ax Depth
  , slitX :: PC QuantityAxes alt ax X
  , dummyY :: PC QuantityAxes alt ax Y
  }
  deriving (Generic)
instance (KnownValue alt, AxisOrder QuantityAxes ax) => HeaderKeywords (QuantityPCs alt ax)


wcsAxes :: forall alt es. (Error LiftL1Error :> es, KnownValue alt) => BinnedX -> Header -> Eff es (QuantityAxes alt)
wcsAxes bx h = do
  (ax, ay) <- requireWCSAxes h
  pcsl1 <- requirePCs ax ay h

  yk <- wcsDummyY ay h
  xk <- wcsSlitX ax bx h
  depth <- wcsDepth

  pure $
    QuantityAxes
      { dummyY = QuantityAxis{keys = yk, pcs = pcsY pcsl1}
      , slitX = QuantityAxis{keys = xk, pcs = pcsX pcsl1}
      , depth = depth
      }
 where
  pcsY p = do
    guard (isPCsValid p)
    pure QuantityPCs{dummyY = p.yy, slitX = p.yx, depth = PC 0}

  pcsX p = do
    guard (isPCsValid p)
    pure QuantityPCs{dummyY = p.xy, slitX = p.xx, depth = PC 0}


wcsDepth :: (Monad m) => m (QuantityAxis alt n)
wcsDepth = do
  let crpix = Key 12
      crval = Key 0
      cdelt = Key 0.1
      cunit = Key ""
      ctype = Key "TAU--LOG"
  let keys = WCSAxisKeywords{..}
  let pcs = QuantityPCs{dummyY = PC 0, slitX = PC 0, depth = PC 1.0}
  pure $ QuantityAxis{keys, pcs = Just pcs}


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
  -> DataCube as
  -> Eff es ()
dataSection now info res = do
  cm <- dataCommon now res
  let dat = DataHDUHeader info cm
  addKeywords $ headerKeywords dat


dataCommon :: (Monad m, Index (IndexOf as)) => UTCTime -> DataCube as -> m DataHDUCommon
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
  { opticalDepth :: DataCube as
  , temperature :: DataCube as
  , electronPressure :: DataCube as
  , microTurbulence :: DataCube as
  , magStrength :: DataCube as
  , velocity :: DataCube as
  , magInclination :: DataCube as
  , magAzimuth :: DataCube as
  , geoHeight :: DataCube as
  , gasPressure :: DataCube as
  , density :: DataCube as
  }


decodeQuantitiesFrames :: (MonadThrow m, MonadCatch m) => ByteString -> m [Quantities [SlitX, Depth]]
decodeQuantitiesFrames inp = do
  res <- decodeInversion inp
  resultsQuantities res


decodeInversion :: (MonadThrow m, MonadCatch m) => ByteString -> m (DataCube [Quantity, Depth, FrameY, SlitX])
decodeInversion inp = do
  f <- decode inp
  a <- decodeDataArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ DataCube a


resultsQuantities :: (MonadThrow m) => DataCube [Quantity, Depth, FrameY, SlitX] -> m [Quantities [SlitX, Depth]]
resultsQuantities res = do
  mapM splitQuantitiesM $ splitFrames res


splitQuantitiesM :: (MonadThrow m) => DataCube [Quantity, Depth, SlitX] -> m (Quantities [SlitX, Depth])
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: DataCube [Quantity, Depth, SlitX] -> Maybe (Quantities [SlitX, Depth])
splitQuantities res = do
  let qs = fmap transposeMajor $ outerList res
  [opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density] <- pure qs
  pure Quantities{..}


-- Frames -----------------------------------------------------------------------

-- | Splits any Data Cube into frames when it is the 3/4 dimension
splitFrames :: forall a b d. DataCube [a, b, FrameY, d] -> [DataCube [a, b, d]]
splitFrames res =
  fmap sliceFrame [0 .. numFrames res - 1]
 where
  numFrames :: DataCube [a, b, FrameY, d] -> Int
  numFrames (DataCube arr) =
    let Sz (_ :> _ :> nf :. _) = size arr
     in nf

  sliceFrame :: Int -> DataCube [a, b, d]
  sliceFrame n = sliceM2 n res

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
import NSO.Fits.Generate.Headers.Parse
import NSO.Fits.Generate.Headers.Types
import NSO.Fits.Generate.Headers.WCS
import NSO.Prelude
import Telescope.Fits
import Telescope.Fits.Types (Axes (..), HeaderRecord (..))


data QuantityHeader info = QuantityHeader
  { info :: info
  , common :: DataCommon
  , wcs :: WCSHeader
  }


data WCSHeader = WCSHeader
  { common :: WCSCommon
  , axes :: QuantityAxes 'WCSMain
  , commonA :: WCSCommonA
  , axesA :: QuantityAxes 'A
  }


data DataHeader info = DataHeader
  { info :: info
  , common :: DataCommon
  }


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


-- The Header Docs need to contain info, axes, and common
instance (HeaderKeywords info) => HeaderKeywords (DataHeader info) where
  headerKeywords (DataHeader info common) =
    headerKeywords @info info
      <> headerKeywords common


-- The Header Docs need to contain info, axes, and common
instance (HeaderDoc info) => HeaderDoc (DataHeader info) where
  headerDoc =
    headerDoc @info
      <> (headerDoc @DataHDUAxes)
      <> (headerDoc @DataCommon)


data DataCommon = DataCommon
  { bzero :: BZero
  , bscale :: BScale
  , datamin :: Key Float "The minimum data value"
  , datamax :: Key Float "The maximum data value"
  , date :: Key DateTime "UTC Date/Time of HDU creation, in the form: YYYY-MM-DDThh:mm:ss[.sss…]"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


quantities :: (Error ParseKeyError :> es) => SliceXY -> UTCTime -> Header -> QuantitiesData [SlitX, Depth] -> Eff es Quantities
quantities slice now l1 q = do
  opticalDepth <- quantity @OpticalDepth DataHDUInfo q.opticalDepth
  temperature <- quantity @Temperature DataHDUInfo q.temperature
  electronPressure <- quantity @ElectronPressure DataHDUInfo q.electronPressure
  microTurbulence <- quantity @Microturbulence DataHDUInfo q.microTurbulence
  magStrength <- quantity @MagStrength DataHDUInfo q.magStrength
  velocity <- quantity @Velocity DataHDUInfo q.velocity
  magInclination <- quantity @MagInclination DataHDUInfo q.magInclination
  magAzimuth <- quantity @MagAzimuth DataHDUInfo q.magAzimuth
  geoHeight <- quantity @GeoHeight DataHDUInfo q.geoHeight
  gasPressure <- quantity @GasPressure DataHDUInfo q.gasPressure
  density <- quantity @Density DataHDUInfo q.density
  pure $ Quantities{opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density}
 where
  quantity :: forall info es. (Error ParseKeyError :> es, HeaderKeywords info) => info -> QuantityData [SlitX, Depth] info -> Eff es (Quantity info)
  quantity info d = do
    h <- quantityHeader info d
    pure $ Quantity{header = h, image = d.data_}

  quantityHeader
    :: forall hduInfo es
     . (HeaderKeywords hduInfo, Error ParseKeyError :> es)
    => hduInfo
    -> QuantityData [SlitX, Depth] hduInfo
    -> Eff es (QuantityHeader hduInfo)
  quantityHeader info res = do
    common <- dataCommon now res.data_
    wcs <- wcsHeader
    pure $ QuantityHeader{info, common, wcs}

  wcsHeader :: (Error ParseKeyError :> es) => Eff es WCSHeader
  wcsHeader = do
    -- addKeywords $ headerKeywords wc
    -- addKeywords $ headerKeywords wm
    wm <- wcsAxes @WCSMain slice l1
    wc <- wcsCommon (isWcsValid wm) l1

    -- addKeywords $ headerKeywords wca
    -- addKeywords $ headerKeywords wa
    wca <- wcsCommonA l1
    wa <- wcsAxes @A slice l1
    pure $ WCSHeader{common = wc, axes = wm, commonA = wca, axesA = wa}
   where
    isWcsValid :: QuantityAxes alt -> Bool
    isWcsValid axs =
      isJust axs.dummyY.pcs && isJust axs.slitX.pcs && isJust axs.depth.pcs


quantitiesHDUs :: (Error ParseKeyError :> es) => Quantities -> Eff es [ImageHDU]
quantitiesHDUs qs = execWriter $ do
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
  opticalDepth = dataHDU @OpticalDepth id qs.opticalDepth
  temperature = dataHDU @Temperature id qs.temperature
  electronPressure = dataHDU @ElectronPressure dyneCmToNm qs.electronPressure
  microTurbulence = dataHDU @Microturbulence cmsToKms qs.microTurbulence
  magStrength = dataHDU @MagStrength id qs.magStrength
  velocity = dataHDU @MagStrength cmsToKms qs.magStrength
  magInclination = dataHDU @MagInclination id qs.magInclination
  magAzimuth = dataHDU @MagAzimuth id qs.magAzimuth
  geoHeight = dataHDU @GeoHeight id qs.geoHeight
  gasPressure = dataHDU @GasPressure dyneCmToNm qs.gasPressure
  density = dataHDU @Density gcmToKgm qs.density

  convertData :: (Float -> Float) -> Quantity info -> DataCube [SlitX, Depth]
  convertData f (Quantity _ da) =
    DataCube $ M.map f da.array

  cmsToKms = (/ 100000)

  gcmToKgm = (* 1000)

  dyneCmToNm = (/ 10)

  dataHDU
    :: forall hduInfo es
     . (HeaderKeywords hduInfo, Writer [ImageHDU] :> es, Error ParseKeyError :> es)
    => (Float -> Float)
    -> Quantity hduInfo
    -> Eff es ()
  dataHDU f q = do
    let dat = convertData f q
    let darr = encodeDataArray dat.array
    hd <- writeHeader header
    tell [ImageHDU{header = Header hd, dataArray = addDummyAxis darr}]
   where
    header = do
      sectionHeader "L2 Quantity" "Headers describing the physical quantity"
      let dh = DataHeader{common = q.header.common, info = q.header.info}
      addKeywords $ headerKeywords dh

      sectionHeader "WCS" "WCS Related Keywords"
      addKeywords $ headerKeywords q.header.wcs.common
      addKeywords $ headerKeywords q.header.wcs.axes

      addKeywords $ headerKeywords q.header.wcs.commonA
      addKeywords $ headerKeywords q.header.wcs.axesA


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


wcsAxes :: forall alt es. (Error ParseKeyError :> es, KnownValue alt) => SliceXY -> Header -> Eff es (QuantityAxes alt)
wcsAxes s h = do
  (ax, ay) <- requireWCSAxes h
  pcsl1 <- requirePCs ax ay h

  yk <- wcsDummyY ay s h
  xk <- wcsSlitX ax s h
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
  :: (Index (IndexOf as), HeaderKeywords info)
  => UTCTime
  -> info
  -> DataCube as
  -> Eff es (DataHeader info)
dataSection now info res = do
  cm <- dataCommon now res
  pure $ DataHeader info cm


-- addKeywords $ headerKeywords dat

dataCommon :: (Monad m, Index (IndexOf as)) => UTCTime -> DataCube as -> m DataCommon
dataCommon now res = do
  let date = Key . DateTime . pack . iso8601Show $ now
      datamax = Key $ maximum res.array
      datamin = Key $ minimum res.array
  pure $
    DataCommon
      { bzero = BZero
      , bscale = BScale
      , date
      , datamax
      , datamin
      }


-- Decode Quantities ----------------------------------------------------------------------------------

data Quantities' (f :: Type -> Type) = Quantities
  { opticalDepth :: f OpticalDepth
  , temperature :: f Temperature
  , electronPressure :: f ElectronPressure
  , microTurbulence :: f Microturbulence
  , magStrength :: f MagStrength
  , velocity :: f Velocity
  , magInclination :: f MagInclination
  , magAzimuth :: f MagAzimuth
  , geoHeight :: f GeoHeight
  , gasPressure :: f GasPressure
  , density :: f Density
  }


type QuantitiesHeader = Quantities' QuantityHeader
type QuantitiesData (as :: [Type]) = Quantities' (QuantityData as)
newtype QuantityData as info = QuantityData {data_ :: DataCube as}
data Quantity info = Quantity
  { header :: QuantityHeader info
  , image :: DataCube [SlitX, Depth]
  }
type Quantities = Quantities' Quantity


decodeQuantitiesFrames :: (MonadThrow m, MonadCatch m) => ByteString -> m [QuantitiesData [SlitX, Depth]]
decodeQuantitiesFrames inp = do
  res <- decodeInversion inp
  resultsQuantities res


decodeInversion :: (MonadThrow m, MonadCatch m) => ByteString -> m (DataCube [Quantity a, Depth, FrameY, SlitX])
decodeInversion inp = do
  f <- decode inp
  a <- decodeDataArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ DataCube a


resultsQuantities :: (MonadThrow m) => DataCube [Quantity (), Depth, FrameY, SlitX] -> m [QuantitiesData [SlitX, Depth]]
resultsQuantities res = do
  mapM splitQuantitiesM $ splitFrames res


splitQuantitiesM :: (MonadThrow m) => DataCube [Quantity a, Depth, SlitX] -> m (QuantitiesData [SlitX, Depth])
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: DataCube [Quantity a, Depth, SlitX] -> Maybe (QuantitiesData [SlitX, Depth])
splitQuantities res = do
  let qs = fmap transposeMajor $ outerList res
  [od, t, ep, mt, ms, v, mi, ma, gh, gp, d] <- pure qs
  pure
    Quantities
      { opticalDepth = QuantityData od
      , temperature = QuantityData t
      , electronPressure = QuantityData ep
      , microTurbulence = QuantityData mt
      , magStrength = QuantityData ms
      , velocity = QuantityData v
      , magInclination = QuantityData mi
      , magAzimuth = QuantityData ma
      , geoHeight = QuantityData gh
      , gasPressure = QuantityData gp
      , density = QuantityData d
      }


-- Frames -----------------------------------------------------------------------

-- | Splits any Data Cube into frames when it is the 3rd of 4 dimension
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

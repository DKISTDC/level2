{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.DataHDU where

import Data.Text (pack)
import Data.Time.Format.ISO8601 (iso8601Show)
import Effectful
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc as Doc
import NSO.Fits.Generate.Frames
import NSO.Fits.Generate.Headers
import NSO.Fits.Generate.Keywords
import NSO.Fits.Generate.Results
import NSO.Fits.Generate.Types
import NSO.Prelude
import Telescope.Fits
import Telescope.Fits.Types (Axes (..))


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


quantitiesHDUs :: (Error FitsGenError :> es) => UTCTime -> Header -> Quantities [SlitX, Depth] -> Eff es [ImageHDU]
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
   . (HeaderKeywords info, Writer [ImageHDU] :> es, Error FitsGenError :> es)
  => UTCTime
  -> Header
  -> info
  -> Results [SlitX, Depth]
  -> Eff es ()
dataHDU now l1 info res = do
  let darr = encodeArray res.array
  hd <- writeHeader header
  tell [ImageHDU{header = Header hd, dataArray = addDummyAxis darr}]
 where
  header = do
    dataSection
    wcsSection
  -- statsSection

  dataSection = do
    cm <- common
    let dat = DataHDUHeader info cm
    sectionHeader "Data HDU" "Headers describing the physical quantity"
    addKeywords $ headerKeywords dat

  wcsSection = do
    wc <- wcsCommon l1
    wm <- wcsAxes @WCSMain (size res.array) l1
    wa <- wcsAxes @A (size res.array) l1
    sectionHeader "WCS" "WCS Related Keywords"
    addKeywords $ headerKeywords wc
    addKeywords $ headerKeywords wm
    addKeywords $ headerKeywords wa

  -- statsSection = do
  --   sectionHeader "Statistics" "Statistical information about the data array contained in this FITS file"
  --   ss <- statsHeader res
  --   addKeywords $ headerKeywords ss

  common = do
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

  addDummyAxis :: DataArray -> DataArray
  addDummyAxis DataArray{bitpix, axes, rawData} =
    let Axes as = axes
     in DataArray{bitpix, rawData, axes = Axes $ as <> [1]}

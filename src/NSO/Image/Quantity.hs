{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Image.Quantity where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Fixed (mod')
import Data.Massiv.Array as M (Index, Ix2 (..), IxN (..), Sz (..), map)
import Data.Text (pack)
import Data.Time.Format.ISO8601 (iso8601Show)
import Effectful
import Effectful.Error.Static
import GHC.Generics
import GHC.TypeLits
import NSO.Image.DataCube
import NSO.Image.Headers
import NSO.Image.Headers.Doc as Doc
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Image.Headers.WCS
import NSO.Prelude
import Telescope.Asdf hiding (Key)
import Telescope.Data.Axes (Axes (..), AxisOrder (..))
import Telescope.Data.KnownText
import Telescope.Data.WCS (WCSAlt (..))
import Telescope.Fits as Fits
import Telescope.Fits.Header (Header (..), HeaderRecord (..))


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


instance (KnownSymbol ext, KnownSymbol btype, KnownValue bunit) => ToHeader (DataHDUInfo ext btype bunit) where
  toHeader _ =
    Header $
      fmap
        Keyword
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


instance FromHeader (DataHDUInfo ext btye bunit) where
  parseHeader _ = pure DataHDUInfo


data DataHeader info = DataHeader
  { info :: info
  , common :: DataCommon
  }


-- The library already inserts the NAXIS headers. No need to write them manually
instance (ToHeader info) => ToHeader (DataHeader info) where
  toHeader (DataHeader info common) =
    toHeader info <> toHeader common


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
  deriving (Generic, HeaderDoc, ToHeader, FromHeader)


quantities :: (Error QuantityError :> es) => SliceXY -> UTCTime -> Header -> Quantities (QuantityImage [SlitX, Depth]) -> Eff es (Quantities Quantity)
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
  quantity :: forall info es. (Error QuantityError :> es, ToHeader info) => info -> QuantityImage [SlitX, Depth] info -> Eff es (Quantity info)
  quantity info d = do
    h <- quantityHeader info d
    pure $ Quantity{header = h, image = d.image}

  quantityHeader
    :: forall info es
     . (ToHeader info, Error QuantityError :> es)
    => info
    -> QuantityImage [SlitX, Depth] info
    -> Eff es (QuantityHeader info)
  quantityHeader info res = do
    common <- dataCommon now res.image
    wcs <- wcsHeader slice l1
    pure $ QuantityHeader{info, common, wcs}


wcsHeader :: (Error QuantityError :> es) => SliceXY -> Header -> Eff es (WCSHeader QuantityAxes)
wcsHeader slice l1 = runParseError InvalidWCS $ do
  wm <- wcsAxes @WCSMain slice l1
  wc <- wcsCommon (isWcsValid wm) l1
  wca <- wcsCommonA l1
  wa <- wcsAxes @A slice l1
  pure $ WCSHeader{common = wc, axes = wm, commonA = wca, axesA = wa}
 where
  isWcsValid :: QuantityAxes alt -> Bool
  isWcsValid axs = fromMaybe False $ do
    pcx <- axs.slitX.pcs
    pcy <- axs.dummyY.pcs
    pure $ isPCsValid $ toPCXY pcx pcy


toPCXY :: QuantityPCs alt X -> QuantityPCs alt Y -> PCXY QuantityAxes alt
toPCXY pcx pcy =
  PCXY{xx = pcx.slitX, xy = pcx.dummyY, yx = pcy.slitX, yy = pcy.dummyY}


quantityHeaders :: Quantities Quantity -> Quantities QuantityHeader
quantityHeaders = mapQuantities (.header)


quantityHDUs :: Quantities Quantity -> [ImageHDU]
quantityHDUs qs = runPureEff $ do
  opticalDepth <- dataHDU @OpticalDepth qs.opticalDepth
  temperature <- dataHDU @Temperature qs.temperature
  electronPressure <- dataHDU @ElectronPressure $ convertData dyneCmToNm qs.electronPressure
  microTurbulence <- dataHDU @Microturbulence $ convertData cmsToKms qs.microTurbulence
  magStrength <- dataHDU @MagStrength $ convertData gaussToTesla qs.magStrength
  magInclination <- dataHDU @MagInclination qs.magInclination
  magAzimuth <- dataHDU @MagAzimuth $ convertData forcePositive360 qs.magAzimuth
  geoHeight <- dataHDU @GeoHeight qs.geoHeight
  gasPressure <- dataHDU @GasPressure $ convertData dyneCmToNm qs.gasPressure
  density <- dataHDU @Density $ convertData gcmToKgm qs.density
  velocity <- dataHDU @Velocity $ convertData cmsToKms qs.velocity
  pure $ toList (.hdu) $ Quantities{..}
 where
  convertData :: (Float -> Float) -> Quantity info -> Quantity info
  convertData f (Quantity header da) =
    Quantity
      { header
      , image = DataCube $ M.map f da.array
      }

  dataHDU
    :: forall info es
     . (ToHeader info)
    => Quantity info
    -> Eff es (QuantityHDU info)
  dataHDU q = do
    let darr = encodeDataArray q.image.array
    pure $ QuantityHDU $ ImageHDU{header = toHeader q.header, dataArray = addDummyAxis darr}


cmsToKms :: Float -> Float
cmsToKms = (/ 100000)


gcmToKgm :: Float -> Float
gcmToKgm = (* 1000)


dyneCmToNm :: Float -> Float
dyneCmToNm = (/ 10)


gaussToTesla :: Float -> Float
gaussToTesla = (/ 10000)


forcePositive360 :: Float -> Float
forcePositive360 deg = deg `mod'` 360


data QuantityAxes alt = QuantityAxes
  { depth :: QuantityAxis alt Depth
  , slitX :: QuantityAxis alt X
  , dummyY :: QuantityAxis alt Y
  }
  deriving (Generic, Show)
instance AxisOrder (HDUAxis QuantityAxes Y) where
  axisN = 3
instance AxisOrder (HDUAxis QuantityAxes X) where
  axisN = 2
instance AxisOrder (HDUAxis QuantityAxes Depth) where
  axisN = 1
instance (KnownText alt) => ToHeader (QuantityAxes alt) where
  toHeader (QuantityAxes d x y) = mconcat [toHeader d, toHeader x, toHeader y]


data QuantityAxis alt ax = QuantityAxis
  { keys :: WCSAxisKeywords QuantityAxes alt ax
  , pcs :: Maybe (QuantityPCs alt ax)
  }
  deriving (Generic, Show)
instance (KnownText alt, AxisOrder (HDUAxis QuantityAxes ax)) => ToHeader (QuantityAxis alt ax) where
  toHeader (QuantityAxis keys pcs) = toHeader (toWCSAxis @(HDUAxis QuantityAxes ax) keys) <> toHeader pcs


data QuantityPCs alt ax = QuantityPCs
  { depth :: PC QuantityAxes alt ax Depth
  , slitX :: PC QuantityAxes alt ax X
  , dummyY :: PC QuantityAxes alt ax Y
  }
  deriving (Generic, Show)
instance (KnownText alt, AxisOrder (HDUAxis QuantityAxes ax)) => ToHeader (QuantityPCs alt ax) where
  toHeader pcs =
    Header
      [ Keyword $ keywordRecord pcs.depth
      , Keyword $ keywordRecord pcs.slitX
      , Keyword $ keywordRecord pcs.dummyY
      ]


wcsAxes :: forall alt es. (Error ParseError :> es, KnownText alt) => SliceXY -> Header -> Eff es (QuantityAxes alt)
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
    pure $ QuantityPCs{dummyY = p.yy, slitX = p.yx, depth = PC 0}

  pcsX p = do
    guard (isPCsValid p)
    pure $ QuantityPCs{dummyY = p.xy, slitX = p.xx, depth = PC 0}


wcsDepth :: (Monad m) => m (QuantityAxis alt n)
wcsDepth = do
  let crpix = Key 12
      crval = Key 0
      cdelt = Key (-0.1)
      cunit = Key ""
      ctype = Key "LOGTAU"
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


-- dataSection
--   :: (Index (IndexOf as), HeaderKeywords info)
--   => UTCTime
--   -> info
--   -> DataCube as
--   -> Eff es (DataHeader info)
-- dataSection now info res = do
--   cm <- dataCommon now res
--   pure $ DataHeader info cm

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

data Quantities (f :: Type -> Type) = Quantities
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
  deriving (Generic)
instance ToAsdf (Quantities Quantity)


newtype QuantityImage as info = QuantityImage {image :: DataCube as}


newtype QuantityHDU info = QuantityHDU {hdu :: ImageHDU}


data Quantity info = Quantity
  { header :: QuantityHeader info
  , image :: DataCube [SlitX, Depth]
  }


instance ToAsdf (Quantity x) where
  toValue _ = Null


data QuantityHeader info = QuantityHeader
  { info :: info
  , common :: DataCommon
  , wcs :: WCSHeader QuantityAxes
  }
  deriving (Generic)
instance (ToHeader info) => ToHeader (QuantityHeader info) where
  toHeader h = writeHeader $ do
    sectionHeader "L2 Quantity" "Headers describing the physical quantity"
    let dh = DataHeader{common = h.common, info = h.info}
    addKeywords dh

    sectionHeader "WCS" "WCS Related Keywords"
    addKeywords h.wcs.common
    addKeywords h.wcs.axes

    addKeywords h.wcs.commonA
    addKeywords h.wcs.axesA


fromList :: (forall x. a -> f x) -> [a] -> Maybe (Quantities f)
fromList f qs = do
  [od, t, ep, mt, ms, v, mi, ma, gh, gp, d] <- pure qs
  pure $
    Quantities
      { opticalDepth = f od
      , temperature = f t
      , electronPressure = f ep
      , microTurbulence = f mt
      , magStrength = f ms
      , velocity = f v
      , magInclination = f mi
      , magAzimuth = f ma
      , geoHeight = f gh
      , gasPressure = f gp
      , density = f d
      }


-- TEST: easy to test this for the presence of all of the fields
toList :: (forall x. f x -> a) -> Quantities f -> [a]
toList f qs =
  [ f qs.opticalDepth
  , f qs.temperature
  , f qs.electronPressure
  , f qs.microTurbulence
  , f qs.magStrength
  , f qs.velocity
  , f qs.magInclination
  , f qs.magAzimuth
  , f qs.geoHeight
  , f qs.gasPressure
  , f qs.density
  ]


mapQuantities :: (forall x. f x -> g x) -> Quantities f -> Quantities g
mapQuantities f qs =
  Quantities
    { opticalDepth = f qs.opticalDepth
    , temperature = f qs.temperature
    , electronPressure = f qs.electronPressure
    , microTurbulence = f qs.microTurbulence
    , magStrength = f qs.magStrength
    , velocity = f qs.velocity
    , magInclination = f qs.magInclination
    , magAzimuth = f qs.magAzimuth
    , geoHeight = f qs.geoHeight
    , gasPressure = f qs.gasPressure
    , density = f qs.density
    }


-- | Decodes the L2 input file containing the quantities: inv_res_mod
decodeQuantitiesFrames :: (Error QuantityError :> es) => ByteString -> Eff es [Quantities (QuantityImage [SlitX, Depth])]
decodeQuantitiesFrames inp = do
  res <- decodeInversion inp
  resultsQuantities res


decodeInversion :: (Error QuantityError :> es) => ByteString -> Eff es (DataCube [Quantity a, Depth, FrameY, SlitX])
decodeInversion inp = do
  f <- Fits.decode inp
  a <- decodeDataArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ DataCube a


resultsQuantities :: (Error QuantityError :> es) => DataCube [Quantity (), Depth, FrameY, SlitX] -> Eff es [Quantities (QuantityImage [SlitX, Depth])]
resultsQuantities res = do
  mapM splitQuantitiesM $ splitFrames res


splitQuantitiesM :: (Error QuantityError :> es) => DataCube [Quantity a, Depth, SlitX] -> Eff es (Quantities (QuantityImage [SlitX, Depth]))
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwError $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: DataCube [Quantity a, Depth, SlitX] -> Maybe (Quantities (QuantityImage [SlitX, Depth]))
splitQuantities res = do
  let qs = fmap transposeMajor $ outerList res
  fromList QuantityImage qs


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


data QuantityError
  = InvalidFrameShape (Sz Ix3)
  | InvalidWCS ParseError
  deriving (Show, Exception, Eq)

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Image.Fits.Quantity where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Fixed (mod')
import Data.Massiv.Array as M (Index, Sz (..), map)
import Effectful
import Effectful.Error.Static
import Effectful.Log
import GHC.Generics
import NSO.Image.Blanca (splitFrameY)
import NSO.Image.Headers
import NSO.Image.Headers.DataCommon
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Image.Headers.WCS
import NSO.Image.Types.Frame (Depth, FrameY, SlitX, Stokes)
import NSO.Image.Types.Quantity
import NSO.Prelude
import Telescope.Data.Axes (Axes (..), AxisOrder (..))
import Telescope.Data.DataCube
import Telescope.Data.KnownText
import Telescope.Data.WCS (WCSAlt (..))
import Telescope.Fits as Fits
import Telescope.Fits.Header (Header (..), HeaderRecord (..))


quantities :: (Error QuantityError :> es) => SliceXY -> UTCTime -> Header -> Quantities (QuantityImage [SlitX, Depth]) -> Eff es (Quantities QuantityFrameFits)
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
  quantity :: forall hduInfo es. (Error QuantityError :> es, ToHeader hduInfo) => hduInfo -> QuantityImage [SlitX, Depth] hduInfo -> Eff es (QuantityFrameFits hduInfo)
  quantity hduInfo d = do
    h <- quantityHeader hduInfo d
    pure $ QuantityFrameFits{header = h, image = d.image}

  quantityHeader
    :: forall hduInfo es
     . (ToHeader hduInfo, Error QuantityError :> es)
    => hduInfo
    -> QuantityImage [SlitX, Depth] hduInfo
    -> Eff es (QuantityHeader hduInfo)
  quantityHeader hduInfo res = do
    common <- dataCommon now res.image
    wcs <- wcsHeader slice l1
    pure $ QuantityHeader{hduInfo, common, wcs}


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


quantityHeaders :: Quantities QuantityFrameFits -> Quantities QuantityHeader
quantityHeaders = mapQuantities (.header)


quantityHDUs :: Quantities QuantityFrameFits -> [DataHDU]
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
  convertData :: (Float -> Float) -> QuantityFrameFits info -> QuantityFrameFits info
  convertData f (QuantityFrameFits header da) =
    QuantityFrameFits
      { header
      , image = DataCube $ M.map f da.array
      }

  dataHDU
    :: forall info es
     . (ToHeader info)
    => QuantityFrameFits info
    -> Eff es (QuantityHDU info)
  dataHDU q = do
    let darr = encodeDataArray q.image.array
    pure $ QuantityHDU $ DataHDU{header = toHeader q.header, dataArray = addDummyAxis darr}


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

dataCommon :: (Monad m, Index (IndexOf as)) => UTCTime -> DataCube as Float -> m DataCommon
dataCommon now res = do
  let date = Key now
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

-- instance ToAsdf (Quantities Quantity)
-- instance ToAsdf (Quantities AlignedAxes)

newtype QuantityImage as info = QuantityImage {image :: DataCube as Float}


newtype QuantityHDU info = QuantityHDU {hdu :: DataHDU}


data QuantityFrameFits info = QuantityFrameFits
  { header :: QuantityHeader info
  , image :: DataCube [SlitX, Depth] Float
  }


data QuantityHeader hduInfo = QuantityHeader
  { hduInfo :: hduInfo
  , common :: DataCommon
  , wcs :: WCSHeader QuantityAxes
  }
  deriving (Generic)
instance (ToHeader info) => ToHeader (QuantityHeader info) where
  toHeader h = writeHeader $ do
    sectionHeader "L2 Quantity" "Headers describing the physical quantity"
    addKeywords h.hduInfo
    addKeywords h.common

    addKeywords h.wcs


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


quantitiesFrom :: (forall x. a -> f x) -> a -> Quantities f
quantitiesFrom val a =
  Quantities
    { opticalDepth = val a
    , temperature = val a
    , electronPressure = val a
    , microTurbulence = val a
    , magStrength = val a
    , velocity = val a
    , magInclination = val a
    , magAzimuth = val a
    , geoHeight = val a
    , gasPressure = val a
    , density = val a
    }


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
decodeQuantitiesFrames :: (Error QuantityError :> es, Log :> es) => ByteString -> Eff es [Quantities (QuantityImage [SlitX, Depth])]
decodeQuantitiesFrames inp = do
  res <- decodeInversion inp
  resultsQuantities res


decodeInversion :: (Error QuantityError :> es, Log :> es) => ByteString -> Eff es (DataCube [Quantity, Depth, FrameY, SlitX] Float)
decodeInversion inp = do
  f <- Fits.decode inp
  a <- decodeDataArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ DataCube a


resultsQuantities :: (Error QuantityError :> es) => DataCube [Quantity, Depth, FrameY, SlitX] Float -> Eff es [Quantities (QuantityImage [SlitX, Depth])]
resultsQuantities res = do
  mapM splitQuantitiesM $ splitFrameY res


splitQuantitiesM :: (Error QuantityError :> es) => DataCube [Quantity, Depth, SlitX] Float -> Eff es (Quantities (QuantityImage [SlitX, Depth]))
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwError $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: DataCube [Quantity, Depth, SlitX] Float -> Maybe (Quantities (QuantityImage [SlitX, Depth]))
splitQuantities res = do
  let qs = fmap transposeMajor $ outerList res
  fromList QuantityImage qs


-- Frames -----------------------------------------------------------------------

data QuantityError
  = InvalidFrameShape (Sz Ix3)
  | InvalidWCS ParseError
  deriving (Show, Exception, Eq)

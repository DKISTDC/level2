{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.GWCS where

import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Array, D, Ix2, Ix3)
import Data.Massiv.Array qualified as M
import NSO.Image.Fits.Profile
import NSO.Image.Fits.Quantity
import NSO.Image.GWCS.L1GWCS (HPLat, HPLon, L1GWCS (..), L1WCSTransform (..), Time, Zero, l1WCSTransform)
import NSO.Image.Headers (Observation (..), Obsgeo (..))
import NSO.Image.Headers.Types (Degrees (..), Key (..), Meters (..))
import NSO.Image.Headers.WCS (PC (..), PCXY (..), WCSAxisKeywords (..), WCSCommon (..), WCSHeader (..), Wav, X, Y, toWCSAxis)
import NSO.Image.Primary (PrimaryHeader (..))
import NSO.Image.Types.Frame (Depth, Frames (..), Stokes, middleFrame)
import NSO.Image.Types.Quantity (OpticalDepth)
import NSO.Prelude as Prelude
import Numeric (showFFloat)
import Telescope.Asdf (Anchor (..), ToAsdf (..), Value (..))
import Telescope.Asdf.Core (Quantity (..), Unit (Arcseconds, Pixel, Unit))
import Telescope.Asdf.Core qualified as Unit
import Telescope.Asdf.GWCS as GWCS
import Telescope.Asdf.NDArray (ToNDArray (..))
import Telescope.Data.KnownText
import Telescope.Data.WCS (WCSAlt (..), WCSAxis (..))


transformProfile
  :: WCSCommon
  -> ProfileAxes 'WCSMain
  -> Transform (Pix Stokes, Pix Wav, Pix X, Pix Y) (Pix Stokes, Linear Wav, Alpha, Delta)
transformProfile common axes =
  transformIdentity <&> transformWav <&> transformSpatial common (toWCSAxis axes.slitX.keys) (toWCSAxis axes.dummyY.keys) pcs
 where
  transformWav :: Transform (Pix Wav) (Linear Wav)
  transformWav = wcsLinear $ wcsToNanometers (toWCSAxis axes.wavelength.keys)

  transformIdentity :: (ToAxes (Pix a)) => Transform (Pix a) (Pix a)
  transformIdentity = GWCS.identity

  pcs :: PCXY ProfileAxes 'WCSMain
  pcs = fromMaybe identityPCXY $ do
    pcx <- axes.slitX.pcs
    pcy <- axes.dummyY.pcs
    pure $ PCXY{xx = pcx.slitX, xy = pcx.dummyY, yx = pcy.slitX, yy = pcy.dummyY}


-- Quantity ---------------------------------------------------

transformQuantity
  :: L1WCSTransform
  -> Frames (QuantityAxes 'WCSMain)
  -> Transform (Pix Depth, Pix X, Pix Y) (Linear Depth, HPLon, HPLat, Time)
transformQuantity l1trans axes =
  dropUnusedZeros fullTransform
 where
  fullTransform :: Transform (Pix Depth, Pix X, Pix Y) (Linear Depth, HPLon, HPLat, Time, Zero Wav, Zero Stokes)
  fullTransform =
    let mid = middleFrame axes
     in transformOpticalDepth (toWCSAxis mid.depth.keys) <&> l1WCSTransform l1trans

  dropUnusedZeros :: Transform inp (a, b, c, d, Zero z1, Zero z2) -> Transform inp (w, x, y, z)
  dropUnusedZeros (Transform t) = Transform t


identityPCXY :: PCXY s 'WCSMain
identityPCXY =
  PCXY{xx = PC 1, xy = PC 0, yx = PC 0, yy = PC 1}


data LinearOpticalDepth = LinearOpticalDepth {intercept :: Quantity, slope :: Quantity}
  deriving (Generic)
instance ToAsdf LinearOpticalDepth where
  schema _ = "!transform/linear1d-1.0.0"


transformOpticalDepth :: WCSAxis 'WCSMain Depth -> Transform (Pix Depth) (Linear Depth)
transformOpticalDepth wcsOD =
  let Intercept i = wcsIntercept wcsOD
      s = wcsOD.cdelt
   in transform $ LinearOpticalDepth (Quantity Pixel (factor1digit i)) (Quantity (Unit "pix.pixel**-1") (factor1digit s))
 where
  -- intercept: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 pix, value: 853.7012736084624}
  -- slope: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 pix.pixel**-1, value: 9.99852488051306e-4}
  -- linear (wcsIntercept wcsOD) (Scale $ factor1digit wcsOD.cdelt)

  factor1digit :: Double -> Value
  factor1digit d = String $ cs $ showFFloat (Just 1) d "" -- fromIntegral (round @Double @Integer (d * 10)) / 10


transformSpatial
  :: WCSCommon
  -> WCSAxis 'WCSMain X
  -> WCSAxis 'WCSMain Y
  -> PCXY s 'WCSMain
  -> Transform (Pix X, Pix Y) (Alpha, Delta)
transformSpatial common wcsX wcsY pcs = linearXY |> rotate pcMatrix |> project Pix2Sky |> sky
 where
  pcMatrix :: Array D Ix2 Double
  pcMatrix =
    M.delay $
      M.fromLists' @M.P
        M.Seq
        [ [pcs.xx.value, pcs.xy.value]
        , [pcs.yx.value, pcs.yy.value]
        ]

  linearXY :: Transform (Pix X, Pix Y) (Linear X, Linear Y)
  linearXY = spatialLinear (wcsToDegrees wcsX) <&> spatialLinear (wcsToDegrees wcsY)

  sky :: Transform (Phi, Theta) (Alpha, Delta)
  sky = celestial (Lat $ arcsecondsToDegrees wcsX.crval) (Lon $ arcsecondsToDegrees wcsY.crval) (lonPole common)

  -- we don't use wcsLinear, because it includes the crval, which is already taken into account in the sky transform
  spatialLinear :: forall a alt x. (ToAxes a) => WCSAxis alt x -> Transform (Pix a) (Linear a)
  spatialLinear wcs = linear (spatialIntercept wcs) (Scale wcs.cdelt)

  spatialIntercept :: WCSAxis alt axis -> Intercept
  spatialIntercept w =
    -- crpix is 1-indexed, need to switch to zero
    -- don't use crval, it's already incorporated into the sky transform
    Intercept $ negate (w.cdelt * (w.crpix - 1))


lonPole :: WCSCommon -> LonPole
lonPole common =
  let (Key (Degrees d)) = common.lonpole
   in LonPole d


-- -- linear (wcsShift wcs) (wcsScale wcs)
-- linear (Shift (realToFrac wcs.crpix.ktype)) (wcsScale wcs)

-- wcsScale :: WCSAxisKeywords s alt x -> Scale
-- wcsScale wcs =
--   Scale (realToFrac $ arcsecondsToDegrees wcs.cdelt.ktype)

wcsToDegrees :: WCSAxis alt axis -> WCSAxis alt axis
wcsToDegrees WCSAxis{ctype, cunit, crpix, crval, cdelt} =
  WCSAxis
    { ctype
    , cunit
    , crpix
    , crval = arcsecondsToDegrees crval
    , cdelt = arcsecondsToDegrees cdelt
    }


arcsecondsToDegrees :: Double -> Double
arcsecondsToDegrees f = f / 3600


wcsToNanometers :: WCSAxis alt Wav -> WCSAxis alt Wav
wcsToNanometers WCSAxis{ctype, cunit, crpix, crval, cdelt} =
  WCSAxis
    { ctype
    , cunit
    , crpix
    , crval = toNanometers crval
    , cdelt = toNanometers cdelt
    }
 where
  toNanometers n = n / 10 ^ (9 :: Int)


wcsShift :: WCSAxisKeywords s alt a -> Shift a
wcsShift wcs =
  Shift (realToFrac $ negate (wcs.crpix.ktype - 1))


quantityGWCS :: L1GWCS -> Frames PrimaryHeader -> Frames (QuantityHeader OpticalDepth) -> QuantityGWCS
quantityGWCS l1gwcs primaries quants =
  let firstPrim = head primaries.frames
   in QuantityGWCS $ GWCS inputStep (outputStep firstPrim)
 where
  inputStep :: GWCSStep CoordinateFrame
  inputStep = GWCSStep pixelFrame (Just (transformQuantity l1gwcs.transform (fmap axis quants)).transformation)
   where
    axis :: QuantityHeader x -> QuantityAxes 'WCSMain
    axis q = q.wcs.axes

    pixelFrame :: CoordinateFrame
    pixelFrame =
      CoordinateFrame
        { name = "pixel"
        , axes =
            NE.fromList
              [ FrameAxis 0 "optical_depth" (AxisType "PIXEL") Pixel
              , FrameAxis 1 "slit_x" (AxisType "PIXEL") Pixel
              , FrameAxis 2 "frame_y" (AxisType "PIXEL") Pixel
              ]
        }

  outputStep :: PrimaryHeader -> GWCSStep (CompositeFrame (CoordinateFrame, CelestialFrame HelioprojectiveFrame, TemporalFrame))
  outputStep h0 = GWCSStep compositeFrame Nothing
   where
    compositeFrame =
      CompositeFrame (opticalDepthFrame, celestialFrame 1 l1gwcs.helioprojectiveFrame, temporalFrame)

    opticalDepthFrame =
      CoordinateFrame
        { name = "optical_depth"
        , axes =
            NE.fromList
              [ FrameAxis 0 "optical_depth" "optical_depth" Pixel
              ]
        }

    temporalFrame =
      TemporalFrame
        { name = "temporal"
        , axisOrder = 3
        , time = h0.observation.dateAvg.ktype
        }


type OpticalDepthFrame = CoordinateFrame


newtype QuantityGWCS
  = QuantityGWCS
      (GWCS CoordinateFrame (CompositeFrame (OpticalDepthFrame, CelestialFrame HelioprojectiveFrame, TemporalFrame)))
instance KnownText QuantityGWCS where
  knownText = "quantityGWCS"


instance ToAsdf QuantityGWCS where
  schema (QuantityGWCS gwcs) = schema gwcs
  anchor _ = Just $ Anchor $ knownText @QuantityGWCS
  toValue (QuantityGWCS gwcs) = toValue gwcs


-- helioprojectiveFrame :: Observation -> Obsgeo -> HelioprojectiveFrame
-- helioprojectiveFrame obs obsgeo =
--   HelioprojectiveFrame
--     { coordinates = Cartesian3D (coord obsgeo.obsgeoX) (coord obsgeo.obsgeoY) (coord obsgeo.obsgeoZ)
--     , observation =
--         HelioObservation
--           { obstime = obs.dateBeg.ktype
--           , rsun = Unit.Quantity Unit.Kilometers (Integer 695700)
--           }
--     }
--  where
--   coord :: Key Meters desc -> Unit.Quantity
--   coord (Key (Meters m)) = Unit.Quantity Unit.Meters $ toValue m

celestialFrame :: Int -> HelioprojectiveFrame -> CelestialFrame HelioprojectiveFrame
celestialFrame n helioFrame =
  CelestialFrame
    { name = "helioprojective"
    , referenceFrame = helioFrame
    , axes =
        NE.fromList
          [ FrameAxis n "helioprojective longitude" (AxisType "pos.helioprojective.lon") Unit.Arcseconds
          , FrameAxis (n + 1) "helioprojective latitude" (AxisType "pos.helioprojective.lat") Unit.Arcseconds
          ]
    }


--
-- reference_frame: !<tag:sunpy.org:sunpy/coordinates/frames/helioprojective-1.0.0>
--   frame_attributes:
--     observer: !<tag:sunpy.org:sunpy/coordinates/frames/heliographic_stonyhurst-1.1.0>
--       data: !<tag:astropy.org:astropy/coordinates/representation-1.1.0>
--         components:
--           x: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 m,
--             value: 151718470759.01736}
--           y: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 m,
--             value: 936374.8961084613}
--           z: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 m,
--             value: -1238552794.1080718}
--         type: CartesianRepresentation
--       frame_attributes:
--         obstime: !time/time-1.1.0 2022-06-02T21:47:26.641
--         rsun: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 km,
--           value: 695700.0}
--     obstime: !time/time-1.1.0 2022-06-02T21:47:26.641
--     rsun: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 km,
--       value: 695700.0}
-- unit: [!unit/unit-1.0.0 deg, !unit/unit-1.0.0 deg]

profileGWCS :: L1GWCS -> WCSHeader ProfileAxes -> ProfileGWCS fit
profileGWCS l1gwcs wcs = ProfileGWCS $ GWCS (inputStep wcs.common wcs.axes) outputStep
 where
  inputStep :: WCSCommon -> ProfileAxes 'WCSMain -> GWCSStep CoordinateFrame
  inputStep common axes = GWCSStep pixelFrame (Just (transformProfile common axes).transformation)
   where
    pixelFrame :: CoordinateFrame
    pixelFrame =
      CoordinateFrame
        { name = "pixel"
        , axes =
            NE.fromList
              [ FrameAxis 0 "stokes" (AxisType "PIXEL") Pixel
              , FrameAxis 1 "wavelength" (AxisType "PIXEL") Pixel
              , FrameAxis 2 "slit_x" (AxisType "PIXEL") Pixel
              , FrameAxis 3 "frame_y" (AxisType "PIXEL") Pixel
              ]
        }

  outputStep :: GWCSStep (CompositeFrame (StokesFrame, SpectralFrame, CelestialFrame HelioprojectiveFrame))
  outputStep = GWCSStep compositeFrame Nothing
   where
    compositeFrame =
      CompositeFrame (stokesFrame, spectralFrame, celestialFrame 2 l1gwcs.helioprojectiveFrame)

    stokesFrame =
      StokesFrame
        { name = "polarization state"
        , axisOrder = 0
        }

    spectralFrame =
      SpectralFrame
        { name = "wavelength"
        , axisOrder = 1
        }


newtype ProfileGWCS fit
  = ProfileGWCS
      (GWCS CoordinateFrame (CompositeFrame (StokesFrame, SpectralFrame, CelestialFrame HelioprojectiveFrame)))


instance (KnownText fit) => KnownText (ProfileGWCS fit) where
  knownText = "ProfileGWCS" <> knownText @fit


instance (KnownText fit) => ToAsdf (ProfileGWCS fit) where
  schema (ProfileGWCS gwcs) = schema gwcs
  anchor _ = Just $ Anchor $ knownText @(ProfileGWCS fit)
  toValue (ProfileGWCS gwcs) = toValue gwcs


-- Varying Celestial Transform -----------------------------------

-- TODO: add time frame
data VaryingCelestialTransform = VaryingCelestialTransform
  { cdelt :: (Double, Double)
  , lonPole :: LonPole
  , -- one of each per frame
    crpixs :: [(Double, Double)]
  , crvals :: [(Double, Double)]
  , pcs :: [((Double, Double), (Double, Double))]
  }
  deriving (Generic)
instance ToAsdf VaryingCelestialTransform where
  schema _ = "asdf://dkist.nso.edu/tags/varying_celestial_transform-1.1.0"
  toValue vct =
    Object
      [ ("cdelt", toNode $ Quantity (Unit "arcsec.pixel**-1") $ toValue vct.cdelt)
      , ("crpix", toNode $ Quantity Pixel $ toValue $ toNDArray $ fmap tupleList vct.crpixs)
      , ("crval_table", toNode $ Quantity Arcseconds $ toValue $ toNDArray $ fmap tupleList vct.crvals)
      , ("pc_table", toNode $ Quantity Pixel $ toValue $ toNDArray $ pcsMatrix vct.pcs)
      , ("lon_pole", toNode vct.lonPole)
      , ("projection", toNode $ Projection Pix2Sky)
      -- , ("inputs", "[x,y,z]")
      -- , ("outputs", "[lon, lat]")
      ]
   where
    tupleList (a, b) = [a, b]

    pcList ((a, b), (c, d)) = [[a, b], [c, d]]

    pcsMatrix :: [((Double, Double), (Double, Double))] -> Array D Ix3 Double
    pcsMatrix pcs =
      M.delay
        $ M.fromLists' @M.P
          M.Seq
        $ fmap pcList pcs


data WCSFrame s = WCSFrame
  { x :: WCSAxis 'WCSMain X
  , y :: WCSAxis 'WCSMain Y
  , pcxy :: PCXY s 'WCSMain
  }


varyingCelestialTransform
  :: WCSCommon
  -> Frames (WCSFrame s)
  -> Transform (Pix X, Pix Y) (HPLon, HPLat)
varyingCelestialTransform common wcss =
  -- swap lat/lon for VaryingCelestialTransform, it expects them backwards from the FITS input
  let mid = middleFrame wcss
      frames = NE.toList wcss.frames
   in transform $
        VaryingCelestialTransform
          { cdelt = (mid.y.cdelt, mid.x.cdelt)
          , crpixs = fmap crpix frames
          , crvals = fmap crval frames
          , pcs = fmap pc frames
          , lonPole = lonPole common
          }
 where
  crpix f =
    -- subtract 1 from crpix. GWCS transforms are zero-indexed while fits is 1-indexed
    (f.y.crpix - 1, f.x.crpix - 1)
  crval f =
    (f.y.crval, f.x.crval)
  pc f =
    -- rotate 180 degrees from [[xx, xy], [yx, yy]]
    ( (f.pcxy.yy.value, f.pcxy.yx.value)
    , (f.pcxy.xy.value, f.pcxy.xx.value)
    )

{-
  - !<asdf://dkist.nso.edu/tags/varying_celestial_transform-1.1.0>
    cdelt: !unit/quantity-1.1.0
      unit: !unit/unit-1.0.0 arcsec.pixel**-1
      value: !core/ndarray-1.0.0
        source: 253
        datatype: float64
        byteorder: little
        shape: [2]
    crpix: !unit/quantity-1.1.0
      unit: !unit/unit-1.0.0 pixel
      value: !core/ndarray-1.0.0
        source: 252
        datatype: float64
        byteorder: little
        shape: [2]
    crval_table: !unit/quantity-1.1.0
      unit: !unit/unit-1.0.0 arcsec
      value: !core/ndarray-1.0.0
        source: 254
        datatype: float64
        byteorder: little
        shape: [490, 2]
    inputs: [x, y, z]
    lon_pole: 180.0
    outputs: [lon, lat]
    pc_table: !unit/quantity-1.1.0
      unit: !unit/unit-1.0.0 pixel
      value: !core/ndarray-1.0.0
        source: 255
        datatype: float64
        byteorder: little
        shape: [490, 2, 2]
    projection: !transform/gnomonic-1.2.0
      direction: pix2sky
      inputs: [x, y]
      outputs: [phi, theta]
-}

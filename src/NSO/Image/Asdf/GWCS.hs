{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Asdf.GWCS where

import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Array, D, Ix2, Ix3)
import Data.Massiv.Array qualified as M
import NSO.Image.Headers (Observation (..), Telescope (..))
import NSO.Image.Headers.Types (Degrees (..), Depth, Key (..), Meters (..), Stokes)
import NSO.Image.Headers.WCS (PC (..), PCXY (..), WCSAxisKeywords (..), WCSCommon (..), WCSHeader (..), Wav, X, Y, toWCSAxis)
import NSO.Image.Primary (PrimaryHeader (..))
import NSO.Image.Profile
import NSO.Image.Quantity
import NSO.Prelude as Prelude
import NSO.Types.Common (DateTime (..))
import Telescope.Asdf (Anchor (..), ToAsdf (..), Value (..))
import Telescope.Asdf.Core (Unit (Pixel))
import Telescope.Asdf.Core qualified as Unit
import Telescope.Asdf.GWCS as GWCS
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
  :: WCSCommon
  -> QuantityAxes 'WCSMain
  -> Transform (Pix Depth, Pix X, Pix Y) (Linear Depth, Alpha, Delta)
transformQuantity common axes =
  transformOpticalDepth (toWCSAxis axes.depth.keys) <&> transformSpatial common (toWCSAxis axes.slitX.keys) (toWCSAxis axes.dummyY.keys) pcs
 where
  pcs :: PCXY QuantityAxes 'WCSMain
  pcs = fromMaybe identityPCXY $ do
    pcx <- axes.slitX.pcs
    pcy <- axes.dummyY.pcs
    pure $ PCXY{xx = pcx.slitX, xy = pcx.dummyY, yx = pcy.slitX, yy = pcy.dummyY}


identityPCXY :: PCXY s 'WCSMain
identityPCXY =
  PCXY{xx = PC 1, xy = PC 0, yx = PC 0, yy = PC 1}


transformOpticalDepth :: WCSAxis 'WCSMain Depth -> Transform (Pix Depth) (Linear Depth)
transformOpticalDepth wcsOD =
  linear (wcsIntercept wcsOD) (Scale $ factor1digit wcsOD.cdelt)
 where
  factor1digit :: Float -> Float
  factor1digit d = fromIntegral (round @Float @Integer (d * 10)) / 10


transformSpatial
  :: WCSCommon
  -> WCSAxis 'WCSMain X
  -> WCSAxis 'WCSMain Y
  -> PCXY s 'WCSMain
  -> Transform (Pix X, Pix Y) (Alpha, Delta)
transformSpatial common wcsX wcsY pcs = linearXY |> rotate pcMatrix |> project Pix2Sky |> sky
 where
  pcMatrix :: Array D Ix2 Float
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
  sky = celestial (Lat $ arcsecondsToDegrees wcsX.crval) (Lon $ arcsecondsToDegrees wcsY.crval) lonPole

  lonPole :: LonPole
  lonPole =
    let (Key (Degrees d)) = common.lonpole
     in LonPole d

  -- we don't use wcsLinear, because it includes the crval, which is already taken into account in the sky transform
  spatialLinear :: forall a alt x. (ToAxes a) => WCSAxis alt x -> Transform (Pix a) (Linear a)
  spatialLinear wcs = linear (spatialIntercept wcs) (Scale wcs.cdelt)

  spatialIntercept :: WCSAxis alt axis -> Intercept
  spatialIntercept w =
    -- crpix is 1-indexed, need to switch to zero
    -- don't use crval, it's already incorporated into the sky transform
    Intercept $ negate (w.cdelt * (w.crpix - 1))


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


arcsecondsToDegrees :: Float -> Float
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


quantityGWCS :: PrimaryHeader -> WCSHeader QuantityAxes -> QuantityGWCS
quantityGWCS primary wcs = QuantityGWCS $ GWCS (inputStep wcs.common wcs.axes) outputStep
 where
  inputStep :: WCSCommon -> QuantityAxes 'WCSMain -> GWCSStep CoordinateFrame
  inputStep common axes = GWCSStep pixelFrame (Just (transformQuantity common axes).transformation)
   where
    pixelFrame :: CoordinateFrame
    pixelFrame =
      CoordinateFrame
        { name = "pixel"
        , axes =
            NE.fromList
              [ FrameAxis 0 "opticalDepth" (AxisType "PIXEL") Pixel
              , FrameAxis 1 "slitX" (AxisType "PIXEL") Pixel
              , FrameAxis 2 "frameY" (AxisType "PIXEL") Pixel
              ]
        }

  outputStep :: GWCSStep (CompositeFrame (CoordinateFrame, CelestialFrame HelioprojectiveFrame))
  outputStep = GWCSStep compositeFrame Nothing
   where
    compositeFrame =
      CompositeFrame (opticalDepthFrame, celestialFrame 1 (helioprojectiveFrame primary))

    opticalDepthFrame =
      CoordinateFrame
        { name = "optical_depth"
        , axes =
            NE.fromList
              [ FrameAxis 0 "optical_depth" "optical_depth" Pixel
              ]
        }


helioprojectiveFrame :: PrimaryHeader -> HelioprojectiveFrame
helioprojectiveFrame primary =
  HelioprojectiveFrame
    { coordinates = Cartesian3D (coord primary.telescope.obsgeoX) (coord primary.telescope.obsgeoY) (coord primary.telescope.obsgeoZ)
    , obstime = primary.observation.dateBeg.ktype.utc
    , rsun = Unit.Quantity Unit.Kilometers (Integer 695700)
    }
 where
  coord :: Key Meters desc -> Unit.Quantity
  coord (Key (Meters m)) = Unit.Quantity Unit.Meters $ toValue m


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

newtype QuantityGWCS
  = QuantityGWCS
      (GWCS CoordinateFrame (CompositeFrame (CoordinateFrame, CelestialFrame HelioprojectiveFrame)))
instance KnownText QuantityGWCS where
  knownText = "quantityGWCS"


instance ToAsdf QuantityGWCS where
  schema (QuantityGWCS gwcs) = schema gwcs
  anchor _ = Just $ Anchor $ knownText @QuantityGWCS
  toValue (QuantityGWCS gwcs) = toValue gwcs


profileGWCS :: PrimaryHeader -> WCSHeader ProfileAxes -> ProfileGWCS
profileGWCS primary wcs = ProfileGWCS $ GWCS (inputStep wcs.common wcs.axes) outputStep
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
              , FrameAxis 2 "slitX" (AxisType "PIXEL") Pixel
              , FrameAxis 3 "frameY" (AxisType "PIXEL") Pixel
              ]
        }

  outputStep :: GWCSStep (CompositeFrame (StokesFrame, SpectralFrame, CelestialFrame HelioprojectiveFrame))
  outputStep = GWCSStep compositeFrame Nothing
   where
    compositeFrame =
      CompositeFrame (stokesFrame, spectralFrame, celestialFrame 2 (helioprojectiveFrame primary))

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


newtype ProfileGWCS
  = ProfileGWCS
      (GWCS CoordinateFrame (CompositeFrame (StokesFrame, SpectralFrame, CelestialFrame HelioprojectiveFrame)))


instance KnownText ProfileGWCS where
  knownText = "profileGWCS"


instance ToAsdf ProfileGWCS where
  schema (ProfileGWCS gwcs) = schema gwcs
  anchor _ = Just $ Anchor $ knownText @ProfileGWCS
  toValue (ProfileGWCS gwcs) = toValue gwcs


-- Varying Celestial Transform -----------------------------------
--
-- TODO: I need a graph of L1 data that looks right, so I can see...

data VaryingCelestialTransform = VaryingCelestialTransform
  { cdelt :: (Double, Double)
  , crpix :: (Double, Double)
  , crvals :: Array D Ix2 Float
  , pcs :: Array D Ix3 Float
  , lonPole :: Degrees
  }
  deriving (Generic)

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

{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Asdf.GWCS where

import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Array, D, Ix2)
import Data.Massiv.Array qualified as M
import NSO.Image.Headers.Types (Degrees (..), Depth, Key (..), Stokes)
import NSO.Image.Headers.WCS (PC (..), PCXY (..), WCSAxisKeywords (..), WCSCommon (..), WCSHeader (..), Wav, X, Y, toWCSAxis)
import NSO.Image.Profile
import NSO.Image.Quantity
import NSO.Prelude as Prelude
import Telescope.Asdf as Asdf
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


wcsShift :: WCSAxisKeywords s alt x -> Shift
wcsShift wcs =
  Shift (realToFrac $ negate (wcs.crpix.ktype - 1))


quantityGWCS :: WCSHeader QuantityAxes -> QuantityGWCS
quantityGWCS wcs = QuantityGWCS $ GWCS (inputStep wcs.common wcs.axes) outputStep
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

  outputStep :: GWCSStep (CompositeFrame (CoordinateFrame, CelestialFrame))
  outputStep = GWCSStep compositeFrame Nothing
   where
    compositeFrame =
      CompositeFrame (opticalDepthFrame, celestialFrame 1)

    opticalDepthFrame =
      CoordinateFrame
        { name = "optical_depth"
        , axes =
            NE.fromList
              [ FrameAxis 0 "optical_depth" "optical_depth" Pixel
              ]
        }


celestialFrame :: Int -> CelestialFrame
celestialFrame n =
  CelestialFrame
    { name = "icrs"
    , referenceFrame = ICRSFrame
    , axes =
        NE.fromList
          [ FrameAxis n "lon" (AxisType "pos.eq.ra") Unit.Degrees
          , FrameAxis (n + 1) "lat" (AxisType "pos.eq.dec") Unit.Degrees
          ]
    }


newtype QuantityGWCS
  = QuantityGWCS
      (GWCS CoordinateFrame (CompositeFrame (CoordinateFrame, CelestialFrame)))
instance KnownText QuantityGWCS where
  knownText = "quantityGWCS"


instance ToAsdf QuantityGWCS where
  schema (QuantityGWCS gwcs) = schema gwcs
  anchor _ = Just $ Anchor $ knownText @QuantityGWCS
  toValue (QuantityGWCS gwcs) = toValue gwcs


profileGWCS :: WCSHeader ProfileAxes -> ProfileGWCS
profileGWCS wcs = ProfileGWCS $ GWCS (inputStep wcs.common wcs.axes) outputStep
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

  outputStep :: GWCSStep (CompositeFrame (StokesFrame, SpectralFrame, CelestialFrame))
  outputStep = GWCSStep compositeFrame Nothing
   where
    compositeFrame =
      CompositeFrame (stokesFrame, spectralFrame, celestialFrame 2)

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
      (GWCS CoordinateFrame (CompositeFrame (StokesFrame, SpectralFrame, CelestialFrame)))


instance KnownText ProfileGWCS where
  knownText = "profileGWCS"


instance ToAsdf ProfileGWCS where
  schema (ProfileGWCS gwcs) = schema gwcs
  anchor _ = Just $ Anchor $ knownText @ProfileGWCS
  toValue (ProfileGWCS gwcs) = toValue gwcs

-- test :: IO ()
-- test = do
--   out <- Asdf.encodeM $ Object [("wcs", toNode $ GWCS inputStep outputStep)]
--   BS.writeFile "/Users/seanhess/Downloads/l2.asdf" out

{-

Created in python:
 -
wcs: !<tag:stsci.edu:gwcs/wcs-1.2.0>
  name: ''
  pixel_shape: null
  steps:
  - !<tag:stsci.edu:gwcs/step-1.1.0>
    frame: !<tag:stsci.edu:gwcs/frame-1.0.0>
      axes_names: [optical_depth, spatial along slit, raster scan step number]
      axes_order: [0, 1, 2]
      axes_type: [PIXEL, PIXEL, PIXEL]
      axis_physical_types: ['custom:PIXEL', 'custom:PIXEL', 'custom:PIXEL']
      name: pixel
      naxes: 3
      unit: [!unit/unit-1.0.0 pixel, !unit/unit-1.0.0 pixel, !unit/unit-1.0.0 pixel]
    transform: !transform/concatenate-1.2.0
      forward:
      - !transform/compose-1.2.0
        forward:
        - !transform/shift-1.2.0
          inputs: [x]
          offset: -11.0
          outputs: [y]
        - !transform/scale-1.2.0
          factor: 0.1
          inputs: [x]
          outputs: [y]
        inputs: [x]
        outputs: [y]
      - !transform/compose-1.2.0
        forward:
        - !transform/compose-1.2.0
          forward:
          - !transform/compose-1.2.0
            forward:
            - !transform/compose-1.2.0
              forward:
              - !transform/concatenate-1.2.0
                forward:
                - !transform/shift-1.2.0
                  inputs: [x]
                  offset: -27.571428
                  outputs: [y]
                - !transform/shift-1.2.0
                  inputs: [x]
                  offset: -13.520178
                  outputs: [y]
                inputs: [x0, x1]
                outputs: [y0, y1]
              - !transform/concatenate-1.2.0
                forward:
                - !transform/scale-1.2.0
                  factor: 0.000415055
                  inputs: [x]
                  outputs: [y]
                - !transform/shift-1.2.0
                  inputs: [x]
                  offset: 5.929356944444445e-05
                  outputs: [y]
                inputs: [x0, x1]
                outputs: [y0, y1]
              inputs: [x0, x1]
              outputs: [y0, y1]
            - !transform/affine-1.3.0
              inputs: [x, y]
              matrix: !core/ndarray-1.0.0
                source: 0
                datatype: float64
                byteorder: little
                shape: [2, 2]
              outputs: [x, y]
              translation: !core/ndarray-1.0.0
                source: 1
                datatype: float64
                byteorder: little
                shape: [2]
            inputs: [x0, x1]
            outputs: [x, y]
          - !transform/gnomonic-1.2.0
            direction: pix2sky
            inputs: [x, y]
            outputs: [phi, theta]
          inputs: [x0, x1]
          outputs: [phi, theta]
        - !transform/rotate3d-1.3.0
          direction: native2celestial
          inputs: [phi_N, theta_N]
          outputs: [alpha_C, delta_C]
          phi: -0.1333360111111111
          psi: 180.0
          theta: -0.1130558888888889
        inputs: [x0, x1]
        outputs: [alpha_C, delta_C]
      inputs: [x, x0, x1]
      outputs: [y, alpha_C, delta_C]
  - !<tag:stsci.edu:gwcs/step-1.1.0>
    frame: !<tag:stsci.edu:gwcs/composite_frame-1.0.0>
      frames:
      - !<tag:stsci.edu:gwcs/frame-1.0.0>
        axes_names: [optical_depth]
        axes_order: [0]
        axes_type: [optical_depth]
        axis_physical_types: ['custom:optical_depth']
        name: optical_depth_out
        naxes: 1
        unit: [!unit/unit-1.0.0 pixel]
      - !<tag:stsci.edu:gwcs/celestial_frame-1.0.0>
        axes_names: [helioprojective longitude, helioprojective latitude]
        axes_order: [1, 2]
        axis_physical_types: ['custom:pos.helioprojective.lon', 'custom:pos.helioprojective.lat']
        name: helioprojective
        reference_frame: !<tag:sunpy.org:sunpy/coordinates/frames/helioprojective-1.0.0>
          frame_attributes:
            observer: !<tag:sunpy.org:sunpy/coordinates/frames/heliographic_stonyhurst-1.1.0>
              data: !<tag:astropy.org:astropy/coordinates/representation-1.1.0>
                components:
                  x: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 m,
                    value: 151718470759.01736}
                  y: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 m,
                    value: 936374.8961084613}
                  z: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 m,
                    value: -1238552794.1080718}
                type: CartesianRepresentation
              frame_attributes:
                obstime: !time/time-1.1.0 2022-06-02T21:47:26.641
                rsun: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 km,
                  value: 695700.0}
            obstime: !time/time-1.1.0 2022-06-02T21:47:26.641
            rsun: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 km,
              value: 695700.0}
        unit: [!unit/unit-1.0.0 deg, !unit/unit-1.0.0 deg]
      name: CompositeFrame
    transform: null
-}

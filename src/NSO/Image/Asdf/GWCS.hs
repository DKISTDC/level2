{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Asdf.GWCS where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Array, D, Ix2)
import Data.Massiv.Array qualified as M
import NSO.Image.Headers.Types (Key (..), WCSAlt (..))
import NSO.Image.Headers.WCS (PC (..), PCXY (..), WCSAxisKeywords (..))
import NSO.Prelude as Prelude
import Telescope.Asdf as Asdf
import Telescope.Asdf.Core (Unit (..))
import Telescope.Asdf.GWCS


-- data CoordinateFrame = CoordinateFrame
--   { name :: Text
--   , axes :: NonEmpty FrameAxis
--   }
--
--
-- data CelestialFrame = CelestialFrame
--   { name :: Text
--   , axes :: NonEmpty FrameAxis
--   -- , referenceFrame ::
--   }

data OpticalDepth deriving (Generic, ToAxes)


transformComposite
  :: WCSAxisKeywords s 'WCSMain OpticalDepth
  -> WCSAxisKeywords s 'WCSMain X
  -> WCSAxisKeywords s 'WCSMain Y
  -> PCXY s 'WCSMain
  -> Transform (Pix OpticalDepth, Pix X, Pix Y) (Linear OpticalDepth, Alpha, Delta)
transformComposite wcsOD wcsX wcsY pcs = transformOpticalDepth wcsOD <&> transformSpatial wcsX wcsY pcs


transformOpticalDepth :: WCSAxisKeywords s 'WCSMain OpticalDepth -> Transform (Pix OpticalDepth) (Linear OpticalDepth)
transformOpticalDepth wcsOD =
  linear (wcsShift wcsOD) (Scale $ factor1digit wcsOD.cdelt.ktype)
 where
  factor1digit :: Float -> Double
  factor1digit d = fromIntegral (round @Float @Integer (d * 10)) / 10


transformSpatial
  :: WCSAxisKeywords s 'WCSMain X
  -> WCSAxisKeywords s 'WCSMain Y
  -> PCXY s 'WCSMain
  -> Transform (Pix X, Pix Y) (Alpha, Delta)
transformSpatial wcsX wcsY pcs = linearXY |> rotate pcMatrix |> project Pix2Sky |> sky
 where
  pcMatrix :: Array D Ix2 Double
  pcMatrix =
    M.delay $
      M.fromLists' @M.P
        M.Seq
        [ [realToFrac pcs.xx.value, realToFrac pcs.xy.value]
        , [realToFrac pcs.yx.value, realToFrac pcs.yy.value]
        ]

  linearXY :: Transform (Pix X, Pix Y) (Linear X, Linear Y)
  linearXY = wcsLinear wcsX <&> wcsLinear wcsY

  sky :: Transform (Phi, Theta) (Alpha, Delta)
  sky = celestial (Lat $ realToFrac wcsX.crval.ktype) (Lon $ realToFrac wcsY.crval.ktype) (LonPole 180)


wcsLinear :: (ToAxes a) => WCSAxisKeywords s alt x -> Transform (Pix a) (Linear a)
wcsLinear wcs =
  linear (wcsShift wcs) (wcsScale wcs)


wcsScale :: WCSAxisKeywords s alt x -> Scale
wcsScale wcs =
  Scale (realToFrac wcs.cdelt.ktype)


wcsShift :: WCSAxisKeywords s alt x -> Shift
wcsShift wcs =
  Shift (realToFrac $ negate (wcs.crpix.ktype - 1))


inputStep :: GWCSStep CoordinateFrame
inputStep = GWCSStep pixelFrame (Just (transformComposite wcsOD wcsX wcsY pcs).transformation)
 where
  pixelFrame :: CoordinateFrame
  pixelFrame =
    CoordinateFrame
      { name = "pixel"
      , axes =
          NE.fromList
            [ FrameAxis 0 "optical_depth" (AxisType "PIXEL") Pixel
            , FrameAxis 1 "spatial along slit" (AxisType "PIXEL") Pixel
            , FrameAxis 2 "raster scan step number" (AxisType "PIXEL") Pixel
            ]
      }

  pcs :: PCXY s 'WCSMain
  pcs = PCXY{xx = PC 1, xy = PC 0, yy = PC 1, yx = PC 0}

  wcsX :: WCSAxisKeywords s 'WCSMain X
  wcsX = WCSAxisKeywords{ctype = Key "", cunit = Key "", crpix = Key 28.571428, crval = Key (-0.11305589), cdelt = Key 4.15055000e-04}

  wcsY :: WCSAxisKeywords s 'WCSMain Y
  wcsY = WCSAxisKeywords{ctype = Key "", cunit = Key "", crpix = Key 14.520178, crval = Key (-0.13333601), cdelt = Key 5.92935694e-05}

  wcsOD :: WCSAxisKeywords s 'WCSMain OpticalDepth
  wcsOD = WCSAxisKeywords{ctype = Key "", cunit = Key "", crpix = Key 12, crval = Key 0, cdelt = Key 0.1}


outputStep :: GWCSStep (CompositeFrame CoordinateFrame CelestialFrame)
outputStep = GWCSStep compositeFrame Nothing
 where
  compositeFrame =
    CompositeFrame opticalDepthFrame celestialFrame

  opticalDepthFrame =
    CoordinateFrame
      { name = "optical_depth"
      , axes =
          NE.fromList
            [ FrameAxis 0 "optical_depth" "optical_depth" (Unit "optical_depth")
            ]
      }

  celestialFrame =
    CelestialFrame
      { name = "icrs"
      , referenceFrame = ICRSFrame
      , axes =
          NE.fromList
            [ FrameAxis 1 "lon" (AxisType "pos.eq.ra") Degrees
            , FrameAxis 2 "lat" (AxisType "pos.eq.dec") Degrees
            ]
      }


data GWCS
  = GWCS
      (GWCSStep CoordinateFrame)
      (GWCSStep (CompositeFrame CoordinateFrame CelestialFrame))


instance ToAsdf GWCS where
  schema _ = "tag:stsci.edu:gwcs/wcs-1.2.0"
  toValue (GWCS inp out) =
    Object
      [ ("name", toNode $ String "")
      , ("steps", toNode $ Array [toNode inp, toNode out])
      ]


test :: IO ()
test = do
  out <- Asdf.encodeM $ Object [("wcs", toNode $ GWCS inputStep outputStep)]
  BS.writeFile "/Users/seanhess/Downloads/l2.asdf" out

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

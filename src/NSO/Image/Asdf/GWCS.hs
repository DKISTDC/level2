{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Asdf.GWCS where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Array, D, Ix2)
import Data.Massiv.Array qualified as M
import NSO.Prelude as Prelude
import Telescope.Asdf as Asdf
import Telescope.Asdf.Core (Unit (Degrees, Pixel))
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


transformComposite :: Transform (Pix OpticalDepth, Pix X, Pix Y) (Scl OpticalDepth, Alpha, Delta)
transformComposite = transformOpticalDepth <&> transformSpatial


transformOpticalDepth :: Transform (Pix OpticalDepth) (Scl OpticalDepth)
transformOpticalDepth = scale 10


transformSpatial :: Transform (Pix X, Pix Y) (Alpha, Delta)
transformSpatial = linearXY |> rotate pcMatrix |> project Pix2Sky |> celestial (Lat 1) (Lon 2) (LonPole 180)
 where
  pcMatrix :: Array D Ix2 Double
  pcMatrix = M.delay $ M.fromLists' @M.P M.Seq [[0, 1], [2, 3]]

  linearX :: Transform (Pix X) (Linear X)
  linearX = linear (Shift 10) (Scale 8)

  linearY :: Transform (Pix Y) (Linear Y)
  linearY = linear (Shift 9) (Scale 7)

  linearXY :: Transform (Pix X, Pix Y) (Linear X, Linear Y)
  linearXY = linearX <&> linearY


inputStep :: GWCSStep CoordinateFrame (Pix OpticalDepth, Pix X, Pix Y) (Scl OpticalDepth, Alpha, Delta)
inputStep = GWCSStep pixelFrame (Just transformComposite)
 where
  pixelFrame :: CoordinateFrame
  pixelFrame =
    CoordinateFrame
      { name = "pixel"
      , axes =
          NE.fromList
            [ FrameAxis "optical_depth" 0 (AxisType "PIXEL") Pixel
            , FrameAxis "spatial along slit" 1 (AxisType "PIXEL") Pixel
            , FrameAxis "raster scan step number" 2 (AxisType "PIXEL") Pixel
            ]
      }


outputStep :: GWCSStep (CompositeFrame CoordinateFrame CelestialFrame) (Scl OpticalDepth, Alpha, Delta) ()
outputStep = GWCSStep compositeFrame Nothing
 where
  compositeFrame =
    CompositeFrame opticalDepthFrame celestialFrame

  opticalDepthFrame =
    CoordinateFrame
      { name = "optical_depth"
      , axes =
          NE.fromList
            [ FrameAxis "optical_depth" 0 (AxisType "optical_depth") Pixel
            ]
      }

  celestialFrame =
    CelestialFrame
      { name = "icrs"
      , referenceFrame = ICRSFrame
      , axes =
          NE.fromList
            [ FrameAxis "lon" 1 (AxisType "pos.eq.ra") Degrees
            , FrameAxis "lat" 2 (AxisType "pos.eq.dec") Degrees
            ]
      }


data GWCS
  = GWCS
      (GWCSStep CoordinateFrame (Pix OpticalDepth, Pix X, Pix Y) (Scl OpticalDepth, Alpha, Delta))
      (GWCSStep (CompositeFrame CoordinateFrame CelestialFrame) (Scl OpticalDepth, Alpha, Delta) ())


instance ToAsdf GWCS where
  schema _ = "tag:stsci.edu:gwcs/wcs-1.2.0"
  toValue (GWCS inp out) =
    Object
      [ ("name", toNode $ String "")
      , ("steps", toNode $ Array [toNode inp, toNode out])
      ]


test :: IO ()
test = do
  out <- Asdf.encodeM $ Object $ [("transform", toNode $ GWCS inputStep outputStep)]
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

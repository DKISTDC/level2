{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}

module NSO.Image.GWCS where

import Data.List.NonEmpty qualified as NE
import GHC.Exts (IsList (toList))
import NSO.Image.Asdf.Ref (Ref (..))
import NSO.Image.Fits.Profile (ProfileAxes (..), ProfileAxis (..))
import NSO.Image.Fits.Quantity hiding (toList)
import NSO.Image.GWCS.AxisMeta (OrderedAxis (..), ToOrderedAxes (..))
import NSO.Image.GWCS.L1GWCS (HPLat, HPLon, L1GWCS (..), L1HelioFrame, Time)
import NSO.Image.GWCS.L1GWCS qualified as L1
import NSO.Image.Headers (Observation (..))
import NSO.Image.Headers.Types (Degrees (..), Key (..), PixelsPerBin (..))
import NSO.Image.Headers.WCS (PC (..), PCXY (..), WCSAxisKeywords (..), WCSCommon (..), WCSHeader (..), Wav, X, Y, toWCSAxis)
import NSO.Image.Primary (PrimaryHeader (..))
import NSO.Image.Types.Frame (Depth, Frames (..), Stokes, middleFrame)
import NSO.Image.Types.Quantity (OpticalDepth)
import NSO.Prelude as Prelude hiding (identity)
import Numeric (showFFloat)
import Telescope.Asdf (Anchor (..), ToAsdf (..), Value (..))
import Telescope.Asdf.Core (Quantity (..), Unit (Nanometers, Pixel, Unit))
import Telescope.Asdf.Core qualified as Unit
import Telescope.Asdf.GWCS as GWCS hiding (celestial)
import Telescope.Data.KnownText
import Telescope.Data.WCS (WCSAlt (..), WCSAxis (..))


transformProfile
  :: PixelsPerBin
  -> ProfileAxes 'WCSMain
  -> Transform (Pix Stokes, Pix Wav, Pix X, Pix Y) (Stokes, Linear Wav, HPLon, HPLat, Time)
transformProfile bin axes =
  stokes <&> linearSpectral (toWCSAxis axes.wavelength.keys) <&> (scaleAxes |> L1.varyingTransformRef)
 where
  scaleAxes :: Transform (Pix X, Pix Y) (Scale X, Pix Y)
  scaleAxes = scaleX bin <&> GWCS.identity @(Pix Y)

  stokes :: Transform (Pix Stokes) Stokes
  stokes = transform Identity


data LinearSpectral = LinearSpectral {intercept :: Quantity, slope :: Quantity}
  deriving (Generic)
instance ToAsdf LinearSpectral where
  schema _ = "!transform/linear1d-1.0.0"


linearSpectral :: WCSAxis 'WCSMain Wav -> Transform (Pix Wav) (Linear Wav)
linearSpectral wcs =
  let Intercept i = wcsIntercept wcs
      s = wcs.cdelt
   in transform $ LinearSpectral (Quantity Nanometers (nanosValue i)) (Quantity (Unit "nm.pixel**-1") (nanosValue s))
 where
  nanosValue :: Double -> Value
  nanosValue d = String $ cs $ showFFloat (Just 6) d ""


-- Quantity ---------------------------------------------------

transformQuantity
  :: PixelsPerBin
  -> Frames (QuantityAxes 'WCSMain)
  -> Transform (Pix Depth, Pix X, Pix Y) (Linear Depth, HPLon, HPLat, Time)
transformQuantity bin axes =
  let mid = middleFrame axes
   in transformOpticalDepth (toWCSAxis mid.depth.keys) <&> spaceTimeTransform
 where
  spaceTimeTransform :: Transform (Pix X, Pix Y) (HPLon, HPLat, Time)
  spaceTimeTransform = duplicateInputs |> scaleZeroAxes |> L1.varyingTransformRef

  -- TODO: do we need to duplicate time? Can this be removed?
  duplicateInputs :: Transform (Pix X, Pix Y) (Pix X, Pix Y)
  -- duplicateInputs = transform $ Mapping [0, 1, 0]
  duplicateInputs = transform $ Mapping [0, 1]

  -- where
  --  zero :: (ToAxes a) => Transform (Pix a) (Zero a)
  --  zero = transform $ GWCS.Const1D $ Quantity Pixel (Integer 0)

  scaleZeroAxes :: Transform (Pix X, Pix Y) (Scale X, Pix Y)
  -- scaleZeroAxes = scaleX bin <&> GWCS.identity @(Pix Y) <&> zero
  scaleZeroAxes = scaleX bin <&> GWCS.identity @(Pix Y)


scaleX :: PixelsPerBin -> Transform (Pix X) (Scale X)
scaleX p = scale (fromIntegral p)


identityPCXY :: PCXY s 'WCSMain
identityPCXY =
  PCXY{xx = PC 1, xy = PC 0, yx = PC 0, yy = PC 1}


data LinearOpticalDepth = LinearOpticalDepth {intercept :: Quantity, slope :: Quantity}
  deriving (Generic)
instance ToAsdf LinearOpticalDepth where
  schema _ = "!transform/linear1d-1.0.0"


transformOpticalDepth :: WCSAxis 'WCSMain Depth -> Transform (Pix Depth) (Linear Depth)
transformOpticalDepth wcsOD =
  -- intercept: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 pix, value: 853.7012736084624}
  -- slope: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 pix.pixel**-1, value: 9.99852488051306e-4}
  -- linear (wcsIntercept wcsOD) (Scale $ factor1digit wcsOD.cdelt)
  let Intercept i = wcsIntercept wcsOD
      s = wcsOD.cdelt
   in transform $ LinearOpticalDepth (Quantity Pixel (factor1digit i)) (Quantity (Unit "pix.pixel**-1") (factor1digit s))


factor1digit :: Double -> Value
factor1digit d = String $ cs $ showFFloat (Just 1) d "" -- fromIntegral (round @Double @Integer (d * 10)) / 10


lonPole :: WCSCommon -> LonPole
lonPole common =
  let (Key (Degrees d)) = common.lonpole
   in LonPole d


wcsShift :: WCSAxisKeywords s alt a -> Shift a
wcsShift wcs =
  Shift (realToFrac $ negate (wcs.crpix.ktype - 1))


-- | Force input and output to match transform
quantityGWCS
  :: forall input output
   . (input ~ (Pix Depth, Pix X, Pix Y), output ~ (Linear Depth, HPLon, HPLat, Time))
  => PixelsPerBin
  -> L1GWCS
  -> Frames PrimaryHeader
  -> Frames (QuantityHeader OpticalDepth)
  -> QuantityGWCS
quantityGWCS bin l1gwcs primaries quants =
  let firstPrim = head primaries.frames
   in QuantityGWCS $ GWCS mempty inputStep (outputStep firstPrim)
 where
  inputStep :: GWCSStep CoordinateFrame
  inputStep = GWCSStep pixelFrame (Just (transformQuantity bin (fmap axis quants)).transformation)
   where
    axis :: QuantityHeader x -> QuantityAxes 'WCSMain
    axis q = q.wcs.axes

    pixelFrame :: CoordinateFrame
    pixelFrame =
      let axes = orderedAxes @input
       in CoordinateFrame
            { name = "pixel"
            , axes = fmap (\ax -> FrameAxis ax.axisOrder ax.axisName (AxisType "PIXEL") Pixel) (NE.fromList $ toList axes)
            }

  outputStep :: PrimaryHeader -> GWCSStep (CompositeFrame (CoordinateFrame, CelestialFrame (Ref L1HelioFrame), TemporalFrame))
  outputStep h0 = GWCSStep compositeFrame Nothing
   where
    compositeFrame =
      let (depth, lon, lat, time) = orderedAxes @output
       in CompositeFrame (opticalDepth depth, celestial lon lat, temporal time)

    opticalDepth :: OrderedAxis (Linear Depth) -> CoordinateFrame
    opticalDepth depth =
      CoordinateFrame
        { name = depth.axisName
        , axes =
            NE.fromList
              [ FrameAxis depth.axisOrder depth.axisName "phys.absorption.opticalDepth" Pixel
              ]
        }

    celestial :: OrderedAxis HPLon -> OrderedAxis HPLat -> CelestialFrame (Ref L1HelioFrame)
    celestial lon _ =
      celestialFrame lon.axisOrder l1gwcs.helioFrame.frame

    -- TODO: this isn't correct. I'm using a tabular temporal frame? Or does the frame want the first value?
    temporal :: OrderedAxis Time -> TemporalFrame
    temporal axis =
      TemporalFrame
        { name = axis.axisName
        , axisOrder = axis.axisOrder
        , time = h0.observation.dateAvg.ktype
        }


type OpticalDepthFrame = CoordinateFrame


newtype QuantityGWCS
  = QuantityGWCS
      (GWCS CoordinateFrame (CompositeFrame (OpticalDepthFrame, CelestialFrame (Ref L1HelioFrame), TemporalFrame)))
instance KnownText QuantityGWCS where
  knownText = "quantityGWCS"


instance ToAsdf QuantityGWCS where
  schema (QuantityGWCS gwcs) = schema gwcs
  anchor _ = Just $ Anchor $ knownText @QuantityGWCS
  toValue (QuantityGWCS gwcs) = toValue gwcs


celestialFrame :: Int -> HelioprojectiveFrame -> CelestialFrame (Ref L1HelioFrame)
celestialFrame n _helioFrame =
  CelestialFrame
    { name = "helioprojective"
    , referenceFrame = Ref -- helioFrame
    , axes =
        NE.fromList
          [ FrameAxis n "pos.helioprojective.lon" (AxisType "custom:pos.helioprojective.lon") Unit.Arcseconds
          , FrameAxis (n + 1) "pos.helioprojective.lat" (AxisType "custom:pos.helioprojective.lat") Unit.Arcseconds
          ]
    }


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

profileGWCS
  :: forall input output
   . (input ~ (Pix Stokes, Pix Wav, Pix X, Pix Y), output ~ (Stokes, Linear Wav, HPLon, HPLat, Time))
  => PixelsPerBin
  -> L1GWCS
  -> PrimaryHeader
  -> WCSHeader ProfileAxes
  -> ProfileGWCS
profileGWCS bin l1gwcs primary wcs =
  ProfileGWCS $ GWCS{name = mempty, input = inputStep, output = outputStep}
 where
  inputStep :: GWCSStep CoordinateFrame
  inputStep =
    let trans :: Transform input output = transformProfile bin wcs.axes
     in GWCSStep pixelFrame (Just trans.transformation)
   where
    pixelFrame :: CoordinateFrame
    pixelFrame =
      let axes = orderedAxes @input
       in CoordinateFrame
            { name = "pixel"
            , axes = fmap (\ax -> FrameAxis ax.axisOrder ax.axisName (AxisType "PIXEL") Pixel) (NE.fromList $ toList axes)
            }

  outputStep :: GWCSStep (CompositeFrame (StokesFrame, SpectralFrame, CelestialFrame (Ref L1HelioFrame), TemporalFrame))
  outputStep = GWCSStep compositeFrame Nothing
   where
    compositeFrame =
      let (stk, wav, lon, lat, time) = orderedAxes @output
       in CompositeFrame (stokes stk, spectral wav, celestial lon lat, temporal time)

    spectral :: OrderedAxis (Linear Wav) -> SpectralFrame
    spectral OrderedAxis{axisName, axisOrder} =
      SpectralFrame
        { name = axisName
        , axisOrder
        }

    celestial :: OrderedAxis HPLon -> OrderedAxis HPLat -> CelestialFrame (Ref L1HelioFrame)
    celestial lon _ =
      celestialFrame lon.axisOrder l1gwcs.helioFrame.frame

    temporal :: OrderedAxis Time -> TemporalFrame
    temporal OrderedAxis{axisName, axisOrder} =
      TemporalFrame
        { name = axisName
        , axisOrder
        , time = primary.observation.dateAvg.ktype
        }

    stokes :: OrderedAxis Stokes -> StokesFrame
    stokes OrderedAxis{axisName, axisOrder} =
      StokesFrame
        { name = axisName
        , axisOrder
        }


newtype ProfileGWCS
  = ProfileGWCS
      (GWCS CoordinateFrame (CompositeFrame (StokesFrame, SpectralFrame, CelestialFrame (Ref L1HelioFrame), TemporalFrame)))


data WCSFrame s = WCSFrame
  { x :: WCSAxis 'WCSMain X
  , y :: WCSAxis 'WCSMain Y
  , pcxy :: PCXY s 'WCSMain
  }

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Image.Asdf.GWCS where

import Control.Arrow

-- import Control.Category (Category)
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import GHC.TypeLits
import NSO.Image.Headers.Keywords (KnownText (..))
import NSO.Prelude as Prelude
import Telescope.Asdf
import Telescope.Asdf.Core
import Telescope.Asdf.NDArray


newtype AxisName = AxisName Text
  deriving newtype (IsString, ToAsdf)


data AxisType = AxisPixel


data CoordinateFrame = CoordinateFrame
  { name :: Text
  , axes :: NonEmpty FrameAxis
  }


data CelestialFrame = CelestialFrame
  { name :: Text
  , axes :: NonEmpty FrameAxis
  -- , referenceFrame ::
  }


-- TODO: axis order when serialized
data FrameAxis = FrameAxis
  { axisName :: AxisName
  , axisOrder :: Int
  , axisType :: AxisType
  , unit :: Unit
  }


-- I'm probably thinking too low level
-- there is a better way
-- we have a number of arguments
-- some sort of monad
-- compose = bind
-- concat = ??? multiple arguments to a function?
-- should make sure all of them are handled
-- like we have inputs A,B,C coming through
-- we need to handle all of them. Can they be combined into one?
-- ASSUME: that the number of inputs = outputs
-- can they be entangled? Yes
-- they aren't independent of each other, but the inputs are the same

-- but sometimes they are independent. Like optical depth has nothing to do with the other ones
-- soo.....
-- well obviously you can concatenate them

-- that affine transform isn't a concatenation
-- it takes two inputs, does some stuff, produces two outputs
-- concat evaluates them separately
--
--
-- so a step has to produce an output of exactly the same shape as the input
-- but not independently
-- not sure how to enforce that in haskell... fun.

-- yeah, they are probably arrows. They have first and second...

-- you don't name the output, you connect them
-- class TStep a where
--   inputName :: a -> AxisName
--
--
-- data t |> ts
-- data t <> ts
--
--
--
-- data

-- class SimpleTransformation t where
--   inputs :: NonEmpty AxisName

-- data Transform = Transform
--   -- these should be the same size? No... not necessarily?
--   { inputs :: NonEmpty AxisName
--   , outputs :: NonEmpty AxisName
--   , transformation :: Transformation
--   }

-- instance Semigroup Transform where
--   t1 <> t2 =
--     Transform
--       { inputs = t1.inputs <> t2.inputs
--       , outputs = t1.outputs <> t2.outputs
--       }
--
--
class KnownAxis a


-- ok, this is interesting, but I don't actually do any computation
-- I'm just serializing it
--
-- maybe we can figure it out from the types
-- adds types to Transformation

-- what about a type-level implementation?
-- and some classes / type functions that can handle it?
--

-- data Transformation b c = Transformation
--   { inputs :: NonEmpty AxisName
--   , outputs :: NonEmpty AxisName
--   , -- the serialized information
--     info :: CompoundTransformation
--   }
--
--
-- it's really a nonempty list... no?
--
--

-- pipeline :: Transform ["optical depth", "x", "y"] ["optical depth", "alpha", "delta"]
-- pipeline = keepOpticalDepth <&> celestial
--  where
--   keepOpticalDepth :: Transform "optical depth" "optical depth"
--   keepOpticalDepth = Transform $ Simple Identity
--
--
-- celestial :: Transform ["x", "y"] ["alpha", "delta"]
-- celestial = _
--

data X
data Y


data Pix a -- Pixel
data Scl a -- Scaled
data Dlt a -- Delta (Shifted)
data Rot a -- Rotated


instance (ToAxes '[a]) => ToAxes '[Pix a] where
  toAxes = toAxes @'[a]
instance (ToAxes '[a]) => ToAxes '[Dlt a] where
  toAxes = toAxes @'[a]
instance (ToAxes '[a]) => ToAxes '[Scl a] where
  toAxes = toAxes @'[a]
instance (ToAxes '[a]) => ToAxes '[Rot a] where
  toAxes = toAxes @'[a]
instance ToAxes '[X] where
  toAxes = ["x"]
instance ToAxes '[Y] where
  toAxes = ["y"]


data Phi
data Theta
data Alpha
data Delta


instance ToAxes '[Phi] where
  toAxes = ["phi"]
instance ToAxes '[Theta] where
  toAxes = ["theta"]
instance ToAxes '[Alpha] where
  toAxes = ["alpha"]
instance ToAxes '[Delta] where
  toAxes = ["delta"]


spatial :: Transform [Pix X, Pix Y] [Alpha, Delta]
spatial = shiftXY |> scaleXY |> rotate |> projection |> celestial


rotate :: Transform [Scl X, Scl Y] [Rot X, Rot Y]
rotate = undefined -- _ -- affine


-- must be rotated first
projection :: Transform [Rot X, Rot Y] [Phi, Theta]
projection = undefined -- _ -- pix2sky


celestial :: Transform [Phi, Theta] [Alpha, Delta]
celestial = undefined -- _ -- rotatenative2celestial


shiftXY :: Transform [Pix X, Pix Y] [Dlt X, Dlt Y]
shiftXY = shift 6 <:> shift 8 <:> empty


scaleXY :: Transform [Dlt X, Dlt Y] [Scl X, Scl Y]
scaleXY = scale 7 <:> scale 8 <:> empty


empty :: Transform '[] '[]
empty = Transform $ Transformation [] [] (Direct mempty mempty)


-- you can't shift anything. It has to NOT be a
shift :: (ToAxes '[f a], ToAxes '[Dlt a]) => Double -> Transform (f a) (Dlt a)
shift d = toTransform $ Shift d


scale :: (ToAxes '[f a], ToAxes '[Scl a]) => Double -> Transform (f a) (Scl a)
scale d = toTransform $ Scale d


data Shifted a
data Scaled a


data Transformation = Transformation
  { inputs :: [AxisName]
  , outputs :: [AxisName]
  , forward :: Forward
  }


data Forward
  = Compose (NonEmpty Transformation)
  | Concat (NonEmpty Transformation)
  | Direct {schemaTag :: SchemaTag, fields :: Value}


data Transform b c = Transform
  { transformation :: Transformation
  }


toTransform :: forall a b c. (ToAsdf a, ToAxes '[b], ToAxes '[c]) => a -> Transform b c
toTransform a =
  Transform
    $ Transformation
      (toAxes @'[b])
      (toAxes @'[c])
    $ Direct (schema a) (toValue a)


instance (ToAxes b, ToAxes c) => ToAsdf (Transform b c) where
  schema (Transform t) = schema t
  toValue (Transform t) = toValue t


instance ToAsdf Transformation where
  schema t =
    case t.forward of
      Compose _ -> "transform/compose-1.2.0"
      Concat _ -> "transform/concatenate-1.2.0"
      Direct{schemaTag} -> schemaTag


  toValue t =
    case t.forward of
      Compose ts -> toValue ts
      Concat ts -> toValue ts
      Direct{fields} -> inputFields <> fields
   where
    inputFields =
      Object
        [ ("inputs", toNode t.inputs)
        , ("outputs", toNode t.outputs)
        ]


(|>) :: forall b c d. (ToAxes b, ToAxes d) => Transform b c -> Transform c d -> Transform b d
(Transform s) |> (Transform t) = Transform
  $ Transformation
    (toAxes @b)
    (toAxes @d)
  $ case t.forward of
    Compose ts -> Compose $ s :| NE.toList ts
    _ -> Compose $ s :| [t]


-- type family TConcat a (b :: [Type]) where
--   -- TConcat a (b, c, d) = (a, b, c, d)
--   -- TConcat (a, b) (c, d) = (a, b, c, d)
--   -- TConcat (a, b, c) d = (a, b, c, d)
--   -- TConcat a (b, c) = (a, b, c)
--   -- TConcat () b = b
--   -- TConcat a () = a
--   -- TConcat a b = (a, b)
--
--   TConcat a [b, c, d] = [a, b, c, d]
--   TConcat a [b, c] = [a, b, c]
--   TConcat a '[b] = [a, b]
--   TConcat a '[] = '[a]

-- we need to preprend an input...
-- (<&>) :: Transform a b -> Transform c d -> Transform (a : cs) (b : ds)

(<:>)
  :: forall (a :: Type) (b :: Type) (cs :: [Type]) (ds :: [Type])
   . (ToAxes (a : cs), ToAxes (b : ds))
  => Transform a b
  -> Transform cs ds
  -> Transform (a : cs) (b : ds)
Transform s <:> Transform t =
  Transform
    $ Transformation
      (toAxes @(a : cs))
      (toAxes @(b : ds))
    $ concatTransform t.inputs t.forward
 where
  concatTransform [] _ = Concat $ NE.singleton s
  concatTransform _ (Concat ts) = Concat $ s :| NE.toList ts
  concatTransform _ _ = Concat $ s :| [t]
infixr 8 <:>


-- transList :: Transformation -> NonEmpty Transformation
-- transList = \case
--   Compose ts -> ts
--   Concat ts -> ts
--   Simple t -> NE.singleton (Simple t)

-- data SimpleTransformation
--   = Scale {factor :: Double}
--   | Shift {offset :: Double}
--   | Identity
--   | Affine {matrix :: NDArrayData, translation :: NDArrayData}
--   | Gnomonic {direction :: Direction}
--   | Rotate3d {direction :: Direction, phi :: Double, psi :: Double, theta :: Double}

data Direction
  = Pix2Sky
  | Native2Celestial


data Shift = Shift Double
data Scale = Scale Double
data Affine = Affine {matrix :: NDArrayData, translation :: NDArrayData}


instance ToAsdf Shift where
  schema _ = "transform/shift-1.2.0"
  toValue (Shift d) =
    Object [("shift", toNode d)]


instance ToAsdf Scale where
  schema _ = "transform/scale-1.2.0"
  toValue (Scale d) =
    Object [("scale", toNode d)]


-- the names of the inputs and outputs are completely arbitrary

-- could we use the actual function composition operator?
-- (.|) :: Transform -> Transform -> Transform
-- t1 .| t2 =
--   Transform
--     { inputs = t1.inputs
--     , outputs = t2.outputs
--     , forward = Compose t1 t2
--     }

-- could we use the actual & operator?
-- (.&) :: Transform -> NonEmpty Transform -> Transform
-- t1 .& t2 =
--   Transform
--     { inputs = t1.inputs <> t2.inputs
--     , outputs = t1.inputs <> t2.outputs
--     , forward = Concat t1 t2
--     }

newtype CompositeFrame = CompositeFrame (NonEmpty CoordinateFrame)


-- data ReferenceFrame = ReferenceFrame
--   { observer :: _
--   }

class ToAxes (as :: [Type]) where
  toAxes :: [AxisName]


-- instance ToAxes '[] where
--   toAxes = []
instance (ToAxes '[a], ToAxes '[b]) => ToAxes [a, b] where
  toAxes = toAxes @'[a] <> toAxes @'[b]

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

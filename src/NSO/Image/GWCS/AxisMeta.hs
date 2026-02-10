{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}

module NSO.Image.GWCS.AxisMeta where

import Data.List qualified as L
import GHC.Exts (IsList (..))
import NSO.Image.GWCS.L1GWCS (HPLat, HPLon, Time)
import NSO.Image.Headers.WCS (Wav, X, Y)
import NSO.Image.Types.Frame (Depth, Stokes)
import NSO.Prelude as Prelude
import Telescope.Asdf (ToAsdf (..))
import Telescope.Asdf.GWCS as GWCS


-- BUG: ordered axes still wrong. It's reversing the list, not reversing the numbers... sigh...

data AxisMeta = AxisMeta
  { axisName :: AxisName
  , aligned :: Bool
  }


-- | type indexed axes labels, alignment, index
class ToAxisMeta a where
  axisMeta :: AxisMeta


instance (ToAxisMeta a) => ToAxisMeta (Pix a) where
  axisMeta = axisMeta @a


instance (ToAxisMeta a) => ToAxisMeta (Linear a) where
  axisMeta = axisMeta @a


instance ToAxisMeta Wav where
  axisMeta = AxisMeta "wavelength" False


instance ToAxisMeta X where
  axisMeta = AxisMeta "slit_x" True


instance ToAxisMeta Y where
  axisMeta = AxisMeta "slit_y" True


instance ToAxisMeta Time where
  axisMeta = AxisMeta "temporal" False


instance ToAxisMeta HPLon where
  axisMeta = AxisMeta "pos.helioprojective.lon" True


instance ToAxisMeta HPLat where
  axisMeta = AxisMeta "pos.helioprojective.lat" True


instance ToAxisMeta Depth where
  axisMeta = AxisMeta "optical_depth" True


instance ToAxisMeta Stokes where
  axisMeta = AxisMeta "stokes" True


class ToOrderedAxes a where
  type AxisO a :: Type
  orderedAxes :: AxisO a


instance (ToAxisMeta a, ToAxisMeta b) => ToOrderedAxes (a, b) where
  type AxisO (a, b) = (OrderedAxis a, OrderedAxis b)
  orderedAxes = (orderedAxis @a 0, orderedAxis @b 1)


instance (ToAxisMeta a, ToAxisMeta b, ToAxisMeta c) => ToOrderedAxes (a, b, c) where
  type AxisO (a, b, c) = (OrderedAxis a, OrderedAxis b, OrderedAxis c)
  orderedAxes = (orderedAxis @a 0, orderedAxis @b 1, orderedAxis @c 2)


instance (ToAxisMeta a, ToAxisMeta b, ToAxisMeta c, ToAxisMeta d) => ToOrderedAxes (a, b, c, d) where
  type AxisO (a, b, c, d) = (OrderedAxis a, OrderedAxis b, OrderedAxis c, OrderedAxis d)
  orderedAxes = (orderedAxis @a 0, orderedAxis @b 1, orderedAxis @c 2, orderedAxis @d 3)


instance (ToAxisMeta a, ToAxisMeta b, ToAxisMeta c, ToAxisMeta d, ToAxisMeta e) => ToOrderedAxes (a, b, c, d, e) where
  type AxisO (a, b, c, d, e) = (OrderedAxis a, OrderedAxis b, OrderedAxis c, OrderedAxis d, OrderedAxis e)
  orderedAxes = (orderedAxis @a 0, orderedAxis @b 1, orderedAxis @c 2, orderedAxis @d 3, orderedAxis @e 4)


instance IsList (OrderedAxis a, OrderedAxis b) where
  type Item (OrderedAxis a, OrderedAxis b) = OrderedAxis ()
  toList (a, b) = [convert a, convert b]
  fromList [a, b] = (convert a, convert b)
  fromList _ = error "Expected 2 element list"


instance IsList (OrderedAxis a, OrderedAxis b, OrderedAxis c) where
  type Item (OrderedAxis a, OrderedAxis b, OrderedAxis c) = OrderedAxis ()
  toList (a, b, c) = [convert a, convert b, convert c]
  fromList [a, b, c] = (convert a, convert b, convert c)
  fromList _ = error "Expected 3 element list"


instance IsList (OrderedAxis a, OrderedAxis b, OrderedAxis c, OrderedAxis d) where
  type Item (OrderedAxis a, OrderedAxis b, OrderedAxis c, OrderedAxis d) = OrderedAxis ()
  toList (a, b, c, d) = [convert a, convert b, convert c, convert d]
  fromList [a, b, c, d] = (convert a, convert b, convert c, convert d)
  fromList _ = error "Expected 4 element list"


instance IsList (OrderedAxis a, OrderedAxis b, OrderedAxis c, OrderedAxis d, OrderedAxis e) where
  type Item (OrderedAxis a, OrderedAxis b, OrderedAxis c, OrderedAxis d, OrderedAxis e) = OrderedAxis ()
  toList (a, b, c, d, e) = [convert a, convert b, convert c, convert d, convert e]
  fromList [a, b, c, d, e] = (convert a, convert b, convert c, convert d, convert e)
  fromList _ = error "Expected 5 element list"


-- WARNING: Axes are in GWCS order, not fits!
alignedAxes :: (IsList a, Item a ~ OrderedAxis ()) => FitsOrderAxes a -> AlignedAxes
alignedAxes (FitsOrderAxes axes) =
  let axesL = toList axes
      maxAxisN = length axesL - 1
      aligned = L.reverse $ filter (.aligned) axesL
      axns :: [Int] = fmap (\a -> maxAxisN - a.axisOrder) aligned :: [Int]
   in AlignedAxes axns


newtype AlignedAxes = AlignedAxes {axes :: [Int]}
  deriving newtype (ToAsdf)


newtype AlignedAxesF f = AlignedAxesF {axes :: [Int]}
  deriving newtype (ToAsdf)


orderedAxis :: forall a. (ToAxisMeta a) => Int -> OrderedAxis a
orderedAxis n = orderedAxis' n (axisMeta @a)


orderedAxis' :: Int -> AxisMeta -> OrderedAxis a
orderedAxis' n meta =
  OrderedAxis
    { axisOrder = n
    , axisName = meta.axisName
    , aligned = meta.aligned
    }


convert :: OrderedAxis a -> OrderedAxis b
convert (OrderedAxis o n a) = OrderedAxis o n a


data OrderedAxis a = OrderedAxis
  { axisOrder :: Int
  , axisName :: AxisName
  , aligned :: Bool
  }


-- orderAxes :: forall a. (ToAxesMeta a) => NonEmpty OrderedAxis
-- orderAxes = NE.zipWith (\n m -> OrderedAxis n m.axisName m.aligned) [0 ..] (axesMeta @a)

-- | Assumed to be in Fits Order, not GWCS
newtype FitsOrderAxes a = FitsOrderAxes a


instance (IsList a, Item a ~ OrderedAxis ()) => ToAsdf (FitsOrderAxes a) where
  toValue (FitsOrderAxes axes) =
    toValue $ fmap (.axisName) $ L.reverse $ toList axes

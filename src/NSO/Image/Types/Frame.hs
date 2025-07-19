{-# LANGUAGE UndecidableInstances #-}

module NSO.Image.Types.Frame
  ( Depth
  , SlitX
  , FrameY
  , Stokes
  , Frames (..)
  , Arms (..)
  , Arm (..)
  , armsFrames
  , framesArms
  , middleFrame
  )
where

import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import NSO.Prelude
import NSO.Types.Common (Stokes)
import NSO.Types.Wavelength (SpectralLine)
import Telescope.Asdf (ToAsdf (..))
import Telescope.Asdf.GWCS (ToAxes (..))
import Telescope.Data.KnownText


data Depth deriving (Generic, ToAxes)
data SlitX
data FrameY
instance KnownText Depth where
  knownText = "OpticalDepth"
instance KnownText SlitX where
  knownText = "SlitX"
instance KnownText FrameY where
  knownText = "FrameY"


newtype Frames a = Frames {frames :: NonEmpty a}
  deriving newtype (Foldable, Functor)
  deriving (Show, Traversable)


middleFrame :: Frames a -> a
middleFrame (Frames as) =
  case take 1 $ NE.drop (length as `div` 2) as of
    [] -> head as
    (a : _) -> a


-- | One entry per ViSP arm
newtype Arms a = Arms {arms :: NonEmpty (Arm a)}
  deriving newtype (Eq)


instance Functor Arms where
  fmap :: (a -> b) -> Arms a -> Arms b
  fmap f (Arms as) = Arms $ fmap (\(Arm l a) -> Arm l (f a)) as


instance (ToAsdf (Arm a)) => ToAsdf (Arms a) where
  -- don't turn it into an YAML list, allow each item to serialize to an
  --  Object, then concatenate them all
  toValue (Arms arms) =
    mconcat $ NE.toList $ fmap toValue arms


instance {-# OVERLAPS #-} Show (Arms [a]) where
  show (Arms as) = "Arms [" <> L.intercalate "," (NE.toList $ fmap (\bs -> show (length bs.value) <> " items") as) <> "]"
instance {-# OVERLAPS #-} Show (Arms (NonEmpty a)) where
  show (Arms as) = "Arms [" <> L.intercalate "," (NE.toList $ fmap (\bs -> show (length bs.value) <> " items") as) <> "]"
instance (Show a) => Show (Arms a) where
  show (Arms as) = "Arms " <> show as


data Arm a = Arm
  { line :: SpectralLine
  , value :: a
  }
  deriving (Eq, Show)


-- Given a list of frames, subdivided by arm, create an Arms (list of arms), split by frames
armsFrames :: forall a. Frames (Arms a) -> Arms (Frames a)
armsFrames (Frames frames) =
  let slines :: NonEmpty SpectralLine = fmap (.line) (head frames).arms
      framesXArms :: NonEmpty (NonEmpty a) = fmap (\farms -> fmap (.value) farms.arms) frames
   in Arms $ NE.zipWith arm slines $ NE.transpose framesXArms
 where
  arm :: SpectralLine -> NonEmpty a -> Arm (Frames a)
  arm l vals = Arm l (Frames vals)


framesArms :: forall a. Arms (Frames a) -> Frames (Arms a)
framesArms (Arms arms) =
  let slines :: NonEmpty SpectralLine = fmap (.line) arms
      values :: NonEmpty (NonEmpty a) = fmap (\fs -> fs.value.frames) arms
   in Frames $ fmap (Arms . NE.zipWith Arm slines) $ NE.transpose values

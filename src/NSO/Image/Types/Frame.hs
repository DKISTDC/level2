module NSO.Image.Types.Frame
  ( Depth
  , SlitX
  , FrameY
  , Stokes
  , Frames (..)
  , Arms (..)
  , armsFrames
  , framesArms
  )
where

import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import NSO.Prelude
import NSO.Types.Common (Stokes)
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


-- | One entry per ViSP arm
newtype Arms a = Arms {arms :: NonEmpty a}
  deriving newtype (Eq, Functor)


instance (ToAsdf a) => ToAsdf (Arms a) where
  -- don't turn it into an YAML list, allow each item to serialize to an
  --  Object, then concatenate them all
  toValue (Arms arms) =
    mconcat $ NE.toList $ fmap toValue arms


instance {-# OVERLAPS #-} Show (Arms [a]) where
  show (Arms as) = "Arms [" <> L.intercalate "," (NE.toList $ fmap (\bs -> show (length bs) <> " items") as) <> "]"
instance {-# OVERLAPS #-} Show (Arms (NonEmpty a)) where
  show (Arms as) = "Arms [" <> L.intercalate "," (NE.toList $ fmap (\bs -> show (length bs) <> " items") as) <> "]"
instance (Show a) => Show (Arms a) where
  show (Arms as) = "Arms " <> show as


-- Given a list of frames, subdivided by arm, create an Arms (list of arms), split by frames
armsFrames :: Frames (Arms a) -> Arms (Frames a)
armsFrames (Frames frames) =
  Arms $ fmap Frames $ NE.transpose $ fmap (.arms) frames


framesArms :: Arms (Frames a) -> Frames (Arms a)
framesArms (Arms arms) =
  Frames $ fmap Arms $ NE.transpose $ fmap (.frames) arms

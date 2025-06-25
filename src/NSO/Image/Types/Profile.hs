module NSO.Image.Types.Profile where

import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import NSO.Image.Types.Axes (Depth, FrameY, SlitX, Stokes)
import NSO.Prelude
import NSO.Types.Wavelength
import Telescope.Asdf (ToAsdf (..))
import Telescope.Data.DataCube
import Telescope.Data.KnownText


data ProfileType
  = Original
  | Fit
  deriving (Show)


instance KnownText Original where
  knownText = "Original"
instance KnownText Fit where
  knownText = "Fit"


-- | A list with one entry per VISP arm
newtype Arms a = Arms {arms :: NonEmpty a}
  deriving newtype (Eq)


instance {-# OVERLAPS #-} Show (Arms [a]) where
  show (Arms as) = "Arms [" <> L.intercalate "," (NE.toList $ fmap (\bs -> show (length bs) <> " items") as) <> "]"
instance {-# OVERLAPS #-} Show (Arms (NonEmpty a)) where
  show (Arms as) = "Arms [" <> L.intercalate "," (NE.toList $ fmap (\bs -> show (length bs) <> " items") as) <> "]"
instance (Show a) => Show (Arms a) where
  show (Arms as) = "Arms " <> show as


instance (ToAsdf a) => ToAsdf (Arms a) where
  toValue (Arms as) = toValue as


newtype ProfileImage fit = ProfileImage
  { data_ :: DataCube [SlitX, Wavelength Nm, Stokes] Float
  }


instance Show (ProfileImage fit) where
  show p = "ProfileImage " <> show (dataCubeAxes p.data_)


data Profile (f :: ProfileType -> Type) = Profile
  { arm :: ArmWavMeta
  , fit :: f Fit
  , original :: f Original
  }
  deriving (Generic)
instance Show (Profile f) where
  show p = "Profile " <> show p.arm.line


newtype WavOffset unit = WavOffset {value :: Float}
  deriving newtype (Show, Ord, Eq)


-- Metadata for an arm profile. Independent of Fit or Original (the wav meta is equal for both)
data ArmWavMeta = ArmWavMeta
  { line :: !SpectralLine
  , length :: !Int -- number of indices in the combined arms
  , pixel :: !Float
  , delta :: !(WavOffset Nm)
  }
  deriving (Show, Eq)

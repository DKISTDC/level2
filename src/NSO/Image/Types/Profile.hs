module NSO.Image.Types.Profile where

import NSO.Image.Headers.Types
import NSO.Prelude
import NSO.Types.Wavelength
import Telescope.Asdf (ToAsdf (..))
import Telescope.Data.DataCube
import Telescope.Data.KnownText


data ProfileType
  = Original
  | Fit


instance KnownText Original where
  knownText = "Original"
instance KnownText Fit where
  knownText = "Fit"


-- | A list with one entry per VISP arm
newtype Arms a = Arms {arms :: [a]}
  deriving (Foldable, Functor, Traversable, Show)


instance (ToAsdf a) => ToAsdf (Arms a) where
  toValue (Arms as) = toValue as


-- | Profile Data for a single FrameY (Scan Position), either Original or Fit
data ProfileImage (fit :: ProfileType) = ProfileImage
  { arm :: ArmWavMeta fit
  , data_ :: DataCube [SlitX, Wavelength Nm, Stokes] Float
  }


data Profile (f :: ProfileType -> Type) = Profile
  { fit :: f Fit
  , original :: f Original
  }
  deriving (Generic)


-- Metadata for an arm profile
data ArmWavMeta (fit :: ProfileType) = ArmWavMeta
  { pixel :: !Double
  , delta :: !(Wavelength Nm)
  , length :: !Int -- number of indices in the combined arms
  , line :: !SpectralLine
  }
  deriving (Show, Eq)

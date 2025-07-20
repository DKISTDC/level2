module NSO.Image.Types.Profile where

import NSO.Image.Types.Frame (SlitX, Stokes)
import NSO.Prelude
import NSO.Types.Wavelength
import Telescope.Data.DataCube
import Telescope.Data.KnownText


data ProfileType
  = Original
  | Fit
  deriving (Show, Eq)


data Profile


instance KnownText Original where
  knownText = "Original"
instance KnownText Fit where
  knownText = "Fit"


newtype ProfileImage fit = ProfileImage
  { data_ :: DataCube [SlitX, Wavelength Nm, Stokes] Float
  }


instance Show (ProfileImage fit) where
  show p = "ProfileImage " <> show (dataCubeAxes p.data_)


data ArmProfileImages = ArmProfileImages
  { arm :: ArmWavMeta
  , fit :: ProfileImage Fit
  , original :: ProfileImage Original
  }
instance Show ArmProfileImages where
  show p = "ArmProfileImages " <> show p.arm.line


-- data Profile (f :: ProfileType -> Type) = Profile
--   { arm :: ArmWavMeta
--   , fit :: f Fit
--   , original :: f Original
--   }
--   deriving (Generic)
-- instance Show (Profile f) where
--   show p = "Profile " <> show p.arm.line

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


data Profiles f = Profiles
  { fit :: f Fit
  , original :: f Original
  }

module NSO.Image.Types.Profile where

import NSO.Image.Headers.Types
import NSO.Prelude
import NSO.Types.Wavelength
import Telescope.Data.DataCube
import Telescope.Data.KnownText


data ProfileType
  = Original
  | Fit


instance KnownText Original where
  knownText = "Original"
instance KnownText Fit where
  knownText = "Fit"


newtype Arms a = Arms {arms :: [a]}
  deriving (Foldable)


data ProfileArm (fit :: ProfileType) = ProfileArm
  { meta :: WavMeta fit
  , frames :: [DataCube [SlitX, Wavelength Nm, Stokes] Float]
  }


-- first, divide by frame, then by arm in each frame
newtype ProfileArms (fit :: ProfileType) = ProfileArms
  { frames :: [Arms (ProfileFrame fit)]
  }


data ProfileFrame (fit :: ProfileType) = ProfileFrame
  { meta :: WavMeta fit
  , image :: DataCube [SlitX, Wavelength Nm, Stokes] Float
  }


-- Metadata for an arm profile
data WavMeta (fit :: ProfileType) = WavMeta
  { pixel :: Double
  , delta :: Wavelength Nm
  , length :: Int -- number of indices in the combined arms
  , line :: SpectralLine
  }
  deriving (Show, Eq)

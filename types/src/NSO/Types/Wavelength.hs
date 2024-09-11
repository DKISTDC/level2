{-# LANGUAGE StrictData #-}

module NSO.Types.Wavelength where

import Data.Scientific (fromFloatDigits)
import GHC.Real (Real)
import NSO.Prelude
import Rel8 (DBType)
import Telescope.Asdf


data Nm


newtype Wavelength a = Wavelength Double
  deriving newtype (Num, Ord, Show, Floating, Fractional, RealFloat, RealFrac, Real, DBType)


instance Eq (Wavelength a) where
  (Wavelength a) == (Wavelength b) =
    decimals a == decimals b
   where
    decimals :: Double -> Int
    decimals n = round (n * 100)


instance ToAsdf (Wavelength a) where
  toValue (Wavelength d) = Number (fromFloatDigits d)


-- | See https://bitbucket.org/dkistdc/dkist-spectral-lines/src/main/dkist_spectral_lines/lines.py
data SpectralLine
  = HeI
  | Ha
  | FeI
  | CaII CaIILine
  deriving (Eq, Ord)


instance Show SpectralLine where
  show HeI = "HeI"
  show Ha = "Hα"
  show FeI = "FeI"
  show (CaII l) = "CaII " <> show l


data CaIILine
  = CaII_849
  | CaII_854
  | CaII_866
  deriving (Bounded, Enum, Eq, Ord)


instance Show CaIILine where
  show CaII_849 = "849"
  show CaII_854 = "854"
  show CaII_866 = "866"

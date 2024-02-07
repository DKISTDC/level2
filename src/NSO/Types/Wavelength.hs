{-# LANGUAGE StrictData #-}

module NSO.Types.Wavelength where

import GHC.Real (Real)
import NSO.Prelude
import Rel8 (DBType)


data Nm


newtype Wavelength a = Wavelength Double
  deriving newtype (Num, Ord, Show, DBType, Floating, Fractional, RealFloat, RealFrac, Real)


instance Eq (Wavelength a) where
  (Wavelength a) == (Wavelength b) =
    decimals a == decimals b
   where
    decimals :: Double -> Int
    decimals n = round (n * 100)


-- | See https://bitbucket.org/dkistdc/dkist-spectral-lines/src/main/dkist_spectral_lines/lines.py
data SpectralLine
  = HeI
  | Ha
  | FeI
  | CaII CaIILine
  deriving (Eq)


instance Show SpectralLine where
  show HeI = "HeI"
  show Ha = "Hα"
  show FeI = "FeI"
  show (CaII l) = "CaII " <> show l


data CaIILine
  = CaII_849
  | CaII_854
  | CaII_866
  deriving (Bounded, Enum, Eq)


instance Show CaIILine where
  show CaII_849 = "849"
  show CaII_854 = "854"
  show CaII_866 = "866"

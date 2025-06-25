{-# LANGUAGE StrictData #-}

module NSO.Types.Wavelength where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (fromFloatDigits)
import GHC.Real (Real)
import NSO.Prelude
import Rel8 (DBType)
import Telescope.Asdf


-- Wavlength units
data Nm -- nanometers
data A -- angstroms
data MA -- milli-angstroms


newtype Wavelength a = Wavelength {value :: Double}
  deriving newtype (Num, Ord, Show, Floating, Fractional, RealFloat, RealFrac, Real, DBType, FromJSON, ToJSON)


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
  = NaD
  | FeI
  | Ha
  | CaII CaIILine
  | HeI
  deriving (Eq, Ord)


instance Show SpectralLine where
  show HeI = "HeI"
  show Ha = "Hα"
  show FeI = "FeI"
  show (CaII l) = "CaII " <> show l
  show NaD = "Na D"


data CaIILine
  = CaII_849
  | CaII_854
  | CaII_866
  deriving (Bounded, Enum, Eq, Ord)
instance Show CaIILine where
  show CaII_849 = "849"
  show CaII_854 = "854"
  show CaII_866 = "866"

--  Ca II (854.21 nm) cm
--

-- Level 1 Spectral Line Names:
-- https://bitbucket.org/dkistdc/dkist-spectral-lines/src/main/dkist_spectral_lines/models.py
--
--     H_GAMMA = "H gamma"
--     H_DELTA = "H delta"
--     FE_I = "Fe I"
--     TI_I = "Ti I"
--     FE_XIII = "Fe XIII"
--     NI_I = "Ni I"
--     CA_II_H = "Ca II H"
--     H_BETA = "H beta"
--     CA_II = "Ca II"
--     K_I = "K I"
--     CA_I = "Ca I"
--     SR_I = "Sr I"
--     HE_I_D3 = "He I D3"
--     H_EPSILON = "H epsilon"
--     SI_X = "Si X"
--     MG_I_B2 = "Mg I b2"
--     HE_I = "He I"
--     MN_I = "Mn I"
--     BA_II = "Ba II"
--     NA_I_D2 = "Na I D2"
--     FE_XIV = "Fe XIV"
--     NA_I_D1 = "Na I D1"
--     SI_IX = "Si IX"
--     FE_XI = "Fe XI"
--     H_ALPHA = "H alpha"
--     CA_II_K = "Ca II K"
--     NA_I = "Na I"
--     MG_I_B1 = "Mg I b1"

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


{- | See https://bitbucket.org/dkistdc/dkist-spectral-lines/src/main/dkist_spectral_lines/lines.py
there are more spectral lines, but these are the ones we use for inversion
-}
data SpectralLine
  = NaID -- pair of D1 and D2: 589.0 and 589.6
  | FeI630 -- pair of 630.15 and 630.25
  | CaII854 -- just the one
  deriving (Eq, Ord, Show, Bounded, Enum)


ionName :: SpectralLine -> Text
ionName = \case
  NaID -> "Na I"
  FeI630 -> "Fe I"
  CaII854 -> "Ca II"


fromIonName :: Text -> Maybe SpectralLine
fromIonName = \case
  "Na I" -> pure NaID
  "Fe I" -> pure FeI630
  "Ca II" -> pure CaII854
  _ -> Nothing

--  Ca II (854.21 nm) cm

-- Level 1 Spectral Line Names:
-- https://bitbucket.org/dkistdc/dkist-spectral-lines/src/main/dkist_spectral_lines/models.py
--
--     TI_I = "Ti I"
--     SR_I = "Sr I"
--     SI_X = "Si X"
--     SI_IX = "Si IX"
--     NI_I = "Ni I"
--     NA_I_D2 = "Na I D2"
--     NA_I_D1 = "Na I D1"
--     NA_I = "Na I"
--     MN_I = "Mn I"
--     MG_I_B2 = "Mg I b2"
--     MG_I_B1 = "Mg I b1"
--     K_I = "K I"
--     H_GAMMA = "H gamma"
--     H_EPSILON = "H epsilon"
--     H_DELTA = "H delta"
--     H_BETA = "H beta"
--     H_ALPHA = "H alpha"
--     HE_I_D3 = "He I D3"
--     HE_I = "He I"
--     FE_XIV = "Fe XIV"
--     FE_XIII = "Fe XIII"
--     FE_XI = "Fe XI"
--     FE_I = "Fe I"
--     CA_II_K = "Ca II K"
--     CA_II_H = "Ca II H"
--     CA_II = "Ca II"
--     CA_I = "Ca I"
--     BA_II = "Ba II"

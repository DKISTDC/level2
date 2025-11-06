{-# LANGUAGE StrictData #-}

module NSO.Types.Wavelength where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isNumber)
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import GHC.Real (Real)
import NSO.Prelude
import Rel8 (DBType)
import Telescope.Asdf
import Text.Read (readMaybe)


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
data SpectralLine = SpectralLine
  { ion :: Ion
  , designation :: Maybe Designation
  , wavelength :: Wavelength Nm
  }
  deriving (Show, Eq)


data Designation
  = D1
  | D2
  | B1
  | Designation Text
  deriving (Show, Read, Eq)


instance ToAsdf SpectralLine where
  toValue = String . renderSpectralLine


ionName :: Ion -> Text
ionName = \case
  NaI -> "Na I"
  FeI -> "Fe I"
  CaII -> "Ca II"
  Ion t -> t


fromIonName :: Text -> Ion
fromIonName = \case
  "Na I" -> NaI
  "Fe I" -> FeI
  "Ca II" -> CaII
  t -> Ion t


data Ion
  = NaI
  | FeI
  | CaII
  | Ion Text
  deriving (Eq, Ord, Show)


renderSpectralLine :: SpectralLine -> Text
renderSpectralLine _ = "Mg I b1 (517.28 nm)"


parseSpectralLine :: Text -> Either String SpectralLine
parseSpectralLine inp = do
  (iont, dest) <- prefix inp
  w <- wavelength inp
  let md = fmap designation dest
  let ion = fromIonName iont
  pure $ SpectralLine ion md w
 where
  designation t =
    case readMaybe (cs t) of
      Nothing -> Designation t
      Just d -> d

  -- Na I D1 (...
  -- (Na I, Just D1)
  prefix start = do
    case T.words $ T.takeWhile (/= '(') start of
      [elm, ion] -> pure (elm <> " " <> ion, Nothing)
      [elm, ion, des] -> pure (elm <> " " <> ion, Just $ T.toUpper des)
      ws -> Left $ "Could not match SpectralLine prefix: " <> show ws

  -- asdf2 (630.15 nm)
  wavelength end = do
    case T.stripSuffix " nm)" $ T.drop 1 $ T.dropWhile (/= '(') end of
      Nothing -> Left $ "Could not locate SpectralLine wavelength: " <> show end
      Just wt -> do
        f <- maybe (Left $ "Could not parse SpectralLine wavelength: " <> show wt) pure $ readMaybe @Double (cs wt)
        pure $ Wavelength f

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

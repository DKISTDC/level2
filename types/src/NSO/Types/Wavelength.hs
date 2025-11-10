{-# LANGUAGE StrictData #-}

module NSO.Types.Wavelength where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (toUpper)
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Real (Real)
import NSO.Prelude
import Numeric (showFFloat)
import Rel8 (DBType, parseTypeInformation, typeInformation)
import Telescope.Asdf (ToAsdf (..), Value (..))
import Text.Casing (wordify)
import Text.Megaparsec as M hiding (ParseError, Token)
import Text.Megaparsec.Char as M
import Text.Megaparsec.Char.Lexer as ML
import Text.Read (readMaybe)


type SpecParser = Parsec Void Text


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
  deriving (Eq, Ord)


instance Show SpectralLine where
  show = cs . spectralLineName


instance DBType SpectralLine where
  typeInformation = parseTypeInformation parseSpectralLine spectralLineName (typeInformation @Text)


data Designation
  = D1
  | D2
  | B1
  | Alpha
  | Beta
  | Designation Text
  deriving (Show, Read, Eq, Ord)


designationName :: Designation -> Text
designationName (Designation t) = t
designationName Alpha = "alpha"
designationName Beta = "beta"
designationName d = cs (show d)


instance ToAsdf SpectralLine where
  toValue = String . spectralLineName


data Ion
  = NaI
  | FeI
  | CaII
  | H
  | SrI
  | Ion Text
  | UnknownIon
  deriving (Eq, Ord, Show, Read)


ionShort :: Ion -> Text
ionShort = T.replace " " "" . ionName


-- | Fe I, H, Mg I
ionName :: Ion -> Text
ionName = \case
  Ion t -> cs $ wordify $ cs t
  UnknownIon -> ""
  i -> cs $ wordify $ show i


-- | Fe I, H, Mg I
fromIonName :: Text -> Ion
fromIonName = \case
  "" -> UnknownIon
  t -> case readMaybe (cs $ T.replace " " "" t) of
    Just i -> i
    Nothing -> Ion (T.replace " " "" t)


spectralLineShort :: SpectralLine -> Text
spectralLineShort s =
  T.intercalate " " $
    catMaybes
      [ knownIonName s.ion
      , designationName <$> s.designation
      ]
 where
  knownIonName UnknownIon = Nothing
  knownIonName i = pure $ ionName i


spectralLineName :: SpectralLine -> Text
spectralLineName s =
  spectralLineShort s <> " " <> cs ("(" <> showFFloat (Just 2) s.wavelength " nm)")


parseSpectralLine :: Text -> Either String SpectralLine
parseSpectralLine inp =
  case M.runParser spectralLine (cs inp) inp of
    Left err -> Left $ M.errorBundlePretty err
    Right s -> pure s
 where
  -- ion ion
  -- ion ion d
  -- ion d

  spectralLine :: SpecParser SpectralLine
  spectralLine = do
    i <- ion
    M.space
    d <- optional designation
    M.space
    w <- wavelength
    pure $ SpectralLine i d w

  ion :: SpecParser Ion
  ion = M.try ionWithNumeral <|> (fromIonName <$> nextWord)

  ionWithNumeral = do
    i <- nextWord
    M.space
    num <- M.takeWhile1P (Just "Numeral") (`elem` ("IVX" :: String))
    pure $ fromIonName (i <> " " <> num)

  designation :: SpecParser Designation
  designation = do
    w <- nextWord
    let ws = upperFirst $ cs w
    case readMaybe ws of
      Nothing -> pure $ Designation $ T.toUpper $ cs ws
      Just d -> pure d
   where
    upperFirst (c : s) = toUpper c : s
    upperFirst s = s

  nextWord :: SpecParser Text
  nextWord = cs <$> M.some M.alphaNumChar

  wavelength :: SpecParser (Wavelength Nm)
  wavelength = do
    _ <- M.char '('
    d <- ML.float
    _ <- M.string' " nm)"
    pure $ Wavelength d

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

module NSO.Image.Headers.Types where

-- import Control.Exception (Exception)
-- import Data.Massiv.Array (Ix3, Ix4, Sz (..))

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as A
import Data.Text (pack)
import Data.Text qualified as T
import GHC.TypeLits
import NSO.Image.Headers.Keywords
import NSO.Prelude
import NSO.Types.Wavelength (Ion, fromIonName, ionName)
import Telescope.Data.KnownText
import Telescope.Data.Parser (expected)
import Telescope.Fits.Header
import Prelude (Real)


-- Keywords ------------------------------------------------------------------

data ExtName (ext :: Symbol) = ExtName
  deriving (Generic)
instance (KnownSymbol ext) => KeywordInfo (ExtName ext) where
  description = "Name of the HDU"
  allowed = [toKeywordValue @(ExtName ext) ExtName]
instance (KnownSymbol ext) => ToKeyword (ExtName ext) where
  toKeywordValue _ = String (pack $ symbolVal @ext Proxy)
instance IsKeyword (ExtName ext) where
  keyword = "EXTNAME"


data BType (ucd :: Symbol) = BType
instance (KnownSymbol ucd) => KeywordInfo (BType ucd) where
  keytype = "Uniform Content Descriptor"
  allowed = [toKeywordValue @(BType ucd) BType]
  description = "The type of the values in the data array"
  comment = "[ucd]"
instance (KnownSymbol ucd) => ToKeyword (BType ucd) where
  toKeywordValue _ = String (pack $ symbolVal @ucd Proxy)
instance IsKeyword (BType ucd) where
  keyword = "BTYPE"


data BUnit (unit :: Unit) = BUnit
instance (KnownValue unit) => KeywordInfo (BUnit unit) where
  keytype = "Unit"
  allowed = [toKeywordValue @(BUnit unit) BUnit]
  description = "The unit of the values in the data array"
instance (KnownValue unit) => ToKeyword (BUnit unit) where
  toKeywordValue _ = knownValue @unit
instance IsKeyword (BUnit unit) where
  keyword = "BUNIT"


data Key ktype (description :: Symbol) = Key {ktype :: ktype}
  deriving (Show, Eq)
instance {-# OVERLAPPABLE #-} (KeyType ktype, KnownSymbol desc) => KeywordInfo (Key ktype desc) where
  keytype = typeName @ktype
  description = pack $ symbolVal @desc Proxy
  allowed = []
  comment = typeComment @ktype
instance {-# OVERLAPPABLE #-} (ToKeyword ktype) => ToKeyword (Key ktype desc) where
  toKeywordValue (Key k) = toKeywordValue @ktype k
instance (FromKeyword ktype) => FromKeyword (Key ktype desc) where
  parseKeywordValue val = Key <$> parseKeywordValue val


instance (KnownValue kvalue, KnownSymbol desc) => KeywordInfo (Key (Constant kvalue) desc) where
  keytype = "Constant"
  allowed = [typeValue @(Constant kvalue) Constant]
  description = pack $ symbolVal @desc Proxy
instance (KnownValue kvalue) => ToKeyword (Key (Constant kvalue) desc) where
  toKeywordValue _ = typeValue @(Constant kvalue) Constant


data Naxis cmt = Naxis
instance (KnownSymbol cmt) => KeywordInfo (Naxis cmt) where
  keytype = "Int"
  description = pack $ symbolVal @cmt Proxy
  comment = pack $ symbolVal @cmt Proxy


-- you shouldn't convert this into a value... it's done by the library
-- instance (KnownSymbol cmt) => ToKeyword (Naxis cmt) where
--   toKeywordValue _ = Integer 0

data NaxisY = NaxisY
instance KeywordInfo NaxisY where
  keytype = "Int"
  description = "Dummy WCS Y Coordinate"
  comment = "Dummy WCS Y Coordinate"
  allowed = [Integer 1]


-- instance ToKeyword NaxisY where
--   toKeywordValue _ = Integer 1

data BZero = BZero
instance KeywordInfo BZero where
  keytype = "Float"
  description = "This keyword represents the physical value corresponding to an array value of zero. The default value for this keyword is 0.0. This keyword, along with BSCALE, is used to linearly scale the array pixel values to transform them into the phyical values that they represent. physical_value = BZERO + BSCALE x array_value"
  allowed = [toKeywordValue BZero]
instance ToKeyword BZero where
  toKeywordValue _ = Integer 0
instance FromKeyword BZero where
  parseKeywordValue _ = pure BZero


data BScale = BScale
instance KeywordInfo BScale where
  keytype = "Float"
  description = "This keyword represents the coefficient of the linear term in the scaling equation, the ratio of physical value to array value at zero offset. The default value for this keyword is 1.0. This keyword, along with BZERO, is used to linearly scale the array pixel values to transform them into the phyical values that they represent."
  allowed = [toKeywordValue BScale]
instance ToKeyword BScale where
  toKeywordValue _ = Integer 1
instance FromKeyword BScale where
  parseKeywordValue _ = pure BScale


newtype Object = Object Text
  deriving newtype (ToKeyword, FromKeyword)
instance KeywordInfo Object where
  keytype = "Object"
  description = "The value field shall contain a character string giving a name for the observed object. Applicable standard values are TBD"
  allowed = fmap String ["unknown", "quietsun", "sunspot", "pore", "plages", "spicules", "filament", "prominence", "coronalhole", "quietcorona", "activecorona"]


newtype Instrument = Instrument Text
  deriving newtype (ToKeyword, FromKeyword, Eq)
instance KeywordInfo Instrument where
  keytype = "Instrument"
  description = "The instrument used to acquire the data associated with the header"
  allowed = fmap String ["VBI", "VISP", "VTF", "DL-NIRSP", "CRYO-NIRSP", "WFC"]


newtype OCSCtrl = OCSCtrl Text
  deriving (Generic)
  deriving newtype (ToKeyword, FromKeyword)
instance KeywordInfo OCSCtrl where
  keytype = "OCSCtrl"
  description = "Control mode the telescope was operated in: ‘Auto’: Data were acquired as part of a regular, automatic execution of an Observing Program ‘Manual’: Data were acquired executing either a part of an or a complete Observing Program manually"
  allowed = fmap String ["auto", "manual"]


newtype EnumKey (ss :: Symbol) (desc :: Symbol) = EnumKey Text
  deriving (Generic)
  deriving newtype (ToKeyword, FromKeyword)
instance (KnownSymbol desc, KnownSymbol ss) => KeywordInfo (EnumKey ss desc) where
  keytype = "Enum"
  description = pack $ symbolVal @desc Proxy
  allowed = fmap String $ T.splitOn "/" $ pack $ symbolVal @ss Proxy


-- Key Types ---------------------------------------------------------

newtype MB = MB Double
  deriving (Generic)
  deriving newtype (ToKeyword, FromKeyword)
instance KeyType MB where
  typeValue (MB s) = Float s


newtype Seconds = Seconds Double
  deriving (Generic)
  deriving newtype (ToKeyword, FromKeyword)
instance KeyType Seconds where
  typeComment = "[s]"
  typeValue (Seconds s) = Float s


newtype Degrees = Degrees Double
  deriving (Generic)
  deriving newtype (ToKeyword, FromKeyword)
instance KeyType Degrees where
  typeComment = "[deg]"
  typeValue (Degrees s) = Float s


data Constant c = Constant deriving (Generic)
instance (KnownValue c) => KeyType (Constant c) where
  typeValue _ = knownValue @c
  typeComment = ""
instance (KnownValue c) => FromKeyword (Constant c) where
  parseKeywordValue _ = pure Constant
instance (KnownText c) => ToJSON (Constant c) where
  toJSON _ = A.String (knownText @c)
instance (KnownText c) => Show (Constant c) where
  show _ = "Constant \"" <> cs (knownText @c) <> "\""


newtype Url = Url Text
  deriving (Generic)
  deriving newtype (ToKeyword, FromKeyword)
instance KeyType Url where
  typeValue (Url u) = String u
  typeComment = ""


newtype Meters = Meters Double
  deriving (Generic)
  deriving newtype (ToKeyword, FromKeyword)
instance KeyType Meters where
  typeValue (Meters m) = Float m
  typeComment = "[m]"


newtype Mps = Mps Double
  deriving (Generic)
  deriving newtype (ToKeyword, FromKeyword)
instance KeyType Mps where
  typeValue (Mps m) = Float m
  typeComment = "[m/s]"


-- Units -------------------------------------------------------------------

data Unit
  = Dimensionless
  | Kelvin
  | N_m2
  | Km_s
  | Tesla
  | Deg
  | Km
  | Kg_m3
  deriving (Generic)
instance Show Unit where
  show Dimensionless = ""
  show Kelvin = "K"
  show N_m2 = "N/m^2"
  show Km_s = "km/s"
  show Tesla = "T"
  show Deg = "deg"
  show Km = "km"
  show Kg_m3 = "kg/m^3"


instance KnownText N_m2 where
  knownText = pack $ show N_m2
instance KnownText Km_s where
  knownText = pack $ show Km_s
instance KnownText Dimensionless where
  knownText = pack $ show Dimensionless
instance KnownText Kelvin where
  knownText = pack $ show Kelvin
instance KnownText Tesla where
  knownText = pack $ show Tesla
instance KnownText Deg where
  knownText = pack $ show Deg
instance KnownText Km where
  knownText = pack $ show Km
instance KnownText Kg_m3 where
  knownText = pack $ show Kg_m3
instance KnownValue N_m2
instance KnownValue Km_s
instance KnownValue Dimensionless
instance KnownValue Kelvin
instance KnownValue Tesla
instance KnownValue Deg
instance KnownValue Km
instance KnownValue Kg_m3


newtype PixelsPerBin = PixelsPerBin Int
  deriving newtype (Show, Eq, Num, Ord, Real, Enum, Integral, FromKeyword)


-- | Headers in inv_res_pre (Profile Fit) that describe the subsection of the canonical data we use
data SliceXY = SliceXY
  { pixelsPerBin :: PixelsPerBin
  , fiducialArmId :: VISPArmId
  }
  deriving (Show, Eq)


newtype VISPArmId = VISPArmId Int
  deriving newtype (FromKeyword, Eq, Show)


newtype ProfType = ProfType Text
  deriving (Generic)
  deriving newtype (FromKeyword, ToKeyword)
instance IsKeyword ProfType where
  keyword = "SPECPFTP"
instance KeywordInfo ProfType


newtype ProfIon = ProfIon Ion
  deriving (Generic)
instance IsKeyword ProfIon where
  keyword = "SPECPFIN"
instance ToKeyword ProfIon where
  toKeywordValue (ProfIon p) = String (ionName p)
instance FromKeyword ProfIon where
  parseKeywordValue = \case
    String val -> pure $ ProfIon $ fromIonName val
    other -> expected "ProfIon" other
instance KeywordInfo ProfIon

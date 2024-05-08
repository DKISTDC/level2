module NSO.Fits.Generate.Types where

import Control.Exception (Exception)
import Data.Massiv.Array (Ix3, Ix4, Sz (..))
import Data.Text (pack)
import Data.Text qualified as T
import GHC.TypeLits
import NSO.Fits.Generate.Keywords
import NSO.Prelude
import Telescope.Fits (Value (..))


data Depth
data SlitX
data FrameY
data Quantity
data Wavs -- The combined wavelength array
data Stokes
data WavIds


-- Keywords ------------------------------------------------------------------

data ExtName (ext :: Symbol) = ExtName
  deriving (Generic)
instance (KnownSymbol ext) => KeywordInfo (ExtName ext) where
  keyword = "extname"
  description = "Name of the HDU"
  allowed = [keyValue @(ExtName ext) ExtName]
  keyValue _ = String (pack $ symbolVal @ext Proxy)


data BType (ucd :: Symbol) = BType deriving (Generic)
instance (KnownSymbol ucd) => KeywordInfo (BType ucd) where
  keyword = "btype"
  keytype = "Uniform Content Descriptor"
  allowed = [keyValue @(BType ucd) BType]
  description = "The type of the values in the data array"
  comment = "[ucd]"
  keyValue _ = String (pack $ symbolVal @ucd Proxy)


data BUnit (unit :: Unit) = BUnit deriving (Generic)
instance (KnownValue unit) => KeywordInfo (BUnit unit) where
  keyword = "bunit"
  keytype = "Unit"
  allowed = [keyValue @(BUnit unit) BUnit]
  description = "The unit of the values in the data array"
  keyValue _ = knownValue @unit


data Key ktype (description :: Symbol) = Key ktype
  deriving (Generic, Show)
instance {-# OVERLAPPABLE #-} (KeyType ktype, KnownSymbol desc) => KeywordInfo (Key ktype desc) where
  keytype = typeName @ktype
  description = pack $ symbolVal @desc Proxy
  allowed = []
  comment = typeComment @ktype
  keyValue (Key t) = typeValue @ktype t


instance (KnownValue kvalue, KnownSymbol desc) => KeywordInfo (Key (Constant kvalue) desc) where
  keytype = "Constant"
  allowed = [typeValue @(Constant kvalue) Constant]
  description = pack $ symbolVal @desc Proxy
  keyValue _ = typeValue @(Constant kvalue) Constant


data Naxis cmt = Naxis deriving (Generic)
instance (KnownSymbol cmt) => KeywordInfo (Naxis cmt) where
  keytype = "Int"
  description = pack $ symbolVal @cmt Proxy
  comment = pack $ symbolVal @cmt Proxy
  keyValue _ = Integer 0


data NaxisY = NaxisY deriving (Generic)
instance KeywordInfo NaxisY where
  keytype = "Int"
  description = "Dummy WCS Y Coordinate"
  comment = "Dummy WCS Y Coordinate"
  allowed = [keyValue NaxisY]
  keyValue _ = Integer 1


data BZero = BZero deriving (Generic)
instance KeywordInfo BZero where
  keytype = "Float"
  description = "This keyword represents the physical value corresponding to an array value of zero. The default value for this keyword is 0.0. This keyword, along with BSCALE, is used to linearly scale the array pixel values to transform them into the phyical values that they represent. physical_value = BZERO + BSCALE x array_value"
  allowed = [keyValue BZero]
  keyValue _ = Integer 0


data BScale = BScale deriving (Generic)
instance KeywordInfo BScale where
  keytype = "Float"
  description = "This keyword represents the coefficient of the linear term in the scaling equation, the ratio of physical value to array value at zero offset. The default value for this keyword is 1.0. This keyword, along with BZERO, is used to linearly scale the array pixel values to transform them into the phyical values that they represent."
  allowed = [keyValue BScale]
  keyValue _ = Integer 1


data Object = Object Text deriving (Generic)
instance KeywordInfo Object where
  keytype = "Object"
  description = "The value field shall contain a character string giving a name for the observed object. Applicable standard values are TBD"
  allowed = fmap String ["unknown", "quietsun", "sunspot", "pore", "plages", "spicules", "filament", "prominence", "coronalhole", "quietcorona", "activecorona"]
  keyValue (Object s) = String s


data Instrument = Instrument Text deriving (Generic)
instance KeywordInfo Instrument where
  keytype = "Instrument"
  description = "The instrument used to acquire the data associated with the header"
  allowed = fmap String ["VBI", "VISP", "VTF", "DL-NIRSP", "CRYO-NIRSP", "WFC"]
  keyValue (Instrument s) = String s


data OCSCtrl = OCSCtrl Text deriving (Generic)
instance KeywordInfo OCSCtrl where
  keytype = "OCSCtrl"
  description = "Control mode the telescope was operated in: ‘Auto’: Data were acquired as part of a regular, automatic execution of an Observing Program ‘Manual’: Data were acquired executing either a part of an or a complete Observing Program manually"
  allowed = fmap String ["auto", "manual"]
  keyValue (OCSCtrl s) = String s


data EnumKey (ss :: Symbol) (desc :: Symbol) = EnumKey Text deriving (Generic)
instance (KnownSymbol desc, KnownSymbol ss) => KeywordInfo (EnumKey ss desc) where
  keytype = "Enum"
  description = pack $ symbolVal @desc Proxy
  allowed = fmap String $ T.splitOn "/" $ pack $ symbolVal @ss Proxy
  keyValue (EnumKey s) = String s


-- Key Types ---------------------------------------------------------

newtype MB = MB Float deriving (Generic)
instance KeyType MB where
  typeValue (MB s) = Float s


newtype Seconds = Seconds Float deriving (Generic)
instance KeyType Seconds where
  typeComment = "[s]"
  typeValue (Seconds s) = Float s


newtype Degrees = Degrees Float deriving (Generic)
instance KeyType Degrees where
  typeComment = "[deg]"
  typeValue (Degrees s) = Float s


data Constant c = Constant deriving (Generic)
instance (KnownValue c) => KeyType (Constant c) where
  typeValue _ = knownValue @c


data DateTime = DateTime Text deriving (Generic)
instance KeyType DateTime where
  typeValue (DateTime s) = String s
  typeComment = ""


data Url = Url Text deriving (Generic)
instance KeyType Url where
  typeValue (Url u) = String u
  typeComment = ""


data Meters = Meters Float deriving (Generic)
instance KeyType Meters where
  typeValue (Meters m) = Float m
  typeComment = "[m]"


data Mps = Mps Float deriving (Generic)
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


instance KnownValue N_m2 where
  knownValue = String $ pack $ show N_m2
instance KnownValue Km_s where
  knownValue = String $ pack $ show Km_s
instance KnownValue Dimensionless where
  knownValue = String $ pack $ show Dimensionless
instance KnownValue Kelvin where
  knownValue = String $ pack $ show Kelvin
instance KnownValue Tesla where
  knownValue = String $ pack $ show Tesla
instance KnownValue Deg where
  knownValue = String $ pack $ show Deg
instance KnownValue Km where
  knownValue = String $ pack $ show Km
instance KnownValue Kg_m3 where
  knownValue = String $ pack $ show Kg_m3


data WCSAlt
  = WCSMain
  | A


instance KnownValue WCSMain where
  knownValueText = ""
instance KnownValue A where
  knownValueText = "A"


-- Error -------------------------------------------------------------
data GenerateError
  = InvalidFrameShape (Sz Ix3)
  | InvalidFits String
  | FrameOutOfBounds (Sz Ix4) Int
  | MissingProfileExtensions String
  | InvalidWavelengthGroups
  deriving (Show, Eq, Exception)

module NSO.Fits.Generate.Types where

import Data.Text (pack)
import GHC.TypeLits
import NSO.Fits.Generate.Keywords
import NSO.Prelude
import Telescope.Fits (Value (..))


data NamedKey (keyword :: Symbol) ktype (description :: Symbol) = NamedKey ktype


newtype MB = MB Float deriving (Generic)
instance KeyValue MB where
  keyValue (MB s) = Float s
instance KeyType MB


newtype Seconds = Seconds Float deriving (Generic)
instance KeyType Seconds where
  typeComment = "[s]"
instance KeyValue Seconds where
  keyValue (Seconds s) = Float s


newtype Degrees = Degrees Float deriving (Generic)
instance KeyType Degrees where
  typeComment = "[deg]"
instance KeyValue Degrees where
  keyValue (Degrees s) = Float s


data ExtName (ext :: Symbol) = ExtName
  deriving (Generic)
instance (KnownSymbol ext) => KeyValue (ExtName ext) where
  keyValue _ = String (pack $ symbolVal @ext Proxy)
instance (KnownSymbol ext) => KeywordInfo (ExtName ext) where
  keyword = "extname"
  description = "Name of the HDU"
  constant = Just (keyValue @(ExtName ext) ExtName)


data BType (ucd :: Symbol) = BType deriving (Generic)
instance (KnownSymbol ucd) => KeyValue (BType ucd) where
  keyValue _ = String (pack $ symbolVal @ucd Proxy)
instance (KnownSymbol ucd) => KeywordInfo (BType ucd) where
  keyword = "btype"
  keytype = "Uniform Content Descriptor"
  constant = Just (keyValue @(BType ucd) BType)
  description = "The type of the values in the data array"
  comment = "[ucd]"


data BUnit (unit :: Unit) = BUnit deriving (Generic)
instance (KnownValue unit) => KeyValue (BUnit unit) where
  keyValue _ = String (pack $ knownValue @unit)
instance (KnownValue unit) => KeywordInfo (BUnit unit) where
  keyword = "bunit"
  keytype = "Unit"
  constant = Just (keyValue @(BUnit unit) BUnit)
  description = "The unit of the values in the data array"


-- well, they are ALL floats
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
  knownValue = show N_m2
instance KnownValue Km_s where
  knownValue = show Km_s
instance KnownValue Dimensionless where
  knownValue = show Dimensionless
instance KnownValue Kelvin where
  knownValue = show Kelvin
instance KnownValue Tesla where
  knownValue = show Tesla
instance KnownValue Deg where
  knownValue = show Deg
instance KnownValue Km where
  knownValue = show Km
instance KnownValue Kg_m3 where
  knownValue = show Kg_m3


data Depth
data SlitX
data FrameY
data Quantity
data DummyY


data Constant c = Constant
instance (KnownValue c) => KeyValue (Constant c) where
  keyValue _ = String (pack $ knownValue @c)


data Key ktype (description :: Symbol) = Key ktype
  deriving (Generic)
instance (KeyValue ktype) => KeyValue (Key ktype desc) where
  keyValue (Key k) = keyValue k


instance {-# OVERLAPPABLE #-} (KeyType ktype, KnownSymbol desc) => KeywordInfo (Key ktype desc) where
  keytype = typeName @ktype
  description = pack $ symbolVal @desc Proxy
  constant = Nothing
  comment = typeComment @ktype


instance (KnownValue kvalue, KnownSymbol desc) => KeywordInfo (Key (Constant kvalue) desc) where
  keytype = "Constant"
  constant = Just (keyValue @(Constant kvalue) Constant)
  description = pack $ symbolVal @desc Proxy


data Naxis cmt deriving (Generic)


instance (KnownSymbol cmt) => KeywordInfo (Naxis cmt) where
  keytype = "Int"
  description = pack $ symbolVal @cmt Proxy
  comment = pack $ symbolVal @cmt Proxy


instance KeywordInfo NaxisY where
  keytype = "Int"
  description = "Dummy WCS Y Coordinate"
  comment = "Dummy WCS Y Coordinate"
  constant = Just (Integer 1)


data NaxisY deriving (Generic)

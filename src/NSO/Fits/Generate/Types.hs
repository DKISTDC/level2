module NSO.Fits.Generate.Types where

import Data.Text (pack)
import GHC.TypeLits
import NSO.Fits.Generate.Keywords
import NSO.Prelude
import Telescope.Fits (Value (..))


data Depth
data SlitX
data FrameY
data Quantity
data DummyY


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
  keyValue _ = String (pack $ knownValue @unit)


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


data PC (i :: Nat) (j :: Nat) = PC Float
instance (KnownNat i, KnownNat j) => KeywordInfo (PC i j) where
  keyword = "PC" <> showN @i Proxy <> "_" <> showN @j Proxy
   where
    showN :: forall n. (KnownNat n) => Proxy n -> Text
    showN p = pack (show $ natVal p)
  keytype = "PCi_j"
  description = "Linear transformation matrix used with the Helioprojective coordinate system"
  keyValue (PC n) = Float n


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
  typeValue _ = String (pack $ knownValue @c)


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

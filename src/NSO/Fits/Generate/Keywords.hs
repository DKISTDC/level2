{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Fits.Generate.Keywords where

import Data.Fits (KeywordRecord (..), Value (..))
import Data.Text (pack)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Types
import NSO.Prelude


-- | Generate a 'KeywordRecord' from a type that supports it
keywordRecord :: forall a. (KeyValue a, KeywordInfo a) => a -> KeywordRecord
keywordRecord a = KeywordRecord (keyword @a) (keyValue a) comt
 where
  comt =
    case comment @a of
      "" -> Nothing
      s -> Just s


class HeaderKeywords a where
  -- | Generate '[KeywordRecord]' from a compound type with multiple fields
  headerKeywords :: a -> [KeywordRecord]
  default headerKeywords :: (Generic a, GenHeaderKeywords (Rep a)) => a -> [KeywordRecord]
  headerKeywords = genHeaderKeywords . from


class GenHeaderKeywords f where
  genHeaderKeywords :: f p -> [KeywordRecord]


-- datatype metadata
instance (GenHeaderKeywords f) => GenHeaderKeywords (M1 D c f) where
  genHeaderKeywords (M1 a) = genHeaderKeywords a


-- constructor metadata
instance (GenHeaderKeywords f) => GenHeaderKeywords (M1 C c f) where
  genHeaderKeywords (M1 a) = genHeaderKeywords a


-- Selectors
instance (GenHeaderKeywords f, Selector s) => GenHeaderKeywords (M1 S s f) where
  genHeaderKeywords (M1 a) =
    let s = selName (undefined :: M1 S s f x)
     in fmap (setKeyword s) $ genHeaderKeywords a
   where
    setKeyword s d = d{_keyword = pack s}


instance (GenHeaderKeywords a, GenHeaderKeywords b) => GenHeaderKeywords (a :*: b) where
  genHeaderKeywords (a :*: b) = genHeaderKeywords a ++ genHeaderKeywords b


instance (KeywordInfo a, KeyValue a) => GenHeaderKeywords (K1 i a) where
  genHeaderKeywords (K1 a) = [keywordRecord a]


class KeyValue a where
  keyValue :: a -> Value


-- instance KeyValue Unit where
--   keyValue u = String (pack $ show u)
--
--
-- instance KeyValue UCD where
--   keyValue u = String (pack $ fromUCD u)

instance (KnownSymbol ext) => KeyValue (ExtName ext) where
  keyValue _ = String (pack $ symbolVal @ext Proxy)


instance (KnownValue ucd) => KeyValue (BType ucd) where
  keyValue _ = String (pack $ knownValue @ucd)


instance (KnownValue unit) => KeyValue (BUnit unit) where
  keyValue _ = String (pack $ knownValue @unit)


instance (KeyValue ktype) => KeyValue (Key ktype desc) where
  keyValue (Key k) = keyValue k


instance (KnownValue c) => KeyValue (Constant c) where
  keyValue _ = String (pack $ knownValue @c)


instance KeyValue String where
  keyValue s = String (pack s)


instance KeyValue Seconds where
  keyValue (Seconds s) = Float s


instance KeyValue MB where
  keyValue (MB s) = Float s


instance KeyValue Degrees where
  keyValue (Degrees s) = Float s


class KeywordInfo a where
  keyword :: Text
  default keyword :: (Generic a, GTypeName (Rep a)) => Text
  keyword = pack $ gtypeName (from (undefined :: a))


  keytype :: Text
  default keytype :: (Generic a, GTypeName (Rep a)) => Text
  keytype = pack $ gtypeName (from (undefined :: a))


  description :: Text
  default description :: Text
  description = ""


  comment :: Text
  default comment :: Text
  comment = ""


  constant :: Maybe Value
  default constant :: Maybe Value
  constant = Nothing


instance (KnownSymbol s) => KeywordInfo (ExtName s) where
  keyword = "extname"
  description = "Name of the HDU"
  constant = Just (keyValue @(ExtName s) ExtName)


instance (KnownValue ucd) => KeywordInfo (BType ucd) where
  keyword = "btype"
  keytype = "Uniform Content Descriptor"
  constant = Just (keyValue @(BType ucd) BType)
  description = "The type of the values in the data array"
  comment = "[ucd]"


instance (KnownValue unit) => KeywordInfo (BUnit unit) where
  keyword = "bunit"
  keytype = "Unit"
  constant = Just (keyValue @(BUnit unit) BUnit)
  description = "The unit of the values in the data array"


instance {-# OVERLAPPABLE #-} (KeyType ktype, KnownSymbol desc) => KeywordInfo (Key ktype desc) where
  keytype = typeName @ktype
  description = pack $ symbolVal @desc Proxy
  constant = Nothing
  comment = typeComment @ktype


instance (KnownValue kvalue, KnownSymbol desc) => KeywordInfo (Key (Constant kvalue) desc) where
  keytype = "Constant"
  constant = Just (keyValue @(Constant kvalue) Constant)
  description = pack $ symbolVal @desc Proxy


class KeyType a where
  typeName :: Text
  default typeName :: (Generic a, GTypeName (Rep a)) => Text
  typeName = pack $ gtypeName (from (undefined :: a))


  typeComment :: Text
  default typeComment :: Text
  typeComment = "[" <> T.toLower (typeName @a) <> "]"


instance KeyType String where
  typeName = "String"
  typeComment = ""
instance KeyType MB
instance KeyType Seconds where
  typeComment = "[s]"
instance KeyType Degrees where
  typeComment = "[deg]"
instance KeyType Int where
  typeName = "Int"
  typeComment = ""


-- | Generic NodeName
class GTypeName f where
  gtypeName :: f p -> String


instance (Datatype d) => GTypeName (D1 d f) where
  gtypeName = datatypeName


class KnownValue a where
  knownValue :: String


instance KnownValue Dimensionless where
  knownValue = show Dimensionless
instance KnownValue Kelvin where
  knownValue = show Kelvin
instance KnownValue Temperature where
  knownValue = fromUCD Temperature
instance KnownValue OpticalDepth where
  knownValue = fromUCD OpticalDepth
instance KnownValue True where
  knownValue = "T"
instance (KnownSymbol s) => KnownValue s where
  knownValue = symbolVal @s Proxy


instance KnownValue N_m2 where
  knownValue = "N/m^2"
instance KnownValue Km_s where
  knownValue = "km/s"

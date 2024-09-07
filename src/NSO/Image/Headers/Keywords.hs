{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Image.Headers.Keywords where

import Data.Text (pack)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits
import Telescope.Fits.Header (KeywordRecord (..), LogicalConstant (..), Value (..))
import Text.Casing (fromHumps, toSnake)

-- import NSO.Fits.Generate.Types
import NSO.Prelude
import NSO.Types.Common (Id (..))


-- | Generate a 'KeywordRecord' from a type that supports it
keywordRecord :: forall a. (KeywordInfo a) => a -> KeywordRecord
keywordRecord a = KeywordRecord (T.toUpper $ keyword @a) (keyValue a) comt
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


-- instance (KeywordInfo a) => HeaderKeywords a where
--   headerKeywords a = [keywordRecord a]

-- instance (KeywordInfo a) => HeaderKeywords (Maybe a) where
--   headerKeywords (Just a) = [keywordRecord a]
--   headerKeywords Nothing = []

-- headerComments :: a -> [(Text, Text)]
-- default headerComments :: a -> [(Text, Text)]
-- headerComments _ = []

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
    fmap (setKeyword selectorKeyword) $ genHeaderKeywords a
   where
    setKeyword s d = d{_keyword = newKeyword s d._keyword}
    newKeyword s "" = s
    newKeyword _ k = k
    selectorKeyword = cleanKeyword $ selName (undefined :: M1 S s f x)


instance (GenHeaderKeywords a, GenHeaderKeywords b) => GenHeaderKeywords (a :*: b) where
  genHeaderKeywords (a :*: b) = genHeaderKeywords a ++ genHeaderKeywords b


-- instance {-# OVERLAPPABLE #-} (KeywordInfo a) => GenHeaderKeywords (K1 r a) where
--   genHeaderKeywords (K1 a) = [keywordRecord a]

instance (HeaderKeywords a) => GenHeaderKeywords (K1 r a) where
  genHeaderKeywords (K1 a) = headerKeywords a


instance {-# OVERLAPPING #-} (HeaderKeywords a) => GenHeaderKeywords (K1 r (Maybe a)) where
  genHeaderKeywords (K1 Nothing) = []
  genHeaderKeywords (K1 (Just a)) = headerKeywords a


cleanKeyword :: String -> Text
cleanKeyword = T.toUpper . pack . toSnake . fromHumps


-- class KeyValue a where
--   keyValue :: a -> Value

-- instance KeyValue String where
--   keyValue s = String (pack s)
--
--
-- instance KeyValue (Id a) where
--   keyValue (Id a) = String a

class KeywordInfo a where
  keyword :: Text
  -- default keyword :: (Generic a, GTypeName (Rep a)) => Text
  -- keyword = cleanKeyword $ gtypeName (from (undefined :: a))
  default keyword :: Text
  -- by default leave the keyword unset, allowing the selector to set it
  -- but if the type specifies one, favor that. See `newKeyword` above
  keyword = ""


  keytype :: Text
  default keytype :: (Generic a, GTypeName (Rep a)) => Text
  keytype = pack $ gtypeName (from (undefined :: a))


  description :: Text
  default description :: Text
  description = ""


  comment :: Text
  default comment :: Text
  comment = ""


  allowed :: [Value]
  default allowed :: [Value]
  allowed = []


  keyValue :: a -> Value
  default keyValue :: (KeyType a) => a -> Value
  keyValue = typeValue


-- optValue :: a -> Maybe Value
-- default optValue :: a -> Maybe Value
-- optValue = pure . keyValue

-- instance (KeywordInfo a) => KeywordInfo (Maybe a) where
--   keyword = keyword @a
--   keytype = keytype @a
--   description = description @a
--   comment = comment @a
--   allowed = allowed @a
--   optValue Nothing = _
--   keyValue (Just a) = _

class KeyType a where
  typeName :: Text
  default typeName :: (Generic a, GTypeName (Rep a)) => Text
  typeName = pack $ gtypeName (from (undefined :: a))


  typeComment :: Text
  default typeComment :: Text
  typeComment = "[" <> T.toLower (typeName @a) <> "]"


  typeValue :: a -> Value


instance KeyType Text where
  typeName = "String"
  typeComment = ""
  typeValue = String
instance KeyType Int where
  typeName = "Int"
  typeComment = ""
  typeValue = Integer
instance KeyType Bool where
  typeName = "Bool"
  typeComment = ""
  typeValue True = Logic T
  typeValue False = Logic F
instance KeyType Float where
  typeName = "Float"
  typeComment = ""
  typeValue = Float
instance KeyType (Id a) where
  typeName = "Identifier"
  typeComment = ""
  typeValue (Id a) = String a


-- | Generic NodeName
class GTypeName f where
  gtypeName :: f p -> String


instance (Datatype d) => GTypeName (D1 d f) where
  gtypeName = datatypeName


class KnownValue a where
  knownValue :: Value
  default knownValue :: Value
  knownValue = String (knownValueText @a)


  knownValueText :: Text
  default knownValueText :: Text
  knownValueText = ""


instance KnownValue True where
  knownValue = Logic T
instance (KnownNat n) => KnownValue (n :: Nat) where
  knownValue = Integer $ fromIntegral $ natVal @n Proxy
instance (KnownSymbol s) => KnownValue s where
  knownValue = String $ pack $ symbolVal @s Proxy

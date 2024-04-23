{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Fits.Generate.Keywords where

import Data.Fits (KeywordRecord (..), Value (..))
import Data.Text (pack)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits
import Text.Casing (fromHumps, toKebab)

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


  headerComments :: a -> [(Text, Text)]
  default headerComments :: a -> [(Text, Text)]
  headerComments _ = []


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
    setKeyword s d = d{_keyword = s}
    selectorKeyword = cleanKeyword $ selName (undefined :: M1 S s f x)


instance (GenHeaderKeywords a, GenHeaderKeywords b) => GenHeaderKeywords (a :*: b) where
  genHeaderKeywords (a :*: b) = genHeaderKeywords a ++ genHeaderKeywords b


instance (KeywordInfo a) => GenHeaderKeywords (K1 i a) where
  genHeaderKeywords (K1 a) = [keywordRecord a]


cleanKeyword :: String -> Text
cleanKeyword = T.toUpper . pack . toKebab . fromHumps


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
  default keyword :: (Generic a, GTypeName (Rep a)) => Text
  keyword = cleanKeyword $ gtypeName (from (undefined :: a))


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


  keyValue :: a -> Value
  default keyValue :: (KeyType a) => a -> Value
  keyValue = typeValue


class KeyType a where
  typeName :: Text
  default typeName :: (Generic a, GTypeName (Rep a)) => Text
  typeName = pack $ gtypeName (from (undefined :: a))


  typeComment :: Text
  default typeComment :: Text
  typeComment = "[" <> T.toLower (typeName @a) <> "]"


  typeValue :: a -> Value


instance KeyType String where
  typeName = "String"
  typeComment = ""
  typeValue s = String (pack s)
instance KeyType Int where
  typeName = "Int"
  typeComment = ""
  typeValue = Integer
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
  knownValue :: String


instance KnownValue True where
  knownValue = "T"
instance (KnownSymbol s) => KnownValue s where
  knownValue = symbolVal @s Proxy

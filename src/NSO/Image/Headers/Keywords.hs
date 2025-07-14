{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Image.Headers.Keywords where

import Data.Text (pack)
import Data.Text qualified as T
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Generics
import GHC.TypeLits
import NSO.Prelude
import NSO.Types.Common (Id (..))
import Telescope.Data.KnownText
import Telescope.Fits.Header (KeywordRecord (..), LogicalConstant (..), ToKeyword (..), Value (..))


-- | Generate a 'KeywordRecord' from a type that knows its own keyword
keywordRecord :: forall a. (KeywordInfo a, IsKeyword a, ToKeyword a) => a -> KeywordRecord
keywordRecord a = KeywordRecord (T.toUpper $ keyword @a) (toKeywordValue a) comt
 where
  comt =
    case comment @a of
      "" -> Nothing
      s -> Just s


class IsKeyword a where
  keyword :: Text


class KeywordInfo a where
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


-- | Allows `Key` to offload work to sub-types
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
  typeValue = Float . realToFrac
instance KeyType Double where
  typeName = "Double"
  typeComment = ""
  typeValue = Float
instance KeyType UTCTime where
  typeName = "UTCTime"
  typeComment = ""
  typeValue utc = String $ cs $ iso8601Show utc
instance KeyType LocalTime where
  typeName = "LocalTime"
  typeComment = ""
  typeValue utc = String $ cs $ iso8601Show utc


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
  default knownValue :: (KnownText a) => Value
  knownValue = String (knownText @a)


instance KnownValue True where
  knownValue = Logic T
instance KnownValue False where
  knownValue = Logic F
instance (KnownNat n) => KnownValue (n :: Nat) where
  knownValue = Integer $ fromIntegral $ natVal @n Proxy
instance (KnownSymbol s) => KnownValue s

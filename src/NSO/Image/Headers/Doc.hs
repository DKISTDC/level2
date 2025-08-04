{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Image.Headers.Doc where

import Data.Text (pack)
import GHC.Generics
import NSO.Image.Headers.Keywords
import NSO.Prelude
import Telescope.Fits (Value (..))
import Web.Hyperbole


docKey :: forall a. (KeywordInfo a, IsKeyword a) => DocKey
docKey = docKey' @a (keyword @a)


docKey' :: forall a. (KeywordInfo a) => Text -> DocKey
docKey' key = DocKey key (keytype @a) (allowedValues $ allowed @a) (description @a)
 where
  allowedValues [] = Nothing
  allowedValues as = Just $ fmap (pack . val) as

  val (String s) = show s
  val (Float f) = show f
  val (Integer i) = show i
  val (Logic l) = show l


data DocKey = DocKey
  { keyword :: Text
  , keytype :: Text
  , -- , constant :: Maybe Text
    allowed :: Maybe [Text]
  , description :: Text
  }
  deriving (Show)


--------------------------------------------------------------------

class HeaderDoc a where
  headerDoc :: [DocKey]
  default headerDoc :: (GenHeaderDoc (Rep a)) => [DocKey]
  headerDoc = genHeaderDoc @(Rep a)


-- doesn't document the instance, but the type
class DocView a where
  docView :: Proxy a -> View c ()


class GenHeaderDoc rep where
  genHeaderDoc :: [DocKey]


-- datatype metadata
instance (GenHeaderDoc f) => GenHeaderDoc (M1 D c f) where
  genHeaderDoc = genHeaderDoc @f


-- constructor metadata
instance (GenHeaderDoc f) => GenHeaderDoc (M1 C c f) where
  genHeaderDoc = genHeaderDoc @f


instance (GenHeaderDoc a, GenHeaderDoc b) => GenHeaderDoc (a :*: b) where
  genHeaderDoc = genHeaderDoc @a <> genHeaderDoc @b


-- Selectors
instance (KeywordInfo field, Selector s) => GenHeaderDoc (M1 S s (K1 R field)) where
  genHeaderDoc =
    let s = selName (undefined :: M1 S s f x)
     in [docKey' @field (pack s)]

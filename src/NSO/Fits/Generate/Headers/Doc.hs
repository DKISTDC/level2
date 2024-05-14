{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Fits.Generate.Headers.Doc where

import Data.Text (pack)
import GHC.Generics
import NSO.Fits.Generate.Headers.Keywords
import NSO.Prelude
import Telescope.Fits.Types (Value (..))
import Web.View


docKey :: forall a. (KeywordInfo a) => DocKey
docKey = DocKey (keyword @a) (keytype @a) (allowedValues $ allowed @a) (description @a)
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


-- Selectors
instance (GenHeaderDoc f, Selector s) => GenHeaderDoc (M1 S s f) where
  genHeaderDoc =
    let s = selName (undefined :: M1 S s f x)
     in fmap (setKeyword s) $ genHeaderDoc @f
   where
    setKeyword s d = d{keyword = pack s}


instance (GenHeaderDoc a, GenHeaderDoc b) => GenHeaderDoc (a :*: b) where
  genHeaderDoc = genHeaderDoc @a ++ genHeaderDoc @b


instance (KeywordInfo field) => GenHeaderDoc (K1 i field) where
  genHeaderDoc = [docKey @field]

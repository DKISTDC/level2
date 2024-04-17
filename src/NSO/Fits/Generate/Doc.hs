{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Fits.Generate.Doc where

import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Key
import NSO.Prelude
import Telescope.Fits.Types (BitPix)
import Web.View


docKey :: forall a. (KeyDoc a) => DocKey
docKey = DocKey (keyword @a) (keytype @a) (constant @a) (description @a)


data DocKey = DocKey
  { keyword :: String
  , keytype :: String
  , constant :: Maybe String
  , description :: String
  }
  deriving (Show)


class KeyDoc a where
  keyword :: String
  default keyword :: (Generic a, GTypeName (Rep a)) => String
  keyword = gtypeName (from (undefined :: a))


  keytype :: String
  default keytype :: (Generic a, GTypeName (Rep a)) => String
  keytype = gtypeName (from (undefined :: a))


  description :: String
  default description :: String
  description = ""


  -- Comments don't belong in "KeyDoc", but maybe we should change the name of the class?
  -- comment :: Maybe String
  -- default comment :: Maybe String
  -- comment = Nothing

  constant :: Maybe String
  default constant :: Maybe String
  constant = Nothing


instance (KnownSymbol s) => KeyDoc (ExtName s) where
  -- the description is exactly the ExtName
  description = "Name of the HDU"
  constant = Just (symbolVal @s Proxy)


-- comment = Just "Name of the HDU"

instance (KnownValue unit) => KeyDoc (BType unit) where
  -- TODO: put the actual UCD value in here
  keytype = "Uniform Content Descriptor"
  constant = Just (knownValue @unit)
  description = "The type of the values in the data array"


instance KeyDoc (BUnit unit) where
  keytype = "!! UNIT !!"
  description = "asdf"


instance {-# OVERLAPPABLE #-} (KnownSymbol kword, TypeName ktype, KnownSymbol desc) => KeyDoc (Key kword ktype desc) where
  keytype = typeName @ktype
  keyword = symbolVal @kword Proxy
  description = symbolVal @desc Proxy
  constant = Nothing


instance (KnownSymbol kword, KnownValue kvalue, KnownSymbol desc) => KeyDoc (Key kword (Constant kvalue) desc) where
  keytype = "Constant"
  constant = Just (knownValue @kvalue)
  keyword = symbolVal @kword Proxy
  description = symbolVal @desc Proxy


-- instance KeyDoc BUnit where
--   description = "Unit of the image values"

class TypeName a where
  typeName :: String
  default typeName :: (Generic a, GTypeName (Rep a)) => String
  typeName = gtypeName (from (undefined :: a))


instance TypeName String where
  typeName = "String"
instance TypeName MB
instance TypeName Seconds
instance TypeName Degrees


-- | Generic NodeName
class GTypeName f where
  gtypeName :: f p -> String


instance (Datatype d) => GTypeName (D1 d f) where
  gtypeName = datatypeName


class KnownValue a where
  knownValue :: String


instance KnownValue Dimensionless where
  knownValue = "Dimensionless"
instance KnownValue Kelvin where
  knownValue = "K"
instance KnownValue True where
  knownValue = "T"
instance (KnownSymbol s) => KnownValue s where
  knownValue = symbolVal @s Proxy


instance KnownValue N_m2 where
  knownValue = "N/m^2"
instance KnownValue Km_s where
  knownValue = "km/s"


-- instance (Datatype d) => TypeName (M1 D d f) where
--   typeName = datatypeName (undefined :: d)

-- class GenKeyDoc f where
--   getKeyDoc :: DocKey
--
--
-- -- datatype metadata
-- instance (GenKeyDoc f) => GenKeyDoc (M1 D c f) where
--   genHeaderDoc = genHeaderDoc @f
--
--
-- -- constructor metadata
-- instance (GenKeyDoc f) => GenKeyDoc (M1 C c f) where
--   genHeaderDoc = genHeaderDoc @f
--
--
-- -- Selectors
-- instance (GenKeyDoc f, Selector s) => GenKeyDoc (M1 S s f) where
--   genHeaderDoc =
--     let s = selName (undefined :: M1 S s f x)
--      in fmap (setKeyword s) $ genHeaderDoc @f
--    where
--     setKeyword s d = d{keyword = s}
--
--
-- instance (GenKeyDoc a, GenKeyDoc b) => GenKeyDoc (a :*: b) where
--   genHeaderDoc = genHeaderDoc @a ++ genHeaderDoc @b
--
--
-- instance (KeyTypeName ktype, KnownSymbol desc) => GenKeyDoc (K1 i (Doc ktype desc)) where
--   genHeaderDoc = [DocKey "TODO SELECTOR" (keyTypeName @ktype Proxy) (symbolVal @desc Proxy)]

--------------------------------------------------------------------

class HeaderDoc a where
  headerDoc :: [DocKey]
  default headerDoc :: (GenHeaderDoc (Rep a)) => [DocKey]
  headerDoc = genHeaderDoc @(Rep a)


instance HeaderDoc (KeyList '[]) where
  headerDoc = []


instance (KeyDoc a, HeaderDoc (KeyList as)) => HeaderDoc (KeyList (a : as)) where
  headerDoc = docKey @a : headerDoc @(KeyList as)


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
    setKeyword s d = d{keyword = s}


instance (GenHeaderDoc a, GenHeaderDoc b) => GenHeaderDoc (a :*: b) where
  genHeaderDoc = genHeaderDoc @a ++ genHeaderDoc @b


instance (KeyDoc field) => GenHeaderDoc (K1 i field) where
  genHeaderDoc = [docKey @field]

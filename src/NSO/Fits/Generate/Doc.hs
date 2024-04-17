{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Fits.Generate.Doc where

import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Key
import NSO.Prelude
import Telescope.Fits.Types (BitPix)
import Web.View


docKey :: forall a. (KeywordInfo a) => DocKey
docKey = DocKey (keyword @a) (keytype @a) (constant @a) (description @a)




data DocKey = DocKey
  { keyword :: String
  , keytype :: String
  , constant :: Maybe String
  , description :: String
  }
  deriving (Show)


class KeywordInfo a where
  keyword :: String
  default keyword :: (Generic a, GTypeName (Rep a)) => String
  keyword = gtypeName (from (undefined :: a))


  keytype :: String
  default keytype :: (Generic a, GTypeName (Rep a)) => String
  keytype = gtypeName (from (undefined :: a))


  description :: String
  default description :: String
  description = ""


  comment :: Maybe String
  default comment :: Maybe String
  comment = Nothing


  constant :: Maybe String
  default constant :: Maybe String
  constant = Nothing


instance (KnownSymbol s) => KeywordInfo (ExtName s) where
  -- the description is exactly the ExtName
  description = "Name of the HDU"
  constant = Just (symbolVal @s Proxy)


instance (KnownValue ucd) => KeywordInfo (BType ucd) where
  keytype = "Uniform Content Descriptor"
  constant = Just (knownValue @ucd)
  description = "The type of the values in the data array"


instance (KnownValue unit) => KeywordInfo (BUnit unit) where
  keytype = "Unit"
  constant = Just (knownValue @unit)
  description = "The unit of the values in the data array"


instance {-# OVERLAPPABLE #-} (TypeName ktype, KnownSymbol desc) => KeywordInfo (Key ktype desc) where
  keytype = typeName @ktype
  description = symbolVal @desc Proxy
  constant = Nothing


instance (KnownValue kvalue, KnownSymbol desc) => KeywordInfo (Key (Constant kvalue) desc) where
  keytype = "Constant"
  constant = Just (knownValue @kvalue)
  description = symbolVal @desc Proxy


class TypeName a where
  typeName :: String
  default typeName :: (Generic a, GTypeName (Rep a)) => String
  typeName = gtypeName (from (undefined :: a))


instance TypeName String where
  typeName = "String"
instance TypeName MB
instance TypeName Seconds
instance TypeName Degrees
instance TypeName Int where
  typeName = "Int"


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
    setKeyword s d = d{keyword = s}


instance (GenHeaderDoc a, GenHeaderDoc b) => GenHeaderDoc (a :*: b) where
  genHeaderDoc = genHeaderDoc @a ++ genHeaderDoc @b


instance (KeywordInfo field) => GenHeaderDoc (K1 i field) where
  genHeaderDoc = [docKey @field]

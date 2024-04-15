{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.Doc where

import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Key
import NSO.Prelude
import Web.View


data DocKey = DocKey
  { keyword :: String
  , keytype :: String
  , description :: String
  }
  deriving (Show)


-- doesn't document the instance, but the type
class DocView a where
  docView :: Proxy a -> View c ()


class GenHeaderKeys rep where
  genHeaderKeys :: [DocKey]


-- datatype metadata
instance (GenHeaderKeys f) => GenHeaderKeys (M1 D c f) where
  genHeaderKeys = genHeaderKeys @f


-- constructor metadata
instance (GenHeaderKeys f) => GenHeaderKeys (M1 C c f) where
  genHeaderKeys = genHeaderKeys @f


-- Selectors
instance (GenHeaderKeys f, Selector s) => GenHeaderKeys (M1 S s f) where
  genHeaderKeys =
    let s = selName (undefined :: M1 S s f x)
     in fmap (setKeyword s) $ genHeaderKeys @f
   where
    setKeyword s d = d{keyword = s}


instance (GenHeaderKeys a, GenHeaderKeys b) => GenHeaderKeys (a :*: b) where
  genHeaderKeys = genHeaderKeys @a ++ genHeaderKeys @b


instance (KeyTypeName ktype, KnownSymbol desc) => GenHeaderKeys (K1 i (Key ktype desc)) where
  genHeaderKeys = [DocKey "TODO SELECTOR" (keyTypeName @ktype Proxy) (symbolVal @desc Proxy)]


-- docKey :: (KnownSymbol desc) => Key typ desc -> DocKey
-- docKey = _

class KeyTypeName a where
  keyTypeName :: Proxy a -> String
instance KeyTypeName String where
  keyTypeName _ = "String"
instance KeyTypeName Seconds where
  keyTypeName _ = "Seconds"

-- -- Constructor names / lines
-- instance (Constructor c, GenRoute f) => GenRoute (M1 C c f) where
--   genRoute (n : ps) = do
--     -- take the first path off the list
--     -- check that it matches the constructor name
--     -- check that the rest matches
--     let name = conName (undefined :: M1 C c f x)
--     guard (n == toLower (pack name))
--     M1 <$> genRoute ps
--   genRoute [] = Nothing
--
--
--   genFirst = M1 genFirst
--
--
--   genPaths (M1 x) =
--     let name = conName (undefined :: M1 C c f x)
--      in toLower (pack name) : genPaths x
--
--
-- -- Unary constructors
-- instance GenRoute U1 where
--   genRoute [] = pure U1
--   genRoute _ = Nothing
--   genPaths _ = []
--   genFirst = U1
--
--
-- -- Sum types
-- instance (GenRoute a, GenRoute b) => GenRoute (a :+: b) where
--   genRoute ps = L1 <$> genRoute ps <|> R1 <$> genRoute ps
--   genFirst = L1 genFirst
--   genPaths (L1 a) = genPaths a
--   genPaths (R1 a) = genPaths a
--
--
-- -- Product types
-- instance (GenRoute a, GenRoute b) => GenRoute (a :*: b) where
--   genRoute (p : ps) = do
--     ga <- genRoute [p]
--     gr <- genRoute ps
--     pure $ ga :*: gr
--   genRoute _ = Nothing
--
--
--   genFirst = genFirst :*: genFirst
--
--
--   genPaths (a :*: b) = genPaths a <> genPaths b
--
--
-- instance (Route sub) => GenRoute (K1 R sub) where
--   genRoute ts = K1 <$> matchRoute ts
--   genFirst = K1 defRoute
--   genPaths (K1 sub) = routePath sub
--
--
-- genRouteRead :: (Read x) => [Text] -> Maybe (K1 R x a)
-- genRouteRead [t] = do
--   K1 <$> readMaybe (unpack t)
-- genRouteRead _ = Nothing
--
--
--
--

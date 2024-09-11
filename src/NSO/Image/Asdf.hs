module NSO.Image.Asdf where

import GHC.Generics (from)
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Frame (L2Frame)
import NSO.Image.Headers.Keywords (HeaderKeywords (..))
import NSO.Image.Primary
import NSO.Image.Profile
import NSO.Image.Quantities
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset)
import NSO.Types.Inversion (Inversion)
import NSO.Types.Wavelength (Nm, Wavelength)
import Telescope.Asdf
import Telescope.Asdf.Class (GToObject (..))
import Telescope.Asdf.Core (Unit (..))
import Telescope.Data.Axes (Axes, Row)


-- Inversion ---------------------------------------

data InversionTree = InversionTree
  { created :: UTCTime
  , fileuris :: Fileuris
  , meta :: InversionTreeMeta
  , quantities :: QuantitiesTree
  , profiles :: ProfilesTree
  }
  deriving (Generic, ToAsdf)


data InversionTreeMeta = InversionTreeMeta
  { headers :: HeaderTable PrimaryHeader
  , inventory :: InversionInventory
  }
  deriving (Generic, ToAsdf)


data InversionInventory = InversionInventory
  { inversionId :: Id Inversion
  , datasetIds :: [Id Dataset]
  , frameCount :: Int
  }
  deriving (Generic, ToAsdf)


newtype Fileuris = Fileuris [Path' Filename L2Frame]


instance ToAsdf Fileuris where
  toValue (Fileuris ps) =
    Array $ fmap pathNode ps
   where
    pathNode (Path fp) = fromValue $ String $ cs fp


-- Quantities ------------------------------------------------

data QuantitiesTree = QuantitiesTree
  { wcs :: WCSTodo
  , quantities :: Quantities QuantityTree
  }
instance ToAsdf QuantitiesTree where
  toValue (QuantitiesTree wcs qs) =
    Object $
      ("wcs", toNode wcs)
        : gToObject (from qs)


data QuantityTree info = QuantityTree
  { unit :: Unit
  , shape :: Axes Row
  , hdu :: Int
  , meta :: QuantityTreeMeta info
  }
  deriving (Generic)
instance (HeaderKeywords info) => ToAsdf (QuantityTree info) where
  schema = "TODO SCHEMA"


data QuantityTreeMeta info = QuantityTreeMeta
  { headers :: HeaderTable (QuantityHeader info)
  }
  deriving (Generic, ToAsdf)


data WCSTodo = WCSTodo
instance ToAsdf WCSTodo where
  toValue _ = String "TODO"


-- Profiles ------------------------------------------------

data ProfilesTree = ProfilesTree
  { wcs :: WCSTodo
  , original630 :: ProfileTree Orig630
  , original854 :: ProfileTree Orig854
  , fit630 :: ProfileTree Fit630
  , fit854 :: ProfileTree Fit854
  }
  deriving (Generic, ToAsdf)


data ProfileTree info = ProfileTree
  { profile :: Text
  , wavelength :: Wavelength Nm
  , unit :: Unit
  , shape :: Axes Row
  , hdu :: Int
  , meta :: ProfileTreeMeta info
  }
  deriving (Generic)
instance (HeaderKeywords info) => ToAsdf (ProfileTree info)


data ProfileTreeMeta info = ProfileTreeMeta
  { headers :: HeaderTable (ProfileHeader info)
  }
  deriving (Generic, ToAsdf)

module NSO.Image.Asdf where

import Data.List.NonEmpty qualified as NE
import GHC.Generics (from)
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Frame
import NSO.Image.Headers.Keywords (HeaderKeywords (..), KnownText (..))
import NSO.Image.Primary
import NSO.Image.Profile
import NSO.Image.Quantities hiding (quantities)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset)
import NSO.Types.Inversion (Inversion)
import NSO.Types.Wavelength (Nm, Wavelength)
import Telescope.Asdf
import Telescope.Asdf.Class (GToObject (..))
import Telescope.Asdf.Core (Unit (..))
import Telescope.Data.Axes (Axes, Row)


inversionTree :: Id Inversion -> [Id Dataset] -> UTCTime -> NonEmpty L2FrameMeta -> InversionTree
inversionTree inversionId datasetIds now metas =
  InversionTree
    { created = now
    , fileuris = Fileuris $ fmap (.path) $ NE.toList metas
    , meta = inversionTreeMeta inversionId datasetIds $ fmap (.primary) metas
    , quantities = quantitiesTree $ fmap (.quantities) metas
    , profiles = profilesTree $ fmap (.profiles) metas
    }


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


inversionTreeMeta :: Id Inversion -> [Id Dataset] -> NonEmpty PrimaryHeader -> InversionTreeMeta
inversionTreeMeta inversionId datasetIds headers =
  InversionTreeMeta
    { headers = HeaderTable headers
    , inventory = inversionInventory
    }
 where
  inversionInventory =
    InversionInventory
      { frameCount = length headers
      , inversionId
      , datasetIds
      }


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


quantitiesTree :: NonEmpty FrameQuantitiesMeta -> QuantitiesTree
quantitiesTree metas =
  QuantitiesTree
    { wcs = WCSTodo
    , quantities =
        Quantities
          { opticalDepth = quantity (.opticalDepth)
          , temperature = quantity (.temperature)
          , electronPressure = quantity (.electronPressure)
          , microTurbulence = quantity (.microTurbulence)
          , magStrength = quantity (.magStrength)
          , velocity = quantity (.velocity)
          , magInclination = quantity (.magInclination)
          , magAzimuth = quantity (.magAzimuth)
          , geoHeight = quantity (.geoHeight)
          , gasPressure = quantity (.gasPressure)
          , density = quantity (.density)
          }
    }
 where
  quantity
    :: forall info ext btype unit
     . (info ~ DataHDUInfo ext btype unit, KnownText unit, HDUOrder info)
    => (Quantities QuantityHeader -> QuantityHeader info)
    -> QuantityTree info
  quantity f = quantityTree (head metas).shape $ fmap f quantities

  quantities = fmap (.quantities) metas


data QuantityTree info = QuantityTree
  { unit :: Unit
  , shape :: Axes Row
  , hdu :: HDUIndex
  , meta :: QuantityTreeMeta info
  }
  deriving (Generic)
instance (HeaderKeywords info) => ToAsdf (QuantityTree info) where
  schema = "TODO SCHEMA"


quantityTree
  :: forall info ext btype unit
   . (KnownText unit, HDUOrder info, info ~ DataHDUInfo ext btype unit)
  => Axes Row
  -> NonEmpty (QuantityHeader info)
  -> QuantityTree info
quantityTree shape heads =
  QuantityTree
    { unit = Unit (knownText @unit)
    , shape
    , hdu = hduIndex @(DataHDUInfo ext btype unit)
    , meta = QuantityTreeMeta{headers = HeaderTable heads}
    }


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


profilesTree :: NonEmpty FrameProfilesMeta -> ProfilesTree
profilesTree metas =
  let ps = fmap (.profiles) metas
      shape = (head metas).shape
   in ProfilesTree
        { wcs = WCSTodo
        , original630 = profileTree "original" shape $ fmap (.orig630) ps
        , original854 = profileTree "original" shape $ fmap (.orig854) ps
        , fit630 = profileTree "fit" shape $ fmap (.fit630) ps
        , fit854 = profileTree "fit" shape $ fmap (.fit854) ps
        }


data ProfileTree info = ProfileTree
  { profile :: Text
  , wavelength :: Wavelength Nm
  , unit :: Unit
  , shape :: Axes Row
  , hdu :: HDUIndex
  , meta :: ProfileTreeMeta info
  }
  deriving (Generic)
instance (HeaderKeywords info) => ToAsdf (ProfileTree info)


profileTree :: forall info. (ProfileInfo info, HDUOrder info) => Text -> Axes Row -> NonEmpty (ProfileHeader info) -> ProfileTree info
profileTree profile shape heads =
  ProfileTree
    { profile
    , wavelength = profileWav @info
    , unit = Count
    , shape
    , hdu = hduIndex @info
    , meta = ProfileTreeMeta{headers = HeaderTable heads}
    }


data ProfileTreeMeta info = ProfileTreeMeta
  { headers :: HeaderTable (ProfileHeader info)
  }
  deriving (Generic, ToAsdf)

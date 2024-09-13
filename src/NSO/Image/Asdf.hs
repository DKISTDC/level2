module NSO.Image.Asdf where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Frame
import NSO.Image.Headers.Keywords (HeaderKeywords (..), KnownText (..))
import NSO.Image.Primary
import NSO.Image.Profile
import NSO.Image.Quantities hiding (quantities)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset)
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import NSO.Types.Wavelength (Nm, Wavelength (..))
import Telescope.Asdf as Asdf
import Telescope.Asdf.Core (Unit (..))
import Telescope.Data.Axes (Axes, Row)


data L2Asdf


asdfDocument :: Id Inversion -> [Id Dataset] -> UTCTime -> NonEmpty L2FrameMeta -> Document
asdfDocument inversionId datasetIds now metas =
  Document
    { wavelengths = wavs
    , inversion = inversionTree
    }
 where
  wavs = [profileWav @Orig630, profileWav @Orig854]

  -- they need to be sorted!
  frames = NE.sort metas

  inversionTree :: InversionTree
  inversionTree =
    InversionTree
      { fileuris = Fileuris $ fmap (.path) $ NE.toList frames
      , meta = inversionTreeMeta $ fmap (.primary) frames
      , quantities = quantitiesSection $ fmap (.quantities) frames
      , profiles = profilesSection $ fmap (.profiles) frames
      }

  inversionTreeMeta :: NonEmpty PrimaryHeader -> InversionTreeMeta
  inversionTreeMeta headers =
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
        , wavelengths = wavs
        , created = now
        }


filenameL2Asdf :: Id Proposal -> Id Inversion -> Path' Filename L2Asdf
filenameL2Asdf _ _ = Path "test.asdf"


encodeL2 :: (Error AsdfError :> es, IOE :> es) => Document -> Eff es ByteString
encodeL2 = Asdf.encode


-- Path $ cs (T.toUpper $ T.map toUnderscore $ ii.fromId <> "_" <> dt) <> "_L2.fits"
-- where
-- toUnderscore :: Char -> Char
-- toUnderscore '.' = '_'
-- toUnderscore ':' = '_'
-- toUnderscore '-' = '_'
-- toUnderscore c = c

-- Inversion ---------------------------------------

data Document = Document
  { wavelengths :: [Wavelength Nm]
  , inversion :: InversionTree
  }
  deriving (Generic, ToAsdf)


data InversionTree = InversionTree
  { fileuris :: Fileuris
  , meta :: InversionTreeMeta
  , quantities :: HDUSection (Quantities QuantityTree)
  , profiles :: HDUSection (Profiles ProfileTree)
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
  , created :: UTCTime
  , wavelengths :: [Wavelength Nm]
  }
  deriving (Generic, ToAsdf)


newtype Fileuris = Fileuris [Path' Filename L2Frame]


instance ToAsdf Fileuris where
  toValue (Fileuris ps) =
    Array $ fmap pathNode ps
   where
    pathNode (Path fp) = fromValue $ String $ cs fp


newtype AxisLabel = AxisLabel Text
  deriving newtype (ToAsdf, IsString)


data HDUSection hdus = HDUSection
  { axes :: [AxisLabel]
  , shape :: Axes Row
  , hdus :: hdus
  , wcs :: WCSTodo
  }


instance (ToAsdf hdus) => ToAsdf (HDUSection hdus) where
  toValue section =
    mconcat
      -- merge the fields from both
      [ Object [("axes", toNode section.axes), ("shape", toNode section.shape)]
      , toValue section.hdus
      , Object [("wcs", toNode section.wcs)] -- put wcs last
      ]


-- Quantities ------------------------------------------------

quantitiesSection :: NonEmpty FrameQuantitiesMeta -> HDUSection (Quantities QuantityTree)
quantitiesSection frames =
  HDUSection
    { axes = ["frameY", "slitX", "opticalDepth"]
    , shape = (head frames).shape
    , hdus =
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
    , wcs = WCSTodo
    }
 where
  quantity
    :: forall info ext btype unit
     . (info ~ DataHDUInfo ext btype unit, KnownText unit, HDUOrder info)
    => (Quantities QuantityHeader -> QuantityHeader info)
    -> QuantityTree info
  quantity f = quantityTree $ fmap f quantities

  quantities = fmap (.quantities) frames


data QuantityTree info = QuantityTree
  { unit :: Unit
  , hdu :: HDUIndex
  , meta :: QuantityTreeMeta info
  }
  deriving (Generic)
instance (HeaderKeywords info) => ToAsdf (QuantityTree info)
instance ToAsdf (Quantities QuantityTree)


quantityTree
  :: forall info ext btype unit
   . (KnownText unit, HDUOrder info, info ~ DataHDUInfo ext btype unit)
  => NonEmpty (QuantityHeader info)
  -> QuantityTree info
quantityTree heads =
  QuantityTree
    { unit = Unit (knownText @unit)
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

profilesSection :: NonEmpty FrameProfilesMeta -> HDUSection (Profiles ProfileTree)
profilesSection frames =
  let shape = (head frames).shape
   in HDUSection
        { wcs = WCSTodo
        , axes = ["frameY", "slitX", "wavelength", "stokes"]
        , shape = shape
        , hdus = profilesTree frames
        }


profilesTree :: NonEmpty FrameProfilesMeta -> Profiles ProfileTree
profilesTree frames =
  let ps = fmap (.profiles) frames
   in Profiles
        { orig630 = profileTree $ fmap (.orig630) ps
        , orig854 = profileTree $ fmap (.orig854) ps
        , fit630 = profileTree $ fmap (.fit630) ps
        , fit854 = profileTree $ fmap (.fit854) ps
        }


data ProfileTree info = ProfileTree
  { profile :: Text
  , wavelength :: Wavelength Nm
  , unit :: Unit
  , hdu :: HDUIndex
  , meta :: ProfileTreeMeta info
  }
  deriving (Generic)
instance (HeaderKeywords info) => ToAsdf (ProfileTree info)
instance ToAsdf (Profiles ProfileTree) where
  -- split into .original and .fit
  toValue ps =
    Object
      [ ("original", toNode original)
      , ("fit", toNode fit)
      ]
   where
    original =
      Object
        [ ("wav6302", toNode ps.orig630)
        , ("wav8542", toNode ps.orig854)
        ]
    fit =
      Object
        [ ("wav6302", toNode ps.fit630)
        , ("wav8542", toNode ps.fit854)
        ]


-- instance ToAsdf (Profiles ProfileTree)

profileTree :: forall info. (ProfileInfo info, KnownText (ProfileType info), HDUOrder info) => NonEmpty (ProfileHeader info) -> ProfileTree info
profileTree heads =
  ProfileTree
    { profile = T.toLower $ knownText @(ProfileType info)
    , wavelength = profileWav @info
    , unit = Count
    , hdu = hduIndex @info
    , meta = ProfileTreeMeta{headers = HeaderTable heads}
    }


data ProfileTreeMeta info = ProfileTreeMeta
  { headers :: HeaderTable (ProfileHeader info)
  }
  deriving (Generic, ToAsdf)

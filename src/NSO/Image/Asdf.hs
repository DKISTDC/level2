{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Asdf where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Frame
import NSO.Image.Headers.Keywords (KnownText (..))
import NSO.Image.Primary
import NSO.Image.Profile
import NSO.Image.Quantity hiding (quantities)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset)
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import NSO.Types.Wavelength (Nm, Wavelength (..))
import Telescope.Asdf as Asdf
import Telescope.Asdf.Core (Unit (..))
import Telescope.Asdf.NDArray (DataType (..))
import Telescope.Data.Axes (Axes, Row)
import Telescope.Fits (ToHeader (..))


data L2Asdf


asdfDocument :: Id Inversion -> [Id Dataset] -> UTCTime -> NonEmpty L2FrameMeta -> Document
asdfDocument inversionId datasetIds now metas =
  Document inversionTree
 where
  -- they need to be sorted!
  frames = NE.sort metas

  inversionTree :: InversionTree
  inversionTree =
    InversionTree
      { fileuris
      , meta = inversionTreeMeta $ fmap (.primary) frames
      , quantities = quantitiesSection fileuris $ fmap (.quantities) frames
      , profiles = profilesSection fileuris $ fmap (.profiles) frames
      }

  inversionTreeMeta :: NonEmpty PrimaryHeader -> InversionTreeMeta
  inversionTreeMeta headers =
    InversionTreeMeta
      { headers = HeaderTable headers
      , frameCount = length headers
      , inversionId
      , datasetIds
      , wavelengths = [profileWav @Orig630, profileWav @Orig854]
      , created = now
      }

  fileuris = Fileuris $ fmap (.path) $ NE.toList frames


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
  { inversion :: InversionTree
  }
  deriving (Generic, ToAsdf)


data InversionTree = InversionTree
  { fileuris :: Fileuris
  , meta :: InversionTreeMeta
  , quantities :: HDUSection (Quantities (DataTree QuantityMeta))
  , profiles :: HDUSection (Profiles ProfileTree)
  }
  deriving (Generic, ToAsdf)


data InversionTreeMeta = InversionTreeMeta
  { inversionId :: Id Inversion
  , created :: UTCTime
  , wavelengths :: [Wavelength Nm]
  , datasetIds :: [Id Dataset]
  , frameCount :: Int
  , headers :: HeaderTable PrimaryHeader
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

quantitiesSection :: Fileuris -> NonEmpty FrameQuantitiesMeta -> HDUSection (Quantities (DataTree QuantityMeta))
quantitiesSection files frames =
  HDUSection
    { axes = ["frameY", "slitX", "opticalDepth"]
    , shape
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
    -> DataTree QuantityMeta info
  quantity f = quantityTree files shape $ fmap f quantities

  quantities = fmap (.quantities) frames

  shape = (head frames).shape


data DataTree meta info = DataTree
  { unit :: Unit
  , data_ :: FileManager
  , meta :: meta info
  , wcs :: WCSTodo
  }
  deriving (Generic)
instance (ToAsdf (meta info)) => ToAsdf (DataTree meta info) where
  toValue q =
    Object
      [ ("unit", toNode q.unit)
      , ("data", toNode q.data_)
      , ("meta", toNode q.meta)
      , ("wcs", toNode q.wcs)
      ]
instance ToAsdf (Quantities (DataTree QuantityMeta))


quantityTree
  :: forall info ext btype unit
   . (KnownText unit, HDUOrder info, info ~ DataHDUInfo ext btype unit)
  => Fileuris
  -> Axes Row
  -> NonEmpty (QuantityHeader info)
  -> DataTree QuantityMeta info
quantityTree files shape heads =
  DataTree
    { unit = Unit (knownText @unit)
    , wcs = WCSTodo
    , data_ = fileManager @info files shape
    , meta = QuantityMeta{headers = HeaderTable heads}
    }


data QuantityMeta info = QuantityMeta
  { headers :: HeaderTable (QuantityHeader info)
  }
  deriving (Generic, ToAsdf)


data WCSTodo = WCSTodo
instance ToAsdf WCSTodo where
  toValue _ = String "TODO"


-- Profiles ------------------------------------------------

profilesSection :: Fileuris -> NonEmpty FrameProfilesMeta -> HDUSection (Profiles ProfileTree)
profilesSection files frames =
  let shape = (head frames).shape
   in HDUSection
        { wcs = WCSTodo
        , axes = ["frameY", "slitX", "wavelength", "stokes"]
        , shape = shape
        , hdus = profilesTree files shape frames
        }


profilesTree :: Fileuris -> Axes Row -> NonEmpty FrameProfilesMeta -> Profiles ProfileTree
profilesTree files shape frames =
  let ps = fmap (.profiles) frames
   in Profiles
        { orig630 = profileTree files shape $ fmap (.orig630) ps
        , orig854 = profileTree files shape $ fmap (.orig854) ps
        , fit630 = profileTree files shape $ fmap (.fit630) ps
        , fit854 = profileTree files shape $ fmap (.fit854) ps
        }


data ProfileTree info = ProfileTree
  { unit :: Unit
  , data_ :: FileManager
  , meta :: ProfileTreeMeta info
  , wcs :: WCSTodo
  }
  deriving (Generic)
instance (ToHeader info) => ToAsdf (ProfileTree info) where
  toValue p =
    Object
      [ ("unit", toNode p.unit)
      , ("data", toNode p.data_)
      , ("meta", toNode p.meta)
      , ("wcs", toNode p.wcs)
      ]
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

profileTree
  :: forall info
   . (ProfileInfo info, KnownText (ProfileType info), HDUOrder info)
  => Fileuris
  -> Axes Row
  -> NonEmpty (ProfileHeader info)
  -> ProfileTree info
profileTree files shape heads =
  ProfileTree
    { unit = Count
    , wcs = WCSTodo
    , data_ = fileManager @info files shape
    , meta =
        ProfileTreeMeta
          { headers = HeaderTable heads
          , wavelength = profileWav @info
          , profile = T.toLower $ knownText @(ProfileType info)
          }
    }


data ProfileTreeMeta info = ProfileTreeMeta
  { headers :: HeaderTable (ProfileHeader info)
  , wavelength :: Wavelength Nm
  , profile :: Text
  }
  deriving (Generic, ToAsdf)


data FileManager = FileManager
  { datatype :: DataType
  , fileuris :: Fileuris
  , shape :: Axes Row
  , target :: HDUIndex
  }
  deriving (Generic)
instance ToAsdf FileManager where
  schema = "asdf://dkist.nso.edu/tags/file_manager-1.0.0"


fileManager :: forall info. (HDUOrder info) => Fileuris -> Axes Row -> FileManager
fileManager fileuris shape =
  FileManager{datatype = Float64, fileuris, shape, target = hduIndex @info}

{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Asdf where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import NSO.Image.Asdf.GWCS
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Asdf.NDCollection
import NSO.Image.Frame
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
import Telescope.Data.Axes (Axes (..), Major (Row))
import Telescope.Data.KnownText
import Telescope.Fits (ToHeader (..))


-- DONE: move extra keys into meta.inventory
-- DONE: support ND collection
-- TODO: 3-arm profiles sodium
-- TODO: fit/orig separate GWCS + anchors

data L2Asdf


asdfDocument :: Id Inversion -> [Id Dataset] -> UTCTime -> NonEmpty L2FrameMeta -> Document
asdfDocument inversionId datasetIds now metas =
  Document inversionTree
 where
  frames = NE.sort metas

  frame = head frames

  inversionTree :: InversionTree
  inversionTree =
    InversionTree
      { fileuris
      , meta = inversionMeta $ fmap (.primary) frames
      , quantities = quantitiesSection (fmap (.quantities) frames) (qgwcs frame)
      , profiles = profilesSection frame.primary $ fmap (.profiles) frames
      }

  -- choose a single frame from which to calculate the GWCS
  qgwcs :: L2FrameMeta -> QuantityGWCS
  qgwcs m = quantityGWCS m.primary m.quantities.items.opticalDepth.wcs

  inversionMeta :: NonEmpty PrimaryHeader -> InversionMeta
  inversionMeta headers =
    InversionMeta{headers = HeaderTable headers, inventory = inversionInventory headers}

  inversionInventory :: NonEmpty PrimaryHeader -> InversionInventory
  inversionInventory headers =
    InversionInventory
      { frameCount = length headers
      , inversionId
      , datasetIds
      , wavelengths = [profileWav @Orig630, profileWav @Orig854]
      , created = now
      }

  fileuris = Fileuris $ fmap (.path) $ NE.toList frames


filenameL2Asdf :: Id Proposal -> Id Inversion -> Path' Filename L2Asdf
filenameL2Asdf _ ii =
  Path $ cs (T.toUpper $ T.map toUnderscore ii.fromId) <> "_L2.asdf"
 where
  toUnderscore :: Char -> Char
  toUnderscore '.' = '_'
  toUnderscore c = c


encodeL2 :: (Error AsdfError :> es, IOE :> es) => Document -> Eff es ByteString
encodeL2 = Asdf.encode


-- Inversion ---------------------------------------

data Document = Document
  { inversion :: InversionTree
  }
  deriving (Generic, ToAsdf)


data InversionTree = InversionTree
  { fileuris :: Fileuris
  , meta :: InversionMeta
  , quantities :: QuantitiesSection
  , profiles :: ProfilesSection
  }
  deriving (Generic, ToAsdf)


data InversionMeta = InversionMeta
  { inventory :: InversionInventory
  , headers :: HeaderTable PrimaryHeader
  }
  deriving (Generic, ToAsdf)


data InversionInventory = InversionInventory
  { inversionId :: Id Inversion
  , created :: UTCTime
  , wavelengths :: [Wavelength Nm]
  , datasetIds :: [Id Dataset]
  , frameCount :: Int
  }
  deriving (Generic)
instance KnownText InversionInventory where
  knownText = "InversionInventory"
instance ToAsdf InversionInventory where
  anchor _ = Just $ Anchor (knownText @InversionInventory)


newtype Fileuris = Fileuris [Path' Filename L2Frame]


instance ToAsdf Fileuris where
  anchor _ = Just $ Anchor "fileuris"
  toValue (Fileuris ps) =
    Array $ fmap pathNode ps
   where
    pathNode (Path fp) = fromValue $ String $ cs fp


data QuantitiesSection = QuantitiesSection
  { axes :: [AxisMeta]
  , items :: Quantities (DataTree QuantityMeta)
  , gwcs :: QuantityGWCS
  }
instance ToAsdf QuantitiesSection where
  schema _ = "tag:sunpy.org:ndcube/ndcube/ndcollection-1.0.0"
  toValue section =
    mconcat
      [ Object [("axes", toNode section.axes), ("gwcs", toNode section.gwcs)]
      , toValue section.items
      , toValue (NDCollection (quantitiesFrom AlignedAxes) section.axes refs)
      ]
   where
    refs :: Quantities Ref
    refs = quantitiesFrom (const Ref) ()
instance ToAsdf (Quantities Ref)


-- you can't pass it a single set of axes
data ProfilesSection = ProfilesSection
  { axes :: [AxisMeta]
  , items :: Profiles ProfileTree
  , gwcsFit :: ProfileGWCS
  , gwcsOrig :: ProfileGWCS
  }


instance ToAsdf ProfilesSection where
  schema _ = "tag:sunpy.org:ndcube/ndcube/ndcollection-1.0.0"
  toValue section =
    mconcat
      [ Object
          [ ("axes", toNode section.axes)
          , ("gwcs_fit", toNode section.gwcsFit)
          , ("gwcs_orig", toNode section.gwcsOrig)
          ]
      , toValue section.items
      , toValue (NDCollection (profilesFrom AlignedAxes) section.axes refs)
      ]
   where
    refs :: Profiles Ref
    refs = profilesFrom (const Ref) ()
instance ToAsdf (Profiles Ref)


-- Quantities ------------------------------------------------

quantitiesSection :: NonEmpty FrameQuantitiesMeta -> QuantityGWCS -> QuantitiesSection
quantitiesSection frames gwcs =
  QuantitiesSection
    { axes =
        [ AxisMeta "frameY" True
        , AxisMeta "slitX" True
        , AxisMeta "opticalDepth" True
        ]
    , gwcs
    , items =
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
    -> DataTree QuantityMeta info
  quantity f = quantityTree shape $ fmap f items

  items :: NonEmpty (Quantities QuantityHeader)
  items = fmap (.items) frames

  shape = (head frames).shape


data DataTree meta info = DataTree
  { unit :: Unit
  , data_ :: FileManager
  , meta :: meta info
  , wcs :: Ref QuantityGWCS
  }
  deriving (Generic)
instance (ToAsdf (meta info), KnownText info) => ToAsdf (DataTree meta info) where
  schema _ = "asdf://dkist.nso.edu/tags/dataset-1.2.0"
  anchor _ = Just $ Anchor $ knownText @info
  toValue q =
    Object
      [ ("unit", toNode q.unit)
      , ("data", toNode q.data_)
      , ("meta", toNode q.meta)
      , ("wcs", toNode q.wcs)
      ]
instance ToAsdf (Quantities (DataTree QuantityMeta))


-- instance KnownText QuantityGWCS where
--   knownText = "quantityGWCS"
--
-- instance ToAsdf QuantityGWCS where
--   schema (QuantityGWCS gwcs) = schema gwcs
--   anchor _ = Just $ Anchor $ knownText @QuantityGWCS
--   toValue (QuantityGWCS gwcs) = toValue gwcs

quantityTree
  :: forall info ext btype unit
   . (KnownText unit, HDUOrder info, info ~ DataHDUInfo ext btype unit)
  => Shape Quantity
  -> NonEmpty (QuantityHeader info)
  -> DataTree QuantityMeta info
quantityTree shape heads =
  DataTree
    { unit = Pixel
    , wcs = Ref
    , data_ = fileManager @info shape.axes
    , meta = QuantityMeta{headers = HeaderTable heads, inventory = Ref}
    }


data QuantityMeta info = QuantityMeta
  { headers :: HeaderTable (QuantityHeader info)
  , inventory :: Ref InversionInventory
  }
  deriving (Generic, ToAsdf)


-- Profiles ------------------------------------------------

profilesSection :: PrimaryHeader -> NonEmpty FrameProfilesMeta -> ProfilesSection
profilesSection primary frames =
  ProfilesSection
    { gwcsFit = profileGWCS primary (head frames).profiles.orig854.wcs
    , gwcsOrig = profileGWCS primary (head frames).profiles.orig630.wcs
    , axes = [AxisMeta "frameY" True, AxisMeta "slitX" True, AxisMeta "wavelength" False, AxisMeta "stokes" True]
    , items = profilesTree frames
    }


profilesTree :: NonEmpty FrameProfilesMeta -> Profiles ProfileTree
profilesTree frames =
  let frame = head frames
      ps = fmap (.profiles) frames
   in Profiles
        { orig630 = profileTree frame.shape630 $ fmap (.orig630) ps
        , orig854 = profileTree frame.shape854 $ fmap (.orig854) ps
        , fit630 = profileTree frame.shape630 $ fmap (.fit630) ps
        , fit854 = profileTree frame.shape854 $ fmap (.fit854) ps
        }


data ProfileTree info = ProfileTree
  { unit :: Unit
  , data_ :: FileManager
  , meta :: ProfileTreeMeta info
  , wcs :: Ref ProfileGWCS
  }
  deriving (Generic)
instance (ToHeader info, KnownText info) => ToAsdf (ProfileTree info) where
  anchor _ = Just $ Anchor $ knownText @info
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


profileTree
  :: forall info
   . (ProfileInfo info, KnownText (ProfileType info), HDUOrder info)
  => Shape Profile
  -> NonEmpty (ProfileHeader info)
  -> ProfileTree info
profileTree shape heads =
  ProfileTree
    { unit = Count
    , wcs = Ref
    , data_ = fileManager @info shape.axes
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


-- File Manager ----------------------------------------------------

data FileManager = FileManager
  { datatype :: DataType
  , fileuris :: Ref "fileuris"
  , shape :: Axes Row
  , target :: HDUIndex
  }
  deriving (Generic)
instance ToAsdf FileManager where
  schema _ = "asdf://dkist.nso.edu/tags/file_manager-1.0.0"


fileManager :: forall info. (HDUOrder info) => Axes Row -> FileManager
fileManager axes =
  FileManager{datatype = Float64, fileuris = Ref, shape = axes, target = hduIndex @info}


-- Ref --------------------------------------------------------------

data Ref ref = Ref
instance (KnownText ref) => ToAsdf (Ref ref) where
  toValue _ =
    Alias $ Anchor $ knownText @ref
instance (KnownText ref) => KnownText (Ref ref) where
  knownText = knownText @ref

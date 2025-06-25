{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Asdf where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import NSO.Data.Spectra qualified as Spectra
import NSO.Image.Asdf.FileManager (FileManager, fileManager)
import NSO.Image.Asdf.GWCS
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Asdf.NDCollection
import NSO.Image.Asdf.Ref
import NSO.Image.Files qualified as Files
import NSO.Image.Fits
import NSO.Image.Fits.Quantity hiding (quantities)
import NSO.Image.Headers.DataCommon
import NSO.Image.Primary
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset, Dataset' (datasetId))
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import NSO.Types.Wavelength (Nm, Wavelength (..))
import Telescope.Asdf as Asdf
import Telescope.Asdf.Core (Unit (..))
import Telescope.Data.KnownText


-- DONE: move extra keys into meta.inventory
-- DONE: support ND collection
-- DOING: 3-arm profiles sodium
-- TODO: fit/orig separate GWCS + anchors

data L2Asdf


outputL2AsdfPath :: Id Proposal -> Id Inversion -> Path L2Asdf
outputL2AsdfPath ip ii =
  filePath (Files.outputL2Dir ip ii) $ filenameL2Asdf ip ii


asdfDocument :: Id Inversion -> [Dataset] -> UTCTime -> NonEmpty L2FitsMeta -> Document
asdfDocument inversionId dsets now metas =
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
  qgwcs :: L2FitsMeta -> QuantityGWCS
  qgwcs m = quantityGWCS m.primary m.quantities.items.opticalDepth.wcs

  inversionMeta :: NonEmpty PrimaryHeader -> InversionMeta
  inversionMeta headers =
    InversionMeta{headers = HeaderTable headers, inventory = inversionInventory headers}

  inversionInventory :: NonEmpty PrimaryHeader -> InversionInventory
  inversionInventory headers =
    InversionInventory
      { frameCount = length headers
      , inversionId
      , datasetIds = fmap (.datasetId) dsets
      , wavelengths = fmap Spectra.midPoint $ Spectra.identifyLines dsets
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


newtype Fileuris = Fileuris [Path' Filename L2FrameFits]


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


  -- where
  --  refs :: Quantities Ref
  --  refs = quantitiesFrom (const Ref) ()
  toValue section =
    mconcat
      [ Object
          [ ("axes", toNode section.axes)
          , ("meta", toNode $ Object [("gwcs", toNode section.gwcs)])
          ]
      , toValue (NDCollection (quantitiesFrom AlignedAxes) section.axes section.items)
      ]


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
    , data_ = fileManager shape.axes (hduIndex @info)
    , meta = QuantityMeta{headers = HeaderTable heads, inventory = Ref}
    }


data QuantityMeta info = QuantityMeta
  { headers :: HeaderTable (QuantityHeader info)
  , inventory :: Ref InversionInventory
  }
  deriving (Generic, ToAsdf)


-- Profiles ------------------------------------------------

-- you can't pass it a single set of axes
data ProfilesSection = ProfilesSection
  { axes :: [AxisMeta]
  , arms :: Arms (Profile ProfileTree)
  , gwcsFit :: ProfileGWCS Fit
  , gwcsOrig :: ProfileGWCS Original
  }


instance ToAsdf ProfilesSection where
  schema _ = "tag:sunpy.org:ndcube/ndcube/ndcollection-1.0.0"
  toValue section =
    mconcat
      [ Object
          [ ("axes", toNode section.axes)
          , ("meta", toNode meta)
          ]
          -- , toValue (NDCollection (_ AlignedAxes) section.axes section.arms)
          -- , toValue section.arms
      ]
   where
    meta =
      Object
        [ ("gwcs_fit", toNode section.gwcsFit)
        , ("gwcs_orig", toNode section.gwcsOrig)
        ]


-- we need one ProfileMeta for each arm
profilesSection :: PrimaryHeader -> NonEmpty (Arms ProfileMeta) -> ProfilesSection
profilesSection primary frames =
  let sampleArm = head (head frames).arms
      fit = sampleArm.profile.fit
      orig = sampleArm.profile.original
   in ProfilesSection
        { axes = [AxisMeta "frameY" True, AxisMeta "slitX" True, AxisMeta "wavelength" False, AxisMeta "stokes" True]
        , arms = _ -- profilesArmsTree frames
        , gwcsFit = profileGWCS primary fit.wcs
        , gwcsOrig = profileGWCS primary orig.wcs
        }


-- profilesArmsTree :: NonEmpty (Arms ProfileMeta) -> Arms (Profile ProfileTree)
-- profilesArmsTree frames =
--   let frame = head frames
--    in -- ps = fmap (.profiles) frames
--       _ -- Arms []

data ProfileTree fit = ProfileTree
  { unit :: Unit
  , data_ :: FileManager
  , meta :: ProfileTreeMeta fit
  , wcs :: Ref (ProfileGWCS fit)
  }
  deriving (Generic)
instance (KnownText fit) => ToAsdf (ProfileTree fit) where
  -- anchor _ = Just $ Anchor $ knownText @fit
  toValue p =
    Object
      [ ("unit", toNode p.unit)
      , ("data", toNode p.data_)
      , ("meta", toNode p.meta)
      -- , ("wcs", toNode p.wcs)
      ]


-- profileTree
--   :: forall fit
--    . (ProfileInfo fit, KnownText fit)
--   => Shape Profile
--   -> NonEmpty (ProfileHeader info)
--   -> ProfileTree info
-- profileTree shape heads =
--   ProfileTree
--     { unit = Count
--     , wcs = Ref
--     , data_ = fileManager shape.axes
--     , meta =
--         ProfileTreeMeta
--           { headers = HeaderTable heads
--           , wavelength = profileWav @fit
--           , profile = T.toLower $ knownText @fit
--           }
--     }

data ProfileTreeMeta fit = ProfileTreeMeta
  { headers :: HeaderTable (ProfileHeader fit)
  , wavelength :: Wavelength Nm
  , profile :: Text
  }
  deriving (Generic, ToAsdf)

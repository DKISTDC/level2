{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NSO.Image.Asdf where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import GHC.Generics (Rep, from)
import NSO.Data.Spectra (midPoint)
import NSO.Data.Spectra qualified as Spectra
import NSO.Image.Asdf.FileManager (FileManager, fileManager)
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Asdf.NDCollection
import NSO.Image.Asdf.Ref
import NSO.Image.Fits
import NSO.Image.Fits.Quantity hiding (quantities)
import NSO.Image.GWCS
import NSO.Image.GWCS.L1GWCS
import NSO.Image.Headers.DataCommon
import NSO.Image.Headers.Types (PixelsPerBin)
import NSO.Image.Primary
import NSO.Image.Types.Frame (Arm (..), Arms (..), Frames (..), armsFrames)
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset, Dataset' (datasetId, primaryProposalId))
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import NSO.Types.Wavelength (Nm, SpectralLine (..), Wavelength (..), ionName)
import Telescope.Asdf as Asdf
import Telescope.Asdf.Class (GToObject (..))
import Telescope.Asdf.Core (Unit (..))
import Telescope.Data.KnownText
import Text.Casing (quietSnake)


-- DONE: move extra keys into meta.inventory
-- DONE: support ND collection
-- DONE: 3-arm profiles sodium
-- DONE: fit/orig separate GWCS + anchors
-- DONE: fix gwcs
-- TODO: labeled meta.axes for profiles

asdfDocument :: Id Inversion -> Dataset -> [Dataset] -> PixelsPerBin -> UTCTime -> L1Asdf -> Frames L2FitsMeta -> Document
asdfDocument inversionId dscanon dsets bin now l1asdf metas =
  let frames = Frames $ NE.sort metas.frames
   in Document (inversionTree frames)
 where
  inversionTree :: Frames L2FitsMeta -> InversionTree
  inversionTree sorted =
    InversionTree
      { fileuris
      , meta = inversionMeta $ fmap (.primary) sorted
      , quantities = quantitiesSection (fmap (.quantities) sorted) (qgwcs sorted)
      , profiles = profilesSection bin l1asdf.dataset.wcs (head sorted.frames).primary $ fmap (.profiles) sorted
      }

  -- choose a single frame from which to calculate the GWCS
  qgwcs :: Frames L2FitsMeta -> QuantityGWCS
  qgwcs sorted =
    quantityGWCS
      bin
      l1asdf.dataset.wcs
      (fmap (.primary) sorted)
      (fmap (\m -> m.quantities.items.opticalDepth) sorted)

  inversionMeta :: Frames PrimaryHeader -> InversionMeta
  inversionMeta headers =
    InversionMeta{headers = HeaderTable headers, inventory = inversionInventory headers}

  inversionInventory :: Frames PrimaryHeader -> InversionInventory
  inversionInventory headers =
    let specs = Spectra.identifyLines dsets
     in InversionInventory
          { frameCount = length headers
          , inversionId
          , proposalId = dscanon.primaryProposalId
          , canonicalDataset = dscanon.datasetId
          , spectralLines = specs
          , datasetIds = fmap (.datasetId) dsets
          , wavelengths = fmap Spectra.midPoint specs
          , created = now
          }

  fileuris = Fileuris $ fmap (.path) $ NE.toList metas.frames


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
  , proposalId :: Id Proposal
  , canonicalDataset :: Id Dataset
  , datasetIds :: [Id Dataset]
  , spectralLines :: [SpectralLine]
  , wavelengths :: [Wavelength Nm]
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
  schema _ = "tag:sunpy.org:ndcube/ndcollection-1.0.0"
  toValue section =
    Object
      [ ("meta", toNode meta)
      , ("aligned_axes", toNode $ QuantitiesAlignedAxes aligned)
      , ("items", toNode section.items)
      ]
   where
    aligned :: Quantities AlignedAxesF
    aligned = quantitiesFrom AlignedAxesF (alignedAxes section.axes).axes

    meta =
      Object
        [ ("axes", toNode section.axes)
        , ("gwcs", toNode section.gwcs)
        ]


-- Quantities ------------------------------------------------

quantitiesSection :: Frames FrameQuantitiesMeta -> QuantityGWCS -> QuantitiesSection
quantitiesSection metas gwcs =
  QuantitiesSection
    { axes =
        [ AxisMeta "frame_y" True
        , AxisMeta "slit_x" True
        , AxisMeta "optical_depth" True
        ]
    , gwcs
    , items =
        Quantities
          { opticalDepth = quantity (.opticalDepth)
          , temperature = quantity (.temperature)
          , electronPressure = quantity (.electronPressure)
          , microturbulence = quantity (.microturbulence)
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

  items :: Frames (Quantities QuantityHeader)
  items = fmap (.items) metas

  shape = (head metas.frames).shape


data DataTree meta info = DataTree
  { unit :: Unit
  , data_ :: FileManager
  , meta :: meta info
  , wcs :: Ref QuantityGWCS
  }
  deriving (Generic)
instance (ToAsdf (meta info), KnownText info) => ToAsdf (DataTree meta info) where
  -- schema _ = "asdf://dkist.nso.edu/tags/dataset-1.2.0"
  anchor _ = Just $ Anchor $ knownText @info
  toValue q =
    Object
      [ ("unit", toNode q.unit)
      , ("data", toNode q.data_)
      , ("meta", toNode q.meta)
      , ("wcs", toNode q.wcs)
      ]


snakeObject :: (Generic a, GToObject (Rep a)) => a -> [(Key, Node)]
snakeObject a = fmap (first (cs . quietSnake . cs)) $ gToObject (from a)


instance ToAsdf (Quantities (DataTree QuantityMeta)) where
  toValue q = Object $ snakeObject q


newtype QuantitiesAlignedAxes = QuantitiesAlignedAxes (Quantities AlignedAxesF)


instance ToAsdf QuantitiesAlignedAxes where
  toValue (QuantitiesAlignedAxes q) = Object $ snakeObject q


quantityTree
  :: forall info ext btype unit
   . (KnownText unit, HDUOrder info, info ~ DataHDUInfo ext btype unit)
  => Shape Quantity
  -> Frames (QuantityHeader info)
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

data ProfilesSection = ProfilesSection
  { axes :: [AxisMeta]
  , arms :: Arms (Profiles ProfileTree)
  , gwcs :: ProfileGWCS
  }


-- data ArmProfileAxes = ArmProfileAxes
--   { arm :: ArmWavMeta
--   , fit :: AlignedAxes Fit
--   , original :: AlignedAxes Original
--   }

-- newtype ArmsProfileAxes f = ArmsProfileAxes (Arms ArmProfileAxes)
--   deriving newtype (ToAsdf)

instance ToAsdf ProfilesSection where
  schema _ = "tag:sunpy.org:ndcube/ndcollection-1.0.0"
  toValue section =
    Object
      [ ("meta", toNode meta)
      , ("aligned_axes", toNode aligned)
      , ("items", toNode section.arms)
      ]
   where
    aligned :: Arms (Arm SpectralLine (Profiles AlignedAxesF))
    aligned = fmap alignedArmAxes section.arms

    alignedArmAxes :: Profiles ProfileTree -> Arm SpectralLine (Profiles AlignedAxesF)
    alignedArmAxes pfs =
      let AlignedAxes axs = alignedAxes section.axes
       in Arm pfs.fit.meta.spectralLine $ Profiles{fit = AlignedAxesF axs, original = AlignedAxesF axs}

    meta =
      Object
        [ ("axes", toNode section.axes)
        , ("gwcs", toNode section.gwcs)
        ]


instance ToAsdf ProfileGWCS where
  schema (ProfileGWCS g) = schema g
  anchor _ = Just $ Anchor $ knownText @ProfileGWCS
  toValue (ProfileGWCS g) = toValue g


instance ToAsdf (Arm SpectralLine (Profiles AlignedAxesF)) where
  toValue a =
    Object
      [ (profileKey a.arm Original, toNode a.value.original)
      , (profileKey a.arm Fit, toNode a.value.fit)
      ]


profileKey :: SpectralLine -> ProfileType -> Key
profileKey line pt = cs $ show line <> "_" <> suffix pt
 where
  suffix Original = "orig"
  suffix Fit = "fit"


-- newtype ProfilesItems = ProfilesItems (Arms (ArmProfile ProfileTree))
-- instance ToAsdf ProfilesItems where
--   schema _ = "asdf://dkist.nso.edu/tags/dataset-1.2.0"
--   toValue (ProfilesItems arms) = toValue arms

-- we need one ProfileMeta for each arm
profilesSection :: PixelsPerBin -> L1GWCS -> PrimaryHeader -> Frames (Arms ArmFrameProfileMeta) -> ProfilesSection
profilesSection bin l1gwcs primary profs =
  ProfilesSection
    { axes = [AxisMeta "frame_y" True, AxisMeta "slit_x" True, AxisMeta "wavelength" False, AxisMeta "stokes" True]
    , arms = profilesArmsTree profs
    , gwcs = profileGWCS bin l1gwcs primary
    }


-- where
--  armGWCS :: Arm SpectralLine L1GWCS -> ArmFrameProfileMeta -> Arm SpectralLine ProfileGWCS
--  armGWCS ag am =
--    Arm ag.arm $ profileGWCS bin ag.value primary am.fit.wcs

profilesArmsTree :: Frames (Arms ArmFrameProfileMeta) -> Arms (Profiles ProfileTree)
profilesArmsTree framesByArms =
  let Arms arms = armsFrames framesByArms :: Arms (Frames ArmFrameProfileMeta)
      armNums = NE.fromList [0 ..] :: NonEmpty Int
   in Arms $ NE.zipWith armProfileTree armNums arms
 where
  armProfileTree :: Int -> Frames ArmFrameProfileMeta -> Profiles ProfileTree
  armProfileTree armNum profs =
    let frame = head profs.frames
        arm = frame.arm
        index = hduIndex @(Arms Profile) + HDUIndex (armNum * 2)
        original = profileTree index arm frame.shape $ fmap (\f -> f.original) profs
        fit = profileTree (index + 1) arm frame.shape $ fmap (\f -> f.fit) profs
     in Profiles{fit, original}

  profileTree :: forall fit. (KnownText fit) => HDUIndex -> ArmWavMeta -> Shape Profile -> Frames (ProfileHeader fit) -> ProfileTree fit
  profileTree ix arm shape heads =
    ProfileTree
      { unit = Count
      , data_ = fileManager shape.axes ix
      , wcs = Ref
      , meta =
          ProfileTreeMeta
            { headers = HeaderTable heads
            , spectralLine = arm.line
            , profile = T.toLower $ knownText @fit
            }
      }


instance ToAsdf (Profiles ProfileTree) where
  toValue ps =
    let line = ps.fit.meta.spectralLine
     in Object
          [ (profileKey line Original, toNode ps.original)
          , (profileKey line Fit, toNode ps.fit)
          ]


data ProfileTree fit = ProfileTree
  { unit :: Unit
  , data_ :: FileManager
  , meta :: ProfileTreeMeta fit
  , wcs :: Ref ProfileGWCS
  }
  deriving (Generic)
instance (KnownText fit) => ToAsdf (ProfileTree fit) where
  -- anchor _ = Just $ Anchor $ knownText @fit
  -- schema _ = "asdf://dkist.nso.edu/tags/dataset-1.2.0"
  toValue p =
    Object
      [ ("unit", toNode p.unit)
      , ("data", toNode p.data_)
      , ("meta", toNode p.meta)
      , ("wcs", toNode p.wcs)
      ]


data ProfileTreeMeta fit = ProfileTreeMeta
  { headers :: HeaderTable (ProfileHeader fit)
  , spectralLine :: SpectralLine
  , profile :: Text
  }
  deriving (Generic)
instance (KnownText fit) => ToAsdf (ProfileTreeMeta fit) where
  toValue m =
    Object
      [ ("ion", toNode $ ionName m.spectralLine)
      , ("wavelength", toNode $ midPoint m.spectralLine)
      , ("profile", toNode m.profile)
      , ("headers", toNode m.headers)
      , ("inventory", toNode (Ref @InversionInventory))
      ]

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
import NSO.Files
import NSO.Files.Image (L2Asdf, L2Fits)
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch as Scratch (ScratchError (..), pathExists)
import NSO.Image.Asdf.FileManager (FileManager, fileManager)
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Asdf.Ref
import NSO.Image.Fits.Frame
import NSO.Image.Fits.Meta
import NSO.Image.Fits.Profile
import NSO.Image.Fits.Quantity hiding (quantities, toList)
import NSO.Image.GWCS
import NSO.Image.GWCS.AxisMeta
import NSO.Image.GWCS.L1GWCS
import NSO.Image.Headers.DataCommon
import NSO.Image.Headers.Types (PixelsPerBin)
import NSO.Image.Headers.Types qualified as Image
import NSO.Image.Headers.WCS
import NSO.Image.Primary
import NSO.Image.Types.Frame (Arm (..), Arms (..), Depth, Frames (..), armsFrames)
import NSO.Image.Types.Profile
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset, Dataset' (datasetId, primaryProposalId, spectralLines))
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import NSO.Types.Wavelength (Nm, SpectralLine (..), Wavelength (..), ionName, spectralLineName, spectralLineShort)
import Numeric (showFFloat)
import Telescope.Asdf as Asdf
import Telescope.Asdf.Class (GToObject (..))
import Telescope.Asdf.Core as Telescope (Unit (..))
import Telescope.Asdf.GWCS (Pix)
import Telescope.Data.KnownText
import Text.Casing (quietSnake)


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
      , quantities = quantitiesSection bin l1asdf (fmap (.quantities) sorted) (fmap (.primary) sorted)
      , profiles = profilesSection bin l1asdf.dataset.wcs (head sorted.frames).primary $ fmap (.profiles) sorted
      }
  -- = orderedAxes @(Depth, HPLon, HPLat, Time)

  inversionMeta :: Frames PrimaryHeader -> InversionMeta
  inversionMeta headers =
    InversionMeta
      { headers = HeaderTable headers
      , inventory = inversionInventory headers
      , l1gwcs = l1asdf.dataset.wcs
      }

  inversionInventory :: Frames PrimaryHeader -> InversionInventory
  inversionInventory headers =
    let slines :: [SpectralLine] = concatMap (.spectralLines) dsets
     in InversionInventory
          { frameCount = length headers
          , inversionId
          , proposalId = dscanon.primaryProposalId
          , canonicalDataset = dscanon.datasetId
          , spectralLines = fmap spectralLineName slines
          , datasetIds = fmap (.datasetId) dsets
          , wavelengths = fmap (.wavelength) slines
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
  deriving (Generic)
instance ToAsdf InversionTree where
  schema _ = "asdf://dkist.nso.edu/tags/inversion-0.1.0"


data InversionMeta = InversionMeta
  { inventory :: InversionInventory
  , headers :: HeaderTable PrimaryHeader
  , l1gwcs :: L1GWCS
  }
  deriving (Generic, ToAsdf)


data InversionInventory = InversionInventory
  { inversionId :: Id Inversion
  , created :: UTCTime
  , proposalId :: Id Proposal
  , canonicalDataset :: Id Dataset
  , datasetIds :: [Id Dataset]
  , spectralLines :: [Text]
  , wavelengths :: [Wavelength Nm]
  , frameCount :: Int
  }
  deriving (Generic)
instance KnownText InversionInventory where
  knownText = "InversionInventory"
instance ToAsdf InversionInventory where
  anchor _ = Just $ Anchor (knownText @InversionInventory)


newtype Fileuris = Fileuris [Path Scratch Filename L2Fits]


instance ToAsdf Fileuris where
  anchor _ = Just $ Anchor "fileuris"
  toValue (Fileuris ps) =
    Array $ fmap pathNode ps
   where
    pathNode (Path fp) = fromValue $ String $ cs fp


data QuantitiesSection = QuantitiesSection
  { axes :: (OrderedAxis (Pix Depth), OrderedAxis (Pix X), OrderedAxis (Pix Y))
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
    aligned = quantitiesFrom AlignedAxesF (alignedAxes $ FitsOrderAxes section.axes).axes

    meta =
      Object
        [ ("axes", toNode $ FitsOrderAxes section.axes)
        , ("gwcs", toNode section.gwcs)
        ]


-- Quantities ------------------------------------------------

quantitiesSection
  :: forall inputs
   . (inputs ~ (Pix Depth, Pix X, Pix Y))
  => PixelsPerBin
  -> L1Asdf
  -> Frames FrameQuantitiesMeta
  -> Frames PrimaryHeader
  -> QuantitiesSection
quantitiesSection bin l1asdf metas prims =
  QuantitiesSection
    { axes = orderedAxes @inputs
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
  -- choose a single frame from which to calculate the GWCS
  gwcs :: QuantityGWCS
  gwcs =
    quantityGWCS @inputs
      bin
      l1asdf.dataset.wcs
      prims
      (fmap (\m -> m.items.opticalDepth) metas)

  quantity
    :: forall info ext btype unit
     . (info ~ DataHDUInfo ext btype unit, KnownText unit, HDUOrder info, ToAstropyUnit unit)
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
  schema _ = "asdf://dkist.nso.edu/tags/dataset-1.2.0"
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
   . (KnownText unit, HDUOrder info, ToAstropyUnit unit, info ~ DataHDUInfo ext btype unit)
  => Shape Quantity
  -> Frames (QuantityHeader info)
  -> DataTree QuantityMeta info
quantityTree shape heads =
  DataTree
    { unit = toAstropyUnit @unit
    , wcs = Ref
    , data_ = fileManager shape.axes (hduIndex @info)
    , meta = QuantityMeta{headers = HeaderTable heads, inventory = Ref}
    }


class ToAstropyUnit a where
  toAstropyUnit :: Unit
instance ToAstropyUnit Image.Dimensionless where
  toAstropyUnit = DimensionlessUnscaled
instance ToAstropyUnit Image.Kelvin where
  toAstropyUnit = Kelvin
instance ToAstropyUnit Image.N_m2 where
  toAstropyUnit = Product Newtons (Exponent (-2) Meters)
instance ToAstropyUnit Image.Km_s where
  toAstropyUnit = Product Telescope.Kilometers (Exponent (-1) Seconds)
instance ToAstropyUnit Image.Tesla where
  toAstropyUnit = Tesla
instance ToAstropyUnit Image.Deg where
  toAstropyUnit = Degrees
instance ToAstropyUnit Image.Km where
  toAstropyUnit = Telescope.Kilometers
instance ToAstropyUnit Image.Kg_m3 where
  toAstropyUnit = Product Kilograms (Exponent (-3) Meters)


data QuantityMeta info = QuantityMeta
  { headers :: HeaderTable (QuantityHeader info)
  , inventory :: Ref InversionInventory
  }
  deriving (Generic, ToAsdf)


-- Profiles ------------------------------------------------

-- . (inputs ~ (Pix Depth, Pix X, Pix Y))
data ProfilesSection = ProfilesSection
  { axes :: (OrderedAxis (Pix Stokes), OrderedAxis (Pix Wav), OrderedAxis (Pix X), OrderedAxis (Pix Y))
  , arms :: Arms (Profiles ProfileTree)
  , gwcs :: Arms (Arm SpectralLine ProfileGWCS)
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
      let AlignedAxes axs = alignedAxes $ FitsOrderAxes section.axes
       in Arm pfs.fit.meta.spectralLine $ Profiles{fit = AlignedAxesF axs, original = AlignedAxesF axs}

    meta =
      Object
        [ ("axes", toNode $ FitsOrderAxes section.axes)
        , ("gwcs", toNode section.gwcs)
        ]


instance ToAsdf ProfileGWCS where
  schema (ProfileGWCS g) = schema g
  toValue (ProfileGWCS g) = toValue g


instance ToAsdf (Arm SpectralLine ProfileGWCS) where
  schema (Arm _ g) = schema g
  anchor (Arm l _) = Just $ profileGWCSAnchor l
  toValue (Arm l g) =
    Object [(spectralLineKeyword l, Node (schema g) (anchor (Arm l g)) (toValue g))]


profileGWCSAnchor :: SpectralLine -> Anchor
profileGWCSAnchor l = Anchor $ "ProfileGWCS" <> spectralLineKeyword l


spectralLineKeyword :: SpectralLine -> Text
spectralLineKeyword l =
  let Wavelength w = l.wavelength
   in T.replace " " "" (simplifyDesignationName $ spectralLineShort l) <> cs (showFFloat (Just 0) w "")
 where
  simplifyDesignationName =
    -- remove the numeric suffixes
    T.replace "1" "" . T.replace "2" ""


instance ToAsdf (Arm SpectralLine (Profiles AlignedAxesF)) where
  toValue a =
    Object
      [ (profileKey a.arm Original, toNode a.value.original)
      , (profileKey a.arm Fit, toNode a.value.fit)
      ]


profileKey :: SpectralLine -> ProfileType -> Key
profileKey line pt = cs $ spectralLineKeyword line <> "_" <> suffix pt
 where
  suffix Original = "orig"
  suffix Fit = "fit"


profilesSection
  :: forall inputs
   . (inputs ~ (Pix Stokes, Pix Wav, Pix X, Pix Y))
  => PixelsPerBin
  -> L1GWCS
  -> PrimaryHeader
  -> Frames (Arms ArmFrameProfileMeta)
  -> ProfilesSection
profilesSection bin l1gwcs primary profs =
  let sampleFrame :: Arms ArmFrameProfileMeta = head profs.frames
   in ProfilesSection
        { axes = orderedAxes @inputs
        , arms = profilesArmsTree profs
        , gwcs = fmap armGWCS sampleFrame
        }
 where
  armGWCS :: ArmFrameProfileMeta -> Arm SpectralLine ProfileGWCS
  armGWCS am =
    Arm am.arm.line $ profileGWCS @inputs bin l1gwcs primary am.fit.wcs


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
  schema _ = "asdf://dkist.nso.edu/tags/dataset-1.2.0"
  toValue p =
    Object
      [ ("unit", toNode p.unit)
      , ("data", toNode p.data_)
      , ("meta", toNode p.meta)
      , ("wcs", toNode $ Alias $ profileGWCSAnchor p.meta.spectralLine)
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
      [ ("ion", toNode $ ionName m.spectralLine.ion)
      , ("wavelength", toNode m.spectralLine.wavelength)
      , ("profile", toNode m.profile)
      , ("headers", toNode m.headers)
      , ("inventory", toNode (Ref @InversionInventory))
      ]


generatedL2FrameAsdf :: (Scratch :> es, Error ScratchError :> es) => Id Proposal -> Id Inversion -> Eff es (Path Scratch Filename L2Asdf)
generatedL2FrameAsdf propId invId = do
  let fname = Files.filenameL2Asdf propId invId
  let fpath = filePath (Files.outputL2Dir propId invId) fname
  exists <- Scratch.pathExists fpath
  unless exists $ do
    throwError $ ScratchPathMissing fpath.filePath "Generated L2 Asdf"
  pure fname

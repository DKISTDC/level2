module NSO.Image.Fits.Meta where

import Control.Monad (filterM)
import Data.List.Ext
import Data.Massiv.Array ()
import Effectful
import Effectful.Error.Static
import NSO.Image.Fits.Frame
import NSO.Image.Fits.Profile as Profile
import NSO.Image.Fits.Quantity as Quantity
import NSO.Image.Headers.Keywords (IsKeyword (keyword))
import NSO.Image.Headers.Types (ProfIon (..), ProfType (..), SliceXY)
import NSO.Image.Primary
import NSO.Image.Types.Frame (Arms (..))
import NSO.Image.Types.Profile
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Wavelength (SpectralLine (..))
import Telescope.Asdf as Asdf
import Telescope.Data.Axes (Axes (..), axesRowMajor)
import Telescope.Data.DataCube (dataCubeAxes)
import Telescope.Data.Parser (ParseError, parseFail)
import Telescope.Fits as Fits
import Telescope.Fits.Header.Class (parseKeyword)


{- | Meta information for an L2 frame, that isn't the frame itself
allows us to generate the ASDF by reading the FITS
instead of keeping all the fits data in memory
-}
data L2FitsMeta = L2FitsMeta
  { path :: Path' Filename L2FrameFits
  , primary :: PrimaryHeader
  , quantities :: FrameQuantitiesMeta
  , profiles :: Arms ArmFrameProfileMeta
  }


instance Eq L2FitsMeta where
  m1 == m2 = m1.path == m2.path
instance Ord L2FitsMeta where
  m1 <= m2 = m1.path <= m2.path


data FrameQuantitiesMeta = FrameQuantitiesMeta
  { shape :: Shape Quantity
  , items :: Quantities QuantityHeader
  }
  deriving (Generic)


data ArmFrameProfileMeta = ArmFrameProfileMeta
  { arm :: ArmWavMeta
  , shape :: Shape Profile
  , fit :: ProfileHeader Fit
  , original :: ProfileHeader Original
  }
  deriving (Generic)


newtype Shape a = Shape {axes :: Axes Row}
  deriving (Show)


newtype QuantityShape = QuantityShape {axes :: Axes Row}


instance FromHeader (Shape Profile) where
  parseHeader h = do
    n1 <- parseKeyword "NAXIS1" h
    n2 <- parseKeyword "NAXIS2" h
    n3 <- parseKeyword "NAXIS3" h
    n4 <- parseKeyword "NAXIS4" h
    pure $ Shape $ axesRowMajor [n4, n3, n2, n1]


--

instance FromHeader (Shape Quantity) where
  parseHeader h = do
    n1 <- parseKeyword "NAXIS1" h
    n2 <- parseKeyword "NAXIS2" h
    n3 <- parseKeyword "NAXIS3" h
    pure $ Shape $ axesRowMajor [n3, n2, n1]


frameMetaFromL2Fits
  :: (Error ParseError :> es, Error ProfileError :> es, Error QuantityError :> es)
  => Path' Filename L2FrameFits
  -> SliceXY
  -> Arms ArmWavMeta
  -> BinTableHDU
  -> Fits
  -> Eff es L2FitsMeta
frameMetaFromL2Fits path slice arms l1 fits = runParser $ do
  primary <- parseHeader @PrimaryHeader fits.primaryHDU.header

  qh <- headerAt $ hduIndex @OpticalDepth
  qshape <- parseHeader @(Shape Quantity) qh
  quants <- parseQuantities

  ps <- parseAllProfiles arms $ profileHeaders fits

  -- profs <- parseProfiles
  pure $
    L2FitsMeta
      { path
      , primary
      , quantities = FrameQuantitiesMeta{items = quants, shape = qshape}
      , -- need to read all of these!
        profiles = ps
      }
 where
  headerAt :: forall es. (Parser :> es) => HDUIndex -> Eff es Header
  headerAt (HDUIndex index) =
    let extIndex = index - 1
     in case fits.extensions !? extIndex of
          Nothing -> parseFail $ "Missing HDU at " ++ show index
          Just (Image img) -> pure img.header
          Just _ -> parseFail $ "Expected ImageHDU at " ++ show index

  profileHeaders :: Fits -> [Header]
  profileHeaders f =
    let HDUIndex start = hduIndex @(Arms Profile)
     in fmap header $ drop (start - 1) f.extensions
   where
    header (Image dat) = dat.header
    header (BinTable bin) = bin.header

  parseAllProfiles :: (Error ProfileError :> es, Parser :> es) => Arms ArmWavMeta -> [Header] -> Eff es (Arms ArmFrameProfileMeta)
  parseAllProfiles metas hs = do
    as <- mapM (\(arm :: ArmWavMeta) -> parseArmProfile arm hs) metas.arms
    pure $ Arms as

  -- what are looking at here? How many headers are there? One per frame? No.... One per profile
  parseArmProfile :: forall es. (Parser :> es, Error ProfileError :> es) => ArmWavMeta -> [Header] -> Eff es ArmFrameProfileMeta
  parseArmProfile arm hs = do
    fith <- findProfile arm.line Fit hs
    orgh <- findProfile arm.line Original hs
    fit <- parseProfileFit @Fit arm fith
    original <- parseProfileFit @Original arm orgh
    shape <- parseHeader @(Shape Profile) fith
    pure $ ArmFrameProfileMeta{arm, shape, fit, original}

  parseProfileFit :: forall fit es. (Parser :> es, Error ProfileError :> es) => ArmWavMeta -> Header -> Eff es (ProfileHeader fit)
  parseProfileFit meta h = do
    common <- parseHeader h
    wcs <- Profile.wcsHeader meta slice h
    pure $ ProfileHeader{meta, common, wcs}

  findProfile :: (Parser :> es, Error ProfileError :> es) => SpectralLine -> ProfileType -> [Header] -> Eff es Header
  findProfile line typ hs = do
    mh <- flip filterM hs $ \h -> do
      typ' <- parseProfileType h
      ProfIon line' <- parseKeyword (keyword @ProfIon) h
      pure $ typ' == typ && line' == line
    case mh of
      (h : _) -> pure h
      _ -> throwError $ MissingProfileType line typ

  parseProfileType :: (Error ProfileError :> es) => (Parser :> es) => Header -> Eff es ProfileType
  parseProfileType h = do
    ProfType t <- parseKeyword (keyword @ProfType) h
    case t of
      "Original" -> pure Original
      "Fit" -> pure Fit
      _ -> throwError $ InvalidProfileType t

  parseQuantity :: forall q es. (HDUOrder q, FromHeader q, Parser :> es, Error QuantityError :> es) => Eff es (QuantityHeader q)
  parseQuantity = do
    h <- headerAt $ hduIndex @q
    hduInfo <- parseHeader @q h
    common <- parseHeader h
    wcs <- Quantity.wcsHeader slice l1.header
    pure $ QuantityHeader{hduInfo, common, wcs}

  -- parseProfile
  --   :: forall fit wav es
  --    . (FromHeader fit, HDUOrder fit, wav ~ ProfileWav info, Error ProfileError :> es, Parser :> es)
  --   => WavProfile wav
  --   -> Eff es (ProfileHeader info)
  -- parseProfile wprofile = do
  --   h <- headerFor @info
  --   info <- parseHeader @info h
  --   common <- parseHeader h
  --   wcs <- Profile.wcsHeader wprofile slice l1.header
  --   pure $ ProfileHeader{info, common, wcs}

  parseQuantities = do
    opticalDepth <- parseQuantity
    temperature <- parseQuantity
    electronPressure <- parseQuantity
    microTurbulence <- parseQuantity
    magStrength <- parseQuantity
    velocity <- parseQuantity
    magInclination <- parseQuantity
    magAzimuth <- parseQuantity
    geoHeight <- parseQuantity
    gasPressure <- parseQuantity
    density <- parseQuantity
    pure $ Quantities{opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density}


-- parseProfiles = do
--   orig630 <- parseProfile @Orig630 wpo.wav630
--   orig854 <- parseProfile @Orig854 wpo.wav854
--   fit630 <- parseProfile @Fit630 wpf.wav630
--   fit854 <- parseProfile @Fit854 wpf.wav854
--   pure $ Profiles{orig630, orig854, fit630, fit854}

frameMeta :: L2FrameFits -> Path' Filename L2FrameFits -> L2FitsMeta
frameMeta frame path =
  L2FitsMeta
    { primary = frame.primary
    , quantities = quantitiesMeta frame.quantities
    , profiles = undefined -- Arms [] -- profilesMeta frame.profiles
    , path
    }
 where
  quantitiesMeta :: Quantities QuantityFrameFits -> FrameQuantitiesMeta
  quantitiesMeta qs =
    FrameQuantitiesMeta
      { items = quantityHeaders qs
      , shape = Shape $ addDummyY $ dataCubeAxes qs.opticalDepth.image
      }

  -- profilesMeta :: Profiles Profile -> FrameProfilesMeta
  -- profilesMeta ps =
  --   FrameProfilesMeta
  --     { profiles = mapProfiles (\p -> p.header) ps
  --     , shape630 = Shape $ addDummyY $ dataCubeAxes ps.orig630.image
  --     , shape854 = Shape $ addDummyY $ dataCubeAxes ps.orig854.image
  --     }

  addDummyY :: Axes Row -> Axes Row
  addDummyY (Axes axs) = Axes (1 : axs)

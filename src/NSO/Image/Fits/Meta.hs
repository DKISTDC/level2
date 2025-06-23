module NSO.Image.Fits.Meta where

import Data.List.Ext
import Data.Massiv.Array ()
import Effectful
import Effectful.Error.Static
import NSO.Image.Fits.Frame
import NSO.Image.Fits.Profile as Profile
import NSO.Image.Fits.Quantity as Quantity
import NSO.Image.Headers.Types (SliceXY)
import NSO.Image.Primary
import NSO.Image.Types.Profile
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
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
  , profiles :: Arms ProfileMeta -- how do we know if it is a fit or original profile?
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


-- TODO: are we original or fit? How is this shown in the asdf?
data ProfileMeta = ProfileMeta
  { shape :: Shape Profile
  , profile :: Profile ProfileHeader
  }
  deriving (Generic)


newtype Shape a = Shape {axes :: Axes Row}


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

  -- no, we have to look up the appropriate hdu
  qh <- headerAt $ hduIndex @OpticalDepth
  qshape <- parseHeader @(Shape Quantity) qh
  quants <- parseQuantities

  -- p630 <- headerFor @Orig630
  -- shape630 <- parseHeader @(Shape Profile) p630
  --
  -- p854 <- headerFor @Orig854
  -- shape854 <- parseHeader @(Shape Profile) p854

  -- profs <- parseProfiles
  pure $
    L2FitsMeta
      { path
      , primary
      , quantities = FrameQuantitiesMeta{items = quants, shape = qshape}
      , profiles = undefined -- Arms [] -- FrameProfilesMeta{profiles = profs, shape630, shape854}
      }
 where
  headerAt :: forall es. (Parser :> es) => HDUIndex -> Eff es Header
  headerAt (HDUIndex index) =
    let extIndex = index - 1
     in case fits.extensions !? extIndex of
          Nothing -> parseFail $ "Missing HDU at " ++ show index
          Just (Image img) -> pure img.header
          Just _ -> parseFail $ "Expected ImageHDU at " ++ show index

  parseQuantity :: forall q es. (HDUOrder q, FromHeader q, Parser :> es, Error QuantityError :> es) => Eff es (QuantityHeader q)
  parseQuantity = do
    h <- headerAt $ hduIndex @q
    info <- parseHeader @q h
    common <- parseHeader h
    wcs <- Quantity.wcsHeader slice l1.header
    pure $ QuantityHeader{info, common, wcs}

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

{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Frame where

import Data.ByteString qualified as BS
import Data.List ((!?))
import Data.Massiv.Array ()
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import NSO.Image.DataCube (dataCubeAxes)
import NSO.Image.Headers
import NSO.Image.Headers.Types (DateTime (..), Depth, Key (..), SliceXY, SlitX)
import NSO.Image.Primary
import NSO.Image.Profile as Profile
import NSO.Image.Quantity as Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Inversion (Inversion)
import Telescope.Asdf as Asdf
import Telescope.Data.Axes (Axes (..), axesRowMajor)
import Telescope.Data.Parser (ParseError, parseFail, runParser)
import Telescope.Fits as Fits
import Telescope.Fits.Encoding (replaceKeywordLine)
import Telescope.Fits.Header.Class (parseKeyword)


data L2Frame = L2Frame
  { primary :: PrimaryHeader
  , quantities :: Quantities Quantity
  , profiles :: Profiles Profile
  }
  deriving (Generic)


instance ToAsdf L2Frame where
  toValue _ =
    Object
      [ ("primary", fromValue Null)
      , ("quantities", fromValue Null)
      , ("profiles", fromValue Null)
      ]


data L2FrameMeta = L2FrameMeta
  { path :: Path' Filename L2Frame
  , primary :: PrimaryHeader
  , quantities :: FrameQuantitiesMeta
  , profiles :: FrameProfilesMeta
  }
instance Eq L2FrameMeta where
  m1 == m2 = m1.path == m2.path
instance Ord L2FrameMeta where
  m1 <= m2 = m1.path <= m2.path


data FrameQuantitiesMeta = FrameQuantitiesMeta
  { shape :: Shape Quantity
  , quantities :: Quantities QuantityHeader
  }
  deriving (Generic)


data FrameProfilesMeta = FrameProfilesMeta
  { shape :: Shape Profile
  , profiles :: Profiles ProfileHeader
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


instance FromHeader (Shape Quantity) where
  parseHeader h = do
    n1 <- parseKeyword "NAXIS1" h
    n2 <- parseKeyword "NAXIS2" h
    n3 <- parseKeyword "NAXIS3" h
    pure $ Shape $ axesRowMajor [n3, n2, n1]


data L2FrameInputs = L2FrameInputs
  { quantities :: Quantities (QuantityImage [SlitX, Depth])
  , profileFit :: ProfileFrame Fit
  , profileOrig :: ProfileFrame Original
  , l1Frame :: BinTableHDU
  }


filenameL2Frame :: Id Inversion -> DateTime -> Path' Filename L2Frame
filenameL2Frame ii (DateTime dt) = Path $ cs (T.toUpper $ T.map toUnderscore $ ii.fromId <> "_" <> dt) <> "_L2.fits"
 where
  toUnderscore :: Char -> Char
  toUnderscore '.' = '_'
  toUnderscore ':' = '_'
  toUnderscore '-' = '_'
  toUnderscore c = c


generateL2Frame
  :: (Error QuantityError :> es, Error ProfileError :> es, Error PrimaryError :> es, GenRandom :> es)
  => UTCTime
  -> Id Inversion
  -> SliceXY
  -> WavProfiles Original
  -> WavProfiles Fit
  -> L2FrameInputs
  -> Eff es (L2Frame, DateTime)
generateL2Frame now i slice wpo wpf gf = do
  ph <- primaryHeader i gf.l1Frame.header
  qs <- quantities slice now gf.l1Frame.header gf.quantities
  ps <- profiles slice now gf.l1Frame.header wpo wpf gf.profileOrig gf.profileFit
  let dateBeg = ph.observation.dateBeg.ktype
  let frame = L2Frame{primary = ph, quantities = qs, profiles = ps}
  pure (frame, dateBeg)


frameMetaFromL2Fits
  :: (Error ParseError :> es, Error ProfileError :> es, Error QuantityError :> es)
  => Path' Filename L2Frame
  -> SliceXY
  -> WavProfiles Original
  -> WavProfiles Fit
  -> BinTableHDU
  -> Fits
  -> Eff es L2FrameMeta
frameMetaFromL2Fits path slice wpo wpf l1 fits = runParser $ do
  primary <- parseHeader @PrimaryHeader fits.primaryHDU.header

  -- no, we have to look up the appropriate hdu
  qh <- headerFor @OpticalDepth
  qshape <- parseHeader @(Shape Quantity) qh
  quants <- parseQuantities

  ph <- headerFor @Orig630
  pshape <- parseHeader @(Shape Profile) ph
  profs <- parseProfiles
  pure $
    L2FrameMeta
      { path
      , primary
      , quantities = FrameQuantitiesMeta{quantities = quants, shape = qshape}
      , profiles = FrameProfilesMeta{profiles = profs, shape = pshape}
      }
 where
  headerFor :: forall a es. (HDUOrder a, Parser :> es) => Eff es Header
  headerFor =
    let HDUIndex index = hduIndex @a
     in case fits.extensions !? index of
          Nothing -> parseFail $ "Missing HDU at " ++ show index
          Just (Image img) -> pure img.header
          Just _ -> parseFail $ "Expected ImageHDU at " ++ show index

  parseQuantity :: forall q es. (HDUOrder q, FromHeader q, Parser :> es, Error QuantityError :> es) => Eff es (QuantityHeader q)
  parseQuantity = do
    h <- headerFor @q
    info <- parseHeader @q h
    common <- parseHeader h
    wcs <- Quantity.wcsHeader slice l1.header
    pure $ QuantityHeader{info, common, wcs}

  parseProfile
    :: forall info wav es
     . (FromHeader info, HDUOrder info, wav ~ ProfileWav info, Error ProfileError :> es, Parser :> es)
    => WavProfile wav
    -> Eff es (ProfileHeader info)
  parseProfile wprofile = do
    h <- headerFor @info
    info <- parseHeader @info h
    common <- parseHeader h
    wcs <- Profile.wcsHeader wprofile slice l1.header
    pure $ ProfileHeader{info, common, wcs}

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

  parseProfiles = do
    orig630 <- parseProfile @Orig630 wpo.wav630
    orig854 <- parseProfile @Orig854 wpo.wav854
    fit630 <- parseProfile @Fit630 wpf.wav630
    fit854 <- parseProfile @Fit854 wpf.wav854
    pure $ Profiles{orig630, orig854, fit630, fit854}


frameMeta :: L2Frame -> Path' Filename L2Frame -> L2FrameMeta
frameMeta frame path =
  L2FrameMeta
    { primary = frame.primary
    , quantities = quantitiesMeta frame.quantities
    , profiles = profilesMeta frame.profiles
    , path
    }
 where
  quantitiesMeta qs =
    FrameQuantitiesMeta
      { quantities = quantityHeaders qs
      , shape = Shape $ addDummyY $ dataCubeAxes qs.opticalDepth.image
      }

  profilesMeta ps =
    FrameProfilesMeta
      { profiles = profileHeaders ps
      , shape = Shape $ addDummyY $ dataCubeAxes ps.orig630.image
      }

  addDummyY :: Axes Row -> Axes Row
  addDummyY (Axes axs) = Axes (1 : axs)


-- | See the HDUOrder class below
frameToFits :: L2Frame -> Fits
frameToFits frame =
  let prim = primaryHDU frame.primary
      images = quantityHDUs frame.quantities
      profs = profileHDUs frame.profiles
   in Fits prim $ fmap Image (images <> profs)


-- | Encode and insert framevol
encodeL2 :: Fits -> BS.ByteString
encodeL2 f' =
  let out = Fits.encode f'
      mb = fromIntegral (BS.length out) / 1000000
   in replaceKeywordLine "FRAMEVOL" (Float mb) (Just "[Mb]") out


encodeL2Asdf :: (Error AsdfError :> es, IOE :> es) => L2Frame -> Eff es BS.ByteString
encodeL2Asdf = Asdf.encode


class HDUOrder a where
  hduIndex :: HDUIndex


newtype HDUIndex = HDUIndex Int
  deriving newtype (ToAsdf, Num)


instance HDUOrder OpticalDepth where
  hduIndex = 1
instance HDUOrder Temperature where
  hduIndex = 2
instance HDUOrder ElectronPressure where
  hduIndex = 3
instance HDUOrder Microturbulence where
  hduIndex = 4
instance HDUOrder MagStrength where
  hduIndex = 5
instance HDUOrder Velocity where
  hduIndex = 6
instance HDUOrder MagInclination where
  hduIndex = 7
instance HDUOrder MagAzimuth where
  hduIndex = 8
instance HDUOrder GeoHeight where
  hduIndex = 9
instance HDUOrder GasPressure where
  hduIndex = 10
instance HDUOrder Density where
  hduIndex = 11
instance HDUOrder Orig630 where
  hduIndex = 11
instance HDUOrder Orig854 where
  hduIndex = 12
instance HDUOrder Fit630 where
  hduIndex = 13
instance HDUOrder Fit854 where
  hduIndex = 14

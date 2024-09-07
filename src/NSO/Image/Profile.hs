{-# LANGUAGE RecordWildCards #-}

module NSO.Image.Profile where

import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Data.ByteString qualified as BS
import Data.Massiv.Array (Ix2 (..), IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Data.Maybe (isJust)
import Effectful
import Effectful.Error.Static
import NSO.Data.Spectra (midPoint)
import NSO.Image.DataCube
import NSO.Image.Error
import NSO.Image.Headers
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Image.Headers.WCS
import NSO.Image.Quantities (DataCommon (..), DataHDUInfo (..), DataHeader (..), addDummyAxis, dataCommon, splitFrames)
import NSO.Prelude
import NSO.Types.Wavelength (CaIILine (..), Nm, SpectralLine (..), Wavelength (..))
import Telescope.Fits as Fits
import Telescope.Fits.Header (toInt)


-- BUG: PC self_self is wrong
--  ?? Is this still an issue?

data Original
data Fit


type Wav630 = Center 630 Nm
type Wav854 = Center 854 Nm


-- Generating Profiles -------------------------------------------------------------------------------

type ProfileInfo' ext = DataHDUInfo ext "spect.line.profile" Dimensionless


type family ProfileInfo profile wav where
  ProfileInfo Original Wav630 = ProfileInfo' "Original Profile 630.2nm"
  ProfileInfo Original Wav854 = ProfileInfo' "Original Profile 854.2nm"
  ProfileInfo Fit Wav630 = ProfileInfo' "Fit Profile 630.2nm"
  ProfileInfo Fit Wav854 = ProfileInfo' "Fit Profile 854.2nm"


data Profiles (f :: Type -> Type -> Type) = Profiles
  { orig630 :: f Original Wav630
  , orig854 :: f Original Wav854
  , fit630 :: f Fit Wav630
  , fit854 :: f Fit Wav854
  }


data ProfileHeader profile wav = ProfileHeader
  { info :: ProfileInfo profile wav
  , common :: DataCommon
  , wcs :: WCSHeader ProfileAxes
  }


data Profile profile wav = Profile
  { image :: DataCube [SlitX, Wavelength wav, Stokes]
  , header :: ProfileHeader profile wav
  }


profiles
  :: (Error ParseKeyError :> es)
  => SliceXY
  -> UTCTime
  -> Header
  -> WavProfiles Original
  -> WavProfiles Fit
  -> ProfileFrame Original
  -> ProfileFrame Fit
  -> Eff es (Profiles Profile)
profiles slice now l1 wpo wpf po pf = do
  orig630 <- profile @Original @Wav630 DataHDUInfo wpo.wav630 po.wav630
  orig854 <- profile @Original @Wav854 DataHDUInfo wpo.wav854 po.wav854
  fit630 <- profile @Fit @Wav630 DataHDUInfo wpf.wav630 pf.wav630
  fit854 <- profile @Fit @Wav854 DataHDUInfo wpf.wav854 pf.wav854
  pure $ Profiles{orig630, orig854, fit630, fit854}
 where
  profile
    :: forall profile wav es
     . (HeaderKeywords (ProfileInfo profile wav), Error ParseKeyError :> es)
    => ProfileInfo profile wav
    -> WavProfile wav
    -> DataCube [SlitX, Wavelength wav, Stokes]
    -> Eff es (Profile profile wav)
  profile info wprofile image = do
    h <- profileHeader info wprofile image
    pure $ Profile image h

  profileHeader
    :: (HeaderKeywords (ProfileInfo profile wav), Error ParseKeyError :> es)
    => ProfileInfo profile wav
    -> WavProfile wav
    -> DataCube [SlitX, Wavelength wav, Stokes]
    -> Eff es (ProfileHeader profile wav)
  profileHeader info wp image = do
    wcs <- wcsHeader
    common <- dataCommon now image
    pure $ ProfileHeader{common, wcs, info}
   where
    wcsHeader :: (Error ParseKeyError :> es) => Eff es (WCSHeader ProfileAxes)
    wcsHeader = do
      wm <- wcsAxes @WCSMain slice wp l1
      wc <- wcsCommon (isWcsValid wm) l1

      wca <- wcsCommonA l1
      wa <- wcsAxes @A slice wp l1
      pure $ WCSHeader{common = wc, axes = wm, commonA = wca, axesA = wa}

    isWcsValid :: ProfileAxes alt -> Bool
    isWcsValid axs =
      and [isJust axs.dummyY.pcs, isJust axs.slitX.pcs, isJust axs.wavelength.pcs, isJust axs.stokes.pcs]


profileHeaders :: Profiles Profile -> Profiles ProfileHeader
profileHeaders ps =
  Profiles
    { orig630 = ps.orig630.header
    , orig854 = ps.orig854.header
    , fit630 = ps.fit630.header
    , fit854 = ps.fit854.header
    }


profileHDUs
  :: Profiles Profile
  -> [ImageHDU]
profileHDUs ps =
  [ profileHDU ps.orig630
  , profileHDU ps.orig854
  , profileHDU ps.fit630
  , profileHDU ps.fit854
  ]
 where
  profileHDU
    :: (HeaderKeywords (ProfileInfo profile wav))
    => Profile profile wav
    -> ImageHDU
  profileHDU p =
    let darr = encodeDataArray p.image.array
        hd = runPureEff $ writeHeader header
     in ImageHDU{header = Header hd, dataArray = addDummyAxis darr}
   where
    header = do
      sectionHeader "Spectral Profile" "Headers describing the spectral profile"
      addKeywords $ headerKeywords $ DataHeader{common = p.header.common, info = p.header.info}

      sectionHeader "WCS" "WCS Related Keywords"
      addKeywords $ headerKeywords p.header.wcs.common
      addKeywords $ headerKeywords p.header.wcs.axes

      addKeywords $ headerKeywords p.header.wcs.commonA
      addKeywords $ headerKeywords p.header.wcs.axesA


data ProfileAxes alt = ProfileAxes
  { dummyY :: ProfileAxis alt Y
  , slitX :: ProfileAxis alt X
  , wavelength :: ProfileAxis alt Wav
  , stokes :: ProfileAxis alt Stokes
  }
  deriving (Generic)
instance AxisOrder ProfileAxes Y where
  axisN = 4
instance AxisOrder ProfileAxes X where
  axisN = 3
instance AxisOrder ProfileAxes Wav where
  axisN = 2
instance AxisOrder ProfileAxes Stokes where
  axisN = 1
instance (KnownValue alt) => HeaderKeywords (ProfileAxes alt)


data ProfileAxis alt ax = ProfileAxis
  { keys :: WCSAxisKeywords ProfileAxes alt ax
  , pcs :: Maybe (ProfilePCs alt ax)
  }
  deriving (Generic)
instance (KnownValue alt, AxisOrder ProfileAxes ax) => HeaderKeywords (ProfileAxis alt ax)


data ProfilePCs alt ax = ProfilePCs
  { dummyY :: PC ProfileAxes alt ax Y
  , slitX :: PC ProfileAxes alt ax X
  , wavelength :: PC ProfileAxes alt ax Wav
  , stokes :: PC ProfileAxes alt ax Stokes
  }
  deriving (Generic)
instance (KnownValue alt, AxisOrder ProfileAxes ax) => HeaderKeywords (ProfilePCs alt ax)


wcsAxes
  :: forall alt w es
   . (Error ParseKeyError :> es, KnownValue alt)
  => SliceXY
  -> WavProfile w
  -> Header
  -> Eff es (ProfileAxes alt)
wcsAxes s wp h = do
  (ax, ay) <- requireWCSAxes h
  pcsl1 <- requirePCs ax ay h

  yk <- wcsDummyY ay s h
  xk <- wcsSlitX ax s h
  stokes <- wcsStokes
  wavelength <- wcsWavelength wp

  pure $
    ProfileAxes
      { dummyY = ProfileAxis{keys = yk, pcs = pcsY pcsl1}
      , slitX = ProfileAxis{keys = xk, pcs = pcsX pcsl1}
      , stokes
      , wavelength
      }
 where
  pcsY p = do
    guard (isPCsValid p)
    pure $ ProfilePCs{dummyY = p.yy, slitX = p.yx, wavelength = PC 0, stokes = PC 0}

  pcsX p = do
    guard (isPCsValid p)
    pure $ ProfilePCs{dummyY = p.xy, slitX = p.xx, wavelength = PC 0, stokes = PC 0}


lengthSlitX :: DataCube [SlitX, Wavelength w, Stokes] -> Int
lengthSlitX res =
  let Sz (x :> _ :. _) = size res.array
   in x


wcsStokes :: (Monad m) => m (ProfileAxis alt n)
wcsStokes = do
  let crpix = Key 1
      crval = Key 1
      cdelt = Key 1
      cunit = Key ""
      ctype = Key "STOKES"
  let keys = WCSAxisKeywords{..}
  let pcs = ProfilePCs{stokes = PC 1.0, dummyY = PC 0, slitX = PC 0, wavelength = PC 0}
  pure $ ProfileAxis{keys, pcs = Just pcs}


wcsWavelength :: (Monad m) => WavProfile w -> m (ProfileAxis alt n)
wcsWavelength wp = do
  let Wavelength w = midPoint wp.line
  let crpix = Key wp.pixel
      crval = Key (realToFrac w)
      cdelt = Key wp.delta
      cunit = Key "nm"
      ctype = Key "AWAV"
  let keys = WCSAxisKeywords{..}
  let pcs = ProfilePCs{stokes = PC 0.0, dummyY = PC 0, slitX = PC 0, wavelength = PC 1.0}
  pure $ ProfileAxis{keys, pcs = Just pcs}


-- Decoding Profiles ---------------------------------------------------------------------------------------

-- The wavelength data is combined into a single axis, containing both 630 and 854 sections
-- these are both in milliangstroms
data Wavs


-- Milliangstroms
data MA


data Center wl unit


data ProfileFrame profile = ProfileFrame
  { wav630 :: DataCube [SlitX, Wavelength Wav630, Stokes]
  , wav854 :: DataCube [SlitX, Wavelength Wav854, Stokes]
  }


data WavProfile n = WaveProfile
  { pixel :: Float
  , delta :: Float
  , line :: SpectralLine
  }
  deriving (Show, Eq)


newtype WavBreakIndex = WavBreakIndex Int


data ProfileFrames profile = ProfileFrames
  { frames :: [ProfileFrame profile]
  , wavProfiles :: WavProfiles profile
  }


data WavProfiles profile = WavProfiles
  { wav630 :: WavProfile Wav630
  , wav854 :: WavProfile Wav854
  }
  deriving (Show, Eq)


data ProfileFit = ProfileFit
  { profile :: ProfileFrames Fit
  , slice :: SliceXY
  }


decodeProfileFit :: (Error GenerateError :> es) => BS.ByteString -> Eff es ProfileFit
decodeProfileFit inp = do
  f <- decode inp
  profile <- profileFrames f
  slice <- runErrorNoCallStackWith @ParseKeyError (throwError . ParseKeyError) $ requireSlice f.primaryHDU.header
  pure $ ProfileFit{profile, slice}
 where
  requireSlice h = do
    pixelsPerBin <- requireKey "DESR-BIN" toInt h
    pixelBeg <- requireKey "DESR-BEG" toInt h
    pixelEnd <- requireKey "DESR-END" toInt h
    frameBeg <- requireKey "DESR-SC0" toInt h
    frameEnd <- requireKey "DESR-SCN" toInt h
    pure $ SliceXY{pixelsPerBin, pixelBeg, pixelEnd, frameBeg, frameEnd}


decodeProfileOrig :: BS.ByteString -> Eff es (ProfileFrames Original)
decodeProfileOrig inp = do
  f <- decode inp
  profileFrames f


profileFrames :: Fits -> Eff es (ProfileFrames a)
profileFrames f = do
  pro <- mainProfile

  wds <- wavIds
  bx <- indexOfWavBreak wds

  wvs <- wavs
  (w630, w854) <- splitWavs bx wvs
  let wp630 = wavProfile FeI w630
      wp854 = wavProfile (CaII CaII_854) w854

  let pfrs = profileFrameArrays pro
  fs <- mapM (toProfileFrame bx) pfrs

  pure $ ProfileFrames fs (WavProfiles wp630 wp854)
 where
  mainProfile :: (MonadThrow m, MonadCatch m) => m (DataCube [Stokes, Wavs, FrameY, SlitX])
  mainProfile = do
    a <- decodeDataArray @Ix4 @Float f.primaryHDU.dataArray
    pure $ DataCube a

  wavs :: (MonadThrow m, MonadCatch m) => m (DataCube '[Wavs])
  wavs = do
    case f.extensions of
      (Image h : _) -> DataCube <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwM $ MissingProfileExtensions "Wavelength Values"

  wavIds :: (MonadThrow m, MonadCatch m) => m (DataCube '[WavIds])
  wavIds = do
    case f.extensions of
      [_, Image h] -> do
        DataCube <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwM $ MissingProfileExtensions "Wavelength Values"


splitWavs :: (MonadThrow m) => WavBreakIndex -> DataCube '[Wavs] -> m (DataCube '[Wavelength (Center 630 MA)], DataCube '[Wavelength (Center 854 MA)])
splitWavs (WavBreakIndex bx) wvs = do
  (w630, w854) <- splitM0 bx wvs
  pure (centered w630, centered w854)
 where
  centered (DataCube a) = DataCube a


indexOfWavBreak :: (MonadThrow m) => DataCube '[WavIds] -> m WavBreakIndex
indexOfWavBreak wds =
  case group (M.toList wds.array) of
    (w1 : _) -> pure $ WavBreakIndex (length w1)
    _ -> throwM InvalidWavelengthGroups


wavProfile :: SpectralLine -> DataCube '[Wavelength (Center n MA)] -> WavProfile (Center n Nm)
wavProfile l da =
  let DataCube arr = toNanometers da
      ws = M.toList arr
      delta = avgDelta ws
   in WaveProfile
        { delta
        , pixel = pixel0 delta ws
        , line = l
        }
 where
  -- convert from milliangstroms to nanometers, and affirm that the type is correct
  toNanometers :: DataCube '[Wavelength (Center w MA)] -> DataCube '[Wavelength (Center w Nm)]
  toNanometers (DataCube a) = DataCube $ M.map (/ 10000) a


avgDelta :: [Float] -> Float
avgDelta [] = 0
avgDelta ws = round5 $ sum (differences ws) / fromIntegral (length ws - 1)
 where
  differences :: (Num a) => [a] -> [a]
  differences lst = zipWith (-) (drop 1 lst) lst


round5 :: Float -> Float
round5 x = fromIntegral @Int (round $ x * 10 ^ (5 :: Int)) / 10 ^ (5 :: Int)


-- the interpolated pixel offset of a zero value
pixel0 :: Float -> [Float] -> Float
pixel0 dlt as =
  let mn = minimum as
   in negate mn / dlt + 1


profileFrameArrays :: DataCube [Stokes, Wavs, FrameY, SlitX] -> [DataCube [SlitX, Wavs, Stokes]]
profileFrameArrays = fmap swapProfileDimensions . splitFrames


toProfileFrame :: (MonadThrow m) => WavBreakIndex -> DataCube [SlitX, Wavs, Stokes] -> m (ProfileFrame w)
toProfileFrame (WavBreakIndex bx) da = do
  (w630, w854) <- splitM1 bx da
  pure $ ProfileFrame (fromWavs w630) (fromWavs w854)
 where
  -- we can convert from Wavs to (Wavelength n), because there isn't any wavelength data here
  -- it's just one of the pixel axes. And we've successfully split them up here
  fromWavs (DataCube arr) = DataCube arr


swapProfileDimensions :: DataCube [Stokes, Wavs, SlitX] -> DataCube [SlitX, Wavs, Stokes]
swapProfileDimensions =
  transposeMajor . transposeMinor3 . transposeMajor

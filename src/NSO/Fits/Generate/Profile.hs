{-# LANGUAGE RecordWildCards #-}

module NSO.Fits.Generate.Profile where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString qualified as BS
import Data.Massiv.Array (Ix2 (..), IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Data.Maybe (isJust)
import Effectful.Error.Static
import NSO.Data.Spectra (midPoint)
import NSO.Fits.Generate.DataCube
import NSO.Fits.Generate.Error
import NSO.Fits.Generate.Headers
import NSO.Fits.Generate.Headers.Keywords
import NSO.Fits.Generate.Headers.LiftL1
import NSO.Fits.Generate.Headers.Types
import NSO.Fits.Generate.Headers.WCS
import NSO.Fits.Generate.Quantities (DataHDUInfo (..), addDummyAxis, dataSection, splitFrames)
import NSO.Prelude
import NSO.Types.Wavelength (CaIILine (..), Nm, SpectralLine (..), Wavelength (..))
import Telescope.Fits as Fits


-- DONE: add wavelength WCS axis correctly
-- BUG: PC self_self is wrong

data Original
data Fit


-- -- Generating Profiles -------------------------------------------------------------------------------
--
type ProfileInfo ext = DataHDUInfo ext "spect.line.profile" Dimensionless


type OrigProfile630 = ProfileInfo "Original Profile 630.2nm"
type OrigProfile854 = ProfileInfo "Original Profile 854.2nm"
type FitProfile630 = ProfileInfo "Fit Profile 630.2nm"
type FitProfile854 = ProfileInfo "Fit Profile 854.2nm"


profileHDUs
  :: (Error LiftL1Error :> es)
  => UTCTime
  -> Header
  -> WavProfiles Original
  -> WavProfiles Fit
  -> ProfileFrame Original
  -> ProfileFrame Fit
  -> Eff es [ImageHDU]
profileHDUs now l1 wpo wpf po pf = do
  sequence [orig630, orig854, fit630, fit854]
 where
  orig630 = profileHDU @OrigProfile630 now l1 DataHDUInfo wpo.wav630 po.wav630
  orig854 = profileHDU @OrigProfile854 now l1 DataHDUInfo wpo.wav854 po.wav854
  fit630 = profileHDU @FitProfile630 now l1 DataHDUInfo wpf.wav630 pf.wav630
  fit854 = profileHDU @FitProfile854 now l1 DataHDUInfo wpf.wav854 pf.wav854


profileHDU
  :: (HeaderKeywords info, Error LiftL1Error :> es)
  => UTCTime
  -> Header
  -> info
  -> WavProfile w
  -> DataCube [SlitX, Wavelength w, Stokes]
  -> Eff es ImageHDU
profileHDU now l1 info wp da = do
  let darr = encodeArray da.array
  hd <- writeHeader header
  pure ImageHDU{header = Header hd, dataArray = addDummyAxis darr}
 where
  header = do
    sectionHeader "Spectral Profile" "Headers describing the spectral profile"
    dataSection now info da

    sectionHeader "WCS" "WCS Related Keywords"
    wcsSection

  wcsSection = do
    wm <- wcsAxes @WCSMain binnedX wp l1
    wc <- wcsCommon (isWcsValid wm) l1

    addKeywords $ headerKeywords wc
    addKeywords $ headerKeywords wm

    wca <- wcsCommonA l1
    wa <- wcsAxes @A binnedX wp l1
    addKeywords $ headerKeywords wca
    addKeywords $ headerKeywords wa

  binnedX =
    let (Sz (newx :> _)) = size da.array
     in BinnedX newx

  isWcsValid :: ProfileAxes alt -> Bool
  isWcsValid axs =
    and [isJust axs.dummyY.pcs, isJust axs.slitX.pcs, isJust axs.wavelength.pcs, isJust axs.stokes.pcs]


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
   . (Error LiftL1Error :> es, KnownValue alt)
  => BinnedX
  -> WavProfile w
  -> Header
  -> Eff es (ProfileAxes alt)
wcsAxes bx wp h = do
  (ax, ay) <- requireWCSAxes h
  pcsl1 <- requirePCs ax ay h

  yk <- wcsDummyY ay h
  xk <- wcsSlitX ax bx h
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


data ProfileFrame a = ProfileFrame
  { wav630 :: DataCube [SlitX, Wavelength (Center 630 Nm), Stokes]
  , wav854 :: DataCube [SlitX, Wavelength (Center 854 Nm), Stokes]
  }


data WavProfile n = WaveProfile
  { pixel :: Float
  , delta :: Float
  , line :: SpectralLine
  }
  deriving (Show, Eq)


newtype WavBreakIndex = WavBreakIndex Int


data ProfileFrames a = ProfileFrames
  { frames :: [ProfileFrame a]
  , wavProfiles :: WavProfiles a
  }


data WavProfiles a = WavProfiles
  { wav630 :: WavProfile (Center 630 Nm)
  , wav854 :: WavProfile (Center 854 Nm)
  }
  deriving (Show, Eq)


decodeProfileFrames :: forall a m. (MonadThrow m) => BS.ByteString -> m (ProfileFrames a)
decodeProfileFrames inp = do
  f <- decode inp
  pro <- mainProfile f

  wds <- wavIds f
  bx <- indexOfWavBreak wds

  wvs <- wavs f
  (w630, w854) <- splitWavs bx wvs
  let wp630 = wavProfile FeI w630
      wp854 = wavProfile (CaII CaII_854) w854

  let pfrs = profileFrameArrays pro
  fs <- mapM (toProfileFrame bx) pfrs

  pure $ ProfileFrames fs (WavProfiles wp630 wp854)
 where
  mainProfile :: (MonadThrow m) => Fits -> m (DataCube [Stokes, Wavs, FrameY, SlitX])
  mainProfile f = do
    a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
    pure $ DataCube a

  wavs :: (MonadThrow m) => Fits -> m (DataCube '[Wavs])
  wavs f = do
    case f.extensions of
      (Image h : _) -> DataCube <$> decodeArray @Ix1 h.dataArray
      _ -> throwM $ MissingProfileExtensions "Wavelength Values"

  wavIds :: (MonadThrow m) => Fits -> m (DataCube '[WavIds])
  wavIds f = do
    case f.extensions of
      [_, Image h] -> do
        DataCube <$> decodeArray @Ix1 h.dataArray
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

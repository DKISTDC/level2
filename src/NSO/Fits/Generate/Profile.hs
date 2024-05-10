{-# LANGUAGE RecordWildCards #-}

module NSO.Fits.Generate.Profile where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString qualified as BS
import Data.Massiv.Array (Ix2 (..), IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Effectful.Error.Static
import GHC.TypeLits (KnownNat (..))
import NSO.Fits.Generate.DimArray
import NSO.Fits.Generate.Headers
import NSO.Fits.Generate.Headers.Keywords
import NSO.Fits.Generate.Headers.LiftL1
import NSO.Fits.Generate.Headers.Types
import NSO.Fits.Generate.Headers.WCS
import NSO.Fits.Generate.Quantities (DataHDUInfo (..), addDummyAxis, dataSection, splitFrames)
import NSO.Prelude
import NSO.Types.Wavelength (Wavelength)
import Telescope.Fits as Fits
import Prelude (truncate)


-- BUG: generating profiles is slow. Or reading them? Unsure

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
profileHDUs now l1 wpo wpf po pf =
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
  -> DimArray [SlitX, Wavelength w, Stokes]
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
    wc <- wcsCommon l1
    wm <- wcsAxes @WCSMain wp da l1
    wa <- wcsAxes @A wp da l1
    addKeywords $ headerKeywords wc
    addKeywords $ headerKeywords wm
    addKeywords $ headerKeywords wa


data ProfileAxes f = ProfileAxes
  { dummyY :: f 4
  , slitX :: f 3
  , wavelength :: f 2
  , stokes :: f 1
  }
  deriving (Generic)
instance (KnownValue alt, KnownNat n) => HeaderKeywords (ProfileAxes (PC alt n))
instance (KnownValue alt) => HeaderKeywords (ProfileAxes (ProfileAxis alt)) where
  headerKeywords a =
    headerKeywords @(ProfileAxis alt 1) a.stokes
      <> headerKeywords @(ProfileAxis alt 2) a.wavelength
      <> headerKeywords @(ProfileAxis alt 3) a.slitX
      <> headerKeywords @(ProfileAxis alt 4) a.dummyY


data ProfileAxis alt n = ProfileAxis
  { keys :: WCSAxisKeywords alt n
  , pcs :: ProfileAxes (PC alt n)
  }
instance (KnownValue alt, KnownNat n) => HeaderKeywords (ProfileAxis alt n) where
  headerKeywords a =
    headerKeywords a.keys <> headerKeywords a.pcs


wcsAxes :: forall alt w es. (Error LiftL1Error :> es, KnownValue alt) => WavProfile w -> DimArray [SlitX, Wavelength w, Stokes] -> Header -> Eff es (ProfileAxes (ProfileAxis alt))
wcsAxes wp sz h = do
  yk <- wcsDummyYKeys h
  (ypy, ypx) <- wcsDummyYPCs @alt h
  let dummyY = ProfileAxis{keys = yk, pcs = ProfileAxes{dummyY = ypy, slitX = ypx, wavelength = PC 0, stokes = PC 0}}

  xk <- wcsSlitXKeys (lengthSlitX sz) h
  (xpy, xpx) <- wcsSlitXPCs @alt h
  let slitX = ProfileAxis{keys = xk, pcs = ProfileAxes{dummyY = xpy, slitX = xpx, wavelength = PC 0, stokes = PC 0}}

  stokes <- wcsStokes
  wavelength <- wcsWavelength wp

  pure $ ProfileAxes{..}


lengthSlitX :: DimArray [SlitX, Wavelength w, Stokes] -> Int
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
  let pcs = ProfileAxes{stokes = PC 1.0, dummyY = PC 0, slitX = PC 0, wavelength = PC 0}
  pure $ ProfileAxis{keys, pcs}


wcsWavelength :: (Monad m) => WavProfile w -> m (ProfileAxis alt n)
wcsWavelength wp = do
  let crpix = Key 0
      crval = Key 0
      cdelt = Key 0
      cunit = Key "nm"
      ctype = Key "AWAV"
  let keys = WCSAxisKeywords{..}
  let pcs = ProfileAxes{stokes = PC 1.0, dummyY = PC 0, slitX = PC 0, wavelength = PC 0}
  pure $ ProfileAxis{keys, pcs}


-- Decoding Profiles ---------------------------------------------------------------------------------------

data ProfileFrame a = ProfileFrame
  { wav630 :: DimArray [SlitX, Wavelength 630, Stokes]
  , wav854 :: DimArray [SlitX, Wavelength 854, Stokes]
  }


data WavProfile n = WaveProfile
  { pixel :: Float
  , delta :: Float
  }


newtype WavBreakIndex = WavBreakIndex Int


data ProfileFrames a = ProfileFrames
  { frames :: [ProfileFrame a]
  , wavProfiles :: WavProfiles a
  }


data WavProfiles a = WavProfiles
  { wav630 :: WavProfile 630
  , wav854 :: WavProfile 854
  }


decodeProfileFrames :: forall a m. (MonadThrow m) => BS.ByteString -> m (ProfileFrames a)
decodeProfileFrames inp = do
  f <- decode inp
  pro <- mainProfile f

  wds <- wavIds f
  bx <- indexOfWavBreak wds

  (w630, w854) <- wavs bx f
  let wp630 = wavProfile w630
      wp854 = wavProfile w854

  let pfrs = profileFrameArrays pro
  fs <- mapM (toProfileFrame bx) pfrs

  pure $ ProfileFrames fs (WavProfiles wp630 wp854)
 where
  -- mapM (fmap (uncurry ProfileFrame) . splitResultsAlongWavelength bx) $ profileFrames pro

  mainProfile :: (MonadThrow m) => Fits -> m (DimArray [Stokes, Wavs, FrameY, SlitX])
  mainProfile f = do
    a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
    pure $ DimArray a

  indexOfWavBreak :: DimArray '[WavIds] -> m WavBreakIndex
  indexOfWavBreak wds =
    case group (M.toList wds.array) of
      (w1 : _) -> pure $ WavBreakIndex (length w1)
      _ -> throwM InvalidWavelengthGroups

  wavs :: (MonadThrow m) => WavBreakIndex -> Fits -> m (DimArray '[Wavelength 630], DimArray '[Wavelength 854])
  wavs (WavBreakIndex bx) f = do
    case f.extensions of
      (Image h : _) -> do
        wvs <- DimArray <$> decodeArray @Ix1 h.dataArray

        -- WARNING: wvs is in milliangstroms
        -- TODO: convert to nm

        splitM0 bx wvs
      _ -> throwM $ MissingProfileExtensions "Wavelength Values"

  wavIds :: (MonadThrow m) => Fits -> m (DimArray '[WavIds])
  wavIds f = do
    case f.extensions of
      [_, Image h] -> do
        DimArray <$> decodeArray @Ix1 h.dataArray
      _ -> throwM $ MissingProfileExtensions "Wavelength Values"


wavProfile :: DimArray '[Wavelength n] -> WavProfile n
wavProfile (DimArray arr) =
  let ws = M.toList arr
      delta = avgDelta ws
   in WaveProfile
        { delta
        , pixel = pixel0 delta ws
        }


avgDelta :: [Float] -> Float
avgDelta ws = round5 $ sum (differences ws) / fromIntegral (length ws - 1)
 where
  differences :: (Num a) => [a] -> [a]
  differences lst = zipWith (-) (tail lst) lst

  round5 :: Float -> Float
  round5 x = fromIntegral @Int (truncate $ x * 10 ^ (5 :: Int)) / 10 ^ (5 :: Int)


pixel0 :: Float -> [Float] -> Float
pixel0 dlt as =
  let mn = minimum as
   in negate mn / dlt + 1


profileFrameArrays :: DimArray [Stokes, Wavs, FrameY, SlitX] -> [DimArray [SlitX, Wavs, Stokes]]
profileFrameArrays = fmap swapProfileDimensions . splitFrames


toProfileFrame :: (MonadThrow m) => WavBreakIndex -> DimArray [SlitX, Wavs, Stokes] -> m (ProfileFrame a)
toProfileFrame (WavBreakIndex bx) =
  fmap (uncurry ProfileFrame) <$> splitM1 bx


swapProfileDimensions :: DimArray [Stokes, Wavs, SlitX] -> DimArray [SlitX, Wavs, Stokes]
swapProfileDimensions =
  transposeMajor . transposeMinor3 . transposeMajor

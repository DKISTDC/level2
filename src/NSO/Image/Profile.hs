{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NSO.Image.Profile where

import Control.Exception (Exception)
import Data.ByteString qualified as BS
import Data.Massiv.Array (Ix2 (..), IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Data.Maybe (isJust)
import Effectful
import Effectful.Error.Static
import NSO.Data.Spectra (midPoint)
import NSO.Image.Headers
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Image.Headers.WCS
import NSO.Image.NDCollection (AlignedAxes)
import NSO.Image.Quantity (DataCommon (..), DataHDUInfo (..), DataHeader (..), addDummyAxis, dataCommon, splitFrames)
import NSO.Prelude
import NSO.Types.Wavelength (CaIILine (..), Nm, SpectralLine (..), Wavelength (..))
import Telescope.Asdf (ToAsdf (..))
import Telescope.Data.Axes (AxisOrder (..))
import Telescope.Data.DataCube
import Telescope.Data.KnownText
import Telescope.Data.WCS
import Telescope.Fits as Fits
import Telescope.Fits.Header (Header (..), HeaderRecord (..))


-- BUG: PC self_self is wrong
--  ?? Is this still an issue?

-- Generating Profiles -------------------------------------------------------------------------------

data Original
data Fit
type Wav630 = Center 630 Nm
type Wav854 = Center 854 Nm


instance KnownText Original where
  knownText = "Original"
instance KnownText Fit where
  knownText = "Fit"


type ProfileInfo' ext = DataHDUInfo ext "spect.line.profile" Dimensionless


type Orig630 = ProfileInfo' "Original Profile 630.2nm"
type Orig854 = ProfileInfo' "Original Profile 854.2nm"
type Fit630 = ProfileInfo' "Fit Profile 630.2nm"
type Fit854 = ProfileInfo' "Fit Profile 854.2nm"


instance KnownText Orig630 where
  knownText = "Orig630"
instance KnownText Orig854 where
  knownText = "Orig854"
instance KnownText Fit630 where
  knownText = "Fit630"
instance KnownText Fit854 where
  knownText = "Fit854"


class ProfileInfo info where
  type ProfileWav info :: Type
  type ProfileType info :: Type
  profileWav :: Wavelength Nm


instance ProfileInfo Orig630 where
  type ProfileWav Orig630 = Wav630
  type ProfileType Orig630 = Original
  profileWav = Wavelength 630.2


instance ProfileInfo Orig854 where
  type ProfileWav Orig854 = Wav854
  type ProfileType Orig854 = Original
  profileWav = Wavelength 854.2


instance ProfileInfo Fit630 where
  type ProfileWav Fit630 = Wav630
  type ProfileType Fit630 = Fit
  profileWav = Wavelength 630.2


instance ProfileInfo Fit854 where
  type ProfileWav Fit854 = Wav854
  type ProfileType Fit854 = Fit
  profileWav = Wavelength 854.2


data Profiles (f :: Type -> Type) = Profiles
  { orig630 :: f Orig630
  , orig854 :: f Orig854
  , fit630 :: f Fit630
  , fit854 :: f Fit854
  }
  deriving (Generic)
instance ToAsdf (Profiles AlignedAxes)


data ProfileHeader info = ProfileHeader
  { info :: info
  , common :: DataCommon
  , wcs :: WCSHeader ProfileAxes
  }
  deriving (Generic)
instance (ToHeader info) => ToHeader (ProfileHeader info) where
  toHeader h = writeHeader $ do
    sectionHeader "Spectral Profile" "Headers describing the spectral profile"
    addKeywords $ DataHeader{common = h.common, info = h.info}

    sectionHeader "WCS" "WCS Related Keywords"
    addKeywords h.wcs.common
    addKeywords h.wcs.axes

    addKeywords h.wcs.commonA
    addKeywords h.wcs.axesA


data Profile info = Profile
  { image :: DataCube [SlitX, Wavelength (ProfileWav info), Stokes] Float
  , header :: ProfileHeader info
  }


profiles
  :: (Error ProfileError :> es)
  => SliceXY
  -> UTCTime
  -> Header
  -> WavProfiles Original
  -> WavProfiles Fit
  -> ProfileFrame Original
  -> ProfileFrame Fit
  -> Eff es (Profiles Profile)
profiles slice now l1 wpo wpf po pf = do
  orig630 <- profile @Orig630 DataHDUInfo wpo.wav630 po.wav630
  orig854 <- profile @Orig854 DataHDUInfo wpo.wav854 po.wav854
  fit630 <- profile @Fit630 DataHDUInfo wpf.wav630 pf.wav630
  fit854 <- profile @Fit854 DataHDUInfo wpf.wav854 pf.wav854
  pure $ Profiles{orig630, orig854, fit630, fit854}
 where
  profile
    :: forall info wav es
     . (ToHeader info, wav ~ ProfileWav info, Error ProfileError :> es)
    => info
    -> WavProfile wav
    -> DataCube [SlitX, Wavelength wav, Stokes] Float
    -> Eff es (Profile info)
  profile info wprofile image = do
    common <- dataCommon now image
    wcs <- wcsHeader wprofile slice l1
    pure $ Profile image $ ProfileHeader{common, wcs, info}


wcsHeader :: (Error ProfileError :> es) => WavProfile wav -> SliceXY -> Header -> Eff es (WCSHeader ProfileAxes)
wcsHeader wp slice l1 = runParseError InvalidWCS $ do
  wm <- wcsAxes @WCSMain slice wp l1
  wc <- wcsCommon (isWcsValid wm) l1

  wca <- wcsCommonA l1
  wa <- wcsAxes @A slice wp l1
  pure $ WCSHeader{common = wc, axes = wm, commonA = wca, axesA = wa}
 where
  isWcsValid :: ProfileAxes alt -> Bool
  isWcsValid axs =
    and [isJust axs.dummyY.pcs, isJust axs.slitX.pcs, isJust axs.wavelength.pcs, isJust axs.stokes.pcs]


profileHDUs
  :: Profiles Profile
  -> [DataHDU]
profileHDUs ps =
  [ profileHDU ps.orig630
  , profileHDU ps.orig854
  , profileHDU ps.fit630
  , profileHDU ps.fit854
  ]
 where
  profileHDU
    :: (ToHeader info)
    => Profile info
    -> DataHDU
  profileHDU p =
    let darr = encodeDataArray p.image.array
     in DataHDU{header = toHeader p.header, dataArray = addDummyAxis darr}


data ProfileAxes alt = ProfileAxes
  { dummyY :: ProfileAxis alt Y
  , slitX :: ProfileAxis alt X
  , wavelength :: ProfileAxis alt Wav
  , stokes :: ProfileAxis alt Stokes
  }
  deriving (Generic)
instance AxisOrder (HDUAxis ProfileAxes Y) where
  axisN = 4
instance AxisOrder (HDUAxis ProfileAxes X) where
  axisN = 3
instance AxisOrder (HDUAxis ProfileAxes Wav) where
  axisN = 2
instance AxisOrder (HDUAxis ProfileAxes Stokes) where
  axisN = 1
instance (KnownText alt) => ToHeader (ProfileAxes alt) where
  toHeader (ProfileAxes y x w s) =
    mconcat [toHeader y, toHeader x, toHeader w, toHeader s]


data ProfileAxis alt ax = ProfileAxis
  { keys :: WCSAxisKeywords ProfileAxes alt ax
  , pcs :: Maybe (ProfilePCs alt ax)
  }
  deriving (Generic)
instance (KnownText alt, AxisOrder (HDUAxis ProfileAxes ax)) => ToHeader (ProfileAxis alt ax) where
  toHeader pa = toHeader (toWCSAxis @(HDUAxis ProfileAxes ax) pa.keys) <> toHeader pa.pcs


data ProfilePCs alt ax = ProfilePCs
  { dummyY :: PC ProfileAxes alt ax Y
  , slitX :: PC ProfileAxes alt ax X
  , wavelength :: PC ProfileAxes alt ax Wav
  , stokes :: PC ProfileAxes alt ax Stokes
  }
  deriving (Generic)
instance (KnownText alt, AxisOrder (HDUAxis ProfileAxes ax)) => ToHeader (ProfilePCs alt ax) where
  toHeader pcs =
    Header
      [ Keyword $ keywordRecord pcs.dummyY
      , Keyword $ keywordRecord pcs.slitX
      , Keyword $ keywordRecord pcs.wavelength
      , Keyword $ keywordRecord pcs.stokes
      ]


wcsAxes
  :: forall alt w es
   . (Error ParseError :> es, KnownText alt)
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


lengthSlitX :: DataCube [SlitX, Wavelength w, Stokes] Float -> Int
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
  { wav630 :: DataCube [SlitX, Wavelength Wav630, Stokes] Float
  , wav854 :: DataCube [SlitX, Wavelength Wav854, Stokes] Float
  }


data WavProfile n = WavProfile
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


decodeProfileFit :: (Error ProfileError :> es) => BS.ByteString -> Eff es (ProfileFrames Fit)
decodeProfileFit inp = do
  f <- decode inp
  profileFrames f


decodeProfileOrig :: (Error ProfileError :> es) => BS.ByteString -> Eff es (ProfileFrames Original)
decodeProfileOrig inp = do
  f <- decode inp
  profileFrames f


profileFrames :: (Error ProfileError :> es) => Fits -> Eff es (ProfileFrames a)
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
  mainProfile :: Eff es (DataCube [Stokes, Wavs, FrameY, SlitX] Float)
  mainProfile = do
    a <- decodeDataArray @Ix4 @Float f.primaryHDU.dataArray
    pure $ DataCube a

  wavs :: (Error ProfileError :> es) => Eff es (DataCube '[Wavs] Float)
  wavs = do
    case f.extensions of
      -- the first extensions
      (Image h : _) -> DataCube <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwError $ MissingProfileExtensions "Wavelength Values"

  wavIds :: (Error ProfileError :> es) => Eff es (DataCube '[WavIds] Float)
  wavIds = do
    case f.extensions of
      -- should be the second extension
      [_, Image h] -> do
        DataCube <$> decodeDataArray @Ix1 h.dataArray
      _ -> throwError $ MissingProfileExtensions "Wavelength Ids"


splitWavs :: WavBreakIndex -> DataCube '[Wavs] Float -> Eff es (DataCube '[Wavelength (Center 630 MA)] Float, DataCube '[Wavelength (Center 854 MA)] Float)
splitWavs (WavBreakIndex bx) wvs = do
  (w630, w854) <- splitM0 bx wvs
  pure (centered w630, centered w854)
 where
  centered (DataCube a) = DataCube a


indexOfWavBreak :: (Error ProfileError :> es) => DataCube '[WavIds] Float -> Eff es WavBreakIndex
indexOfWavBreak wds =
  case group (M.toList wds.array) of
    (w1 : _) -> pure $ WavBreakIndex (length w1)
    _ -> throwError InvalidWavelengthGroups


wavProfile :: SpectralLine -> DataCube '[Wavelength (Center n MA)] Float -> WavProfile (Center n Nm)
wavProfile l da =
  let DataCube arr = toNanometers da
      ws = M.toList arr
      delta = avgDelta ws
   in WavProfile
        { delta
        , pixel = pixel0 delta ws
        , line = l
        }
 where
  -- convert from milliangstroms to nanometers, and affirm that the type is correct
  toNanometers :: DataCube '[Wavelength (Center w MA)] Float -> DataCube '[Wavelength (Center w Nm)] Float
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


profileFrameArrays :: DataCube [Stokes, Wavs, FrameY, SlitX] Float -> [DataCube [SlitX, Wavs, Stokes] Float]
profileFrameArrays = fmap swapProfileDimensions . splitFrames


toProfileFrame :: WavBreakIndex -> DataCube [SlitX, Wavs, Stokes] Float -> Eff es (ProfileFrame w)
toProfileFrame (WavBreakIndex bx) da = do
  (w630, w854) <- splitM1 bx da
  pure $ ProfileFrame (fromWavs w630) (fromWavs w854)
 where
  -- we can convert from Wavs to (Wavelength n), because there isn't any wavelength data here
  -- it's just one of the pixel axes. And we've successfully split them up here
  fromWavs (DataCube arr) = DataCube arr


swapProfileDimensions :: DataCube [Stokes, Wavs, SlitX] Float -> DataCube [SlitX, Wavs, Stokes] Float
swapProfileDimensions =
  transposeMajor . transposeMinor3 . transposeMajor


mapProfiles :: (forall x. f x -> g x) -> Profiles f -> Profiles g
mapProfiles f ps =
  Profiles
    { orig630 = f ps.orig630
    , orig854 = f ps.orig854
    , fit630 = f ps.fit630
    , fit854 = f ps.fit854
    }


profilesFrom :: (forall x. a -> f x) -> a -> Profiles f
profilesFrom val a =
  Profiles
    { orig630 = val a
    , orig854 = val a
    , fit630 = val a
    , fit854 = val a
    }


data ProfileError
  = InvalidWavelengthGroups
  | MissingProfileExtensions String
  | InvalidWCS ParseError
  deriving (Show, Exception, Eq)

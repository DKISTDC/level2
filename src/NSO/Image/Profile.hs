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
import NSO.Image.Asdf.NDCollection (AlignedAxes)
import NSO.Image.Blanca
import NSO.Image.Headers
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Image.Headers.WCS
import NSO.Image.Quantity (DataCommon (..), DataHDUInfo (..), DataHeader (..), addDummyAxis, dataCommon, splitFrameY)
import NSO.Image.Types.Profile (ProfileType)
import NSO.Image.Types.VISPArm
import NSO.Prelude
import NSO.Types.Wavelength (CaIILine (..), Nm, SpectralLine (..), Wavelength (..))
import Telescope.Asdf (ToAsdf (..))
import Telescope.Data.Axes (AxisOrder (..))
import Telescope.Data.DataCube
import Telescope.Data.KnownText
import Telescope.Data.WCS
import Telescope.Fits as Fits
import Telescope.Fits.Header (Header (..), HeaderRecord (..), ToKeyword (..))


-- BUG: PC self_self is wrong
--  ?? Is this still an issue?

-- Generating Profiles -------------------------------------------------------------------------------

instance (KnownText fit) => ToHeader (WavMeta fit) where
  toHeader meta =
    Header $
      fmap
        Keyword
        -- TODO: ext name based on.... the dynamic info of the profile info
        [ KeywordRecord (keyword @(ExtName "")) (String extName) Nothing
        , keywordRecord @(BType "spect.line.profile") BType
        , keywordRecord @(BUnit Dimensionless) BUnit
        ]
   where
    extName = knownText @fit <> " Profile " <> cs (show meta.line)


-- type family ProfileInfo arm fit where
--   ProfileInfo ArmFeI Original = ProfileInfo' "Original Profile FeI"
--   ProfileInfo ArmFeI Fit = ProfileInfo' "Fit Profile FeI"
--   ProfileInfo ArmCa854 Original = ProfileInfo' "Original Profile CaII 854"
--   ProfileInfo ArmCa854 Fit = ProfileInfo' "Fit Profile CaII 854"
--   ProfileInfo ArmNaD Original = ProfileInfo' "Original Profile NaD"
--   ProfileInfo ArmNaD Fit = ProfileInfo' "Fit Profile NaD"

-- type Orig854 = ProfileInfo' "Original Profile 854.2nm"
-- type Fit630 = ProfileInfo' "Fit Profile 630.2nm"
-- type Fit854 = ProfileInfo' "Fit Profile 854.2nm"

-- instance KnownText Orig630 where
--   knownText = "Orig630"
-- instance KnownText Orig854 where
--   knownText = "Orig854"
-- instance KnownText Fit630 where
--   knownText = "Fit630"
-- instance KnownText Fit854 where
--   knownText = "Fit854"

-- class ProfileInfo wav origFit where
--   profileWav :: Wavelength Nm
--   profileInfo :: DataHDUInfo
--
--
-- instance ProfileInfo Wav630 Original where
--   profileWav = Wavelength 630.2
--   profileInfo =
--
--
-- instance ProfileInfo Wav630 Fit where
--   profileWav = Wavelength 630.2
--   profileInfo = ProfileInfo' "Fit Profile 630.2nm"
--
--
-- instance ProfileInfo Orig854 where
--   profileWav = Wavelength 854.2

--
--
--
-- instance ProfileInfo Fit854 where
--   type ProfileWav Fit854 = Wav854
--   type ProfileType Fit854 = Fit
--   profileWav = Wavelength 854.2

-- data Profiles (f :: Type -> Type) = Profiles
--   { orig630 :: f Orig630
--   , orig854 :: f Orig854
--   , fit630 :: f Fit630
--   , fit854 :: f Fit854
--   }
--   deriving (Generic)
-- instance ToAsdf (Profiles AlignedAxes)

-- data ArmProfiles arm = ArmProfiles
--   { fit :: Profile arm Fit
--   , orig :: Profile arm Original
--   }

-- -- a single profile, fit or original
-- data Profile arm fit = Profile
--   { image :: DataCube [SlitX, Wavelength arm, Stokes] Float
--   , header :: ProfileHeader arm
--   }

data ProfileHeader (fit :: ProfileType) = ProfileHeader
  { meta :: WavMeta fit
  , common :: DataCommon
  , wcs :: WCSHeader ProfileAxes
  }
  deriving (Generic)
instance (KnownText fit) => ToHeader (ProfileHeader fit) where
  toHeader h = writeHeader $ do
    sectionHeader "Spectral Profile" "Headers describing the spectral profile"
    addKeywords $ DataHeader{common = h.common, info = h.meta}

    sectionHeader "WCS" "WCS Related Keywords"
    addKeywords h.wcs.common
    addKeywords h.wcs.axes

    addKeywords h.wcs.commonA
    addKeywords h.wcs.axesA


-- armProfiles
--   :: (Error ProfileError :> es)
--   => SliceXY
--   -> UTCTime
--   -> Header
--   -> VISPArms (WavProfile Original)
--   -> VISPArms (WavProfile Fit)
--   -> ProfileFrame Original
--   -> ProfileFrame Fit
--   -> Eff es (VISPArms ArmProfiles)
-- armProfiles slice now l1 wpo wpf po pf = do
--   -- orig630 <- profile @Orig630 DataHDUInfo wpo.wav630 po.wav630
--   -- orig854 <- profile @Orig854 DataHDUInfo wpo.wav854 po.wav854
--   -- fit630 <- profile @Fit630 DataHDUInfo wpf.wav630 pf.wav630
--   -- fit854 <- profile @Fit854 DataHDUInfo wpf.wav854 pf.wav854
--   -- pure $ Profiles{orig630, orig854, fit630, fit854}
--   pure _
--  where
--   armProfiles info = _
--
--   armProfile
--     :: forall arm fit hduInfo es
--      . (hduInfo ~ ProfileInfo arm fit, ToHeader hduInfo, Error ProfileError :> es)
--     => hduInfo
--     -> WavProfile arm
--     -> DataCube [SlitX, Wavelength arm, Stokes] Float
--     -> Eff es (Profile arm fit)
--   armProfile info wprofile image = do
--     common <- dataCommon now image
--     wcs <- wcsHeader wprofile slice l1
--     pure $ Profile image $ ProfileHeader{common, wcs, info}

wcsHeader :: (Error ProfileError :> es) => WavMeta fit -> SliceXY -> Header -> Eff es (WCSHeader ProfileAxes)
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
  :: forall (fit :: ProfileType) es
   . (KnownText fit, Error ProfileError :> es)
  => UTCTime
  -> Header
  -> SliceXY
  -> Arms (ProfileFrame fit)
  -> Eff es [DataHDU]
profileHDUs now l1 slice (Arms arms) = do
  mapM profileHDU arms
 where
  profileHDU :: ProfileFrame fit -> Eff es DataHDU
  profileHDU p = do
    let darr = encodeDataArray p.image.array
    header <- toHeader <$> profileHeader p
    pure $ DataHDU{header, dataArray = addDummyAxis darr}

  profileHeader
    :: (Error ProfileError :> es)
    => ProfileFrame fit
    -> Eff es (ProfileHeader fit)
  profileHeader frame = do
    common <- dataCommon now frame.image
    wcs <- wcsHeader frame.meta slice l1
    pure $ ProfileHeader{common, wcs, meta = frame.meta}


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
  :: forall alt fit es
   . (Error ParseError :> es, KnownText alt)
  => SliceXY
  -> WavMeta fit
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


-- lengthSlitX :: DataCube [SlitX, Wavelength w, Stokes] Float -> Int
-- lengthSlitX res =
--   let Sz (x :> _ :. _) = size res.array
--    in x

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


wcsWavelength :: (Monad m) => WavMeta fit -> m (ProfileAxis alt n)
wcsWavelength wp = do
  let Wavelength w = midPoint wp.line
  let crpix = Key $ realToFrac wp.pixel
      crval = Key (realToFrac w)
      cdelt = Key $ realToFrac wp.delta
      cunit = Key "nm"
      ctype = Key "AWAV"
  let keys = WCSAxisKeywords{..}
  let pcs = ProfilePCs{stokes = PC 0.0, dummyY = PC 0, slitX = PC 0, wavelength = PC 1.0}
  pure $ ProfileAxis{keys, pcs = Just pcs}


-- mapProfiles :: (forall x. f x -> g x) -> Profiles f -> Profiles g
-- mapProfiles f ps =
--   Profiles
--     { orig630 = f ps.orig630
--     , orig854 = f ps.orig854
--     , fit630 = f ps.fit630
--     , fit854 = f ps.fit854
--     }
--
--
-- profilesFrom :: (forall x. a -> f x) -> a -> Profiles f
-- profilesFrom val a =
--   Profiles
--     { orig630 = val a
--     , orig854 = val a
--     , fit630 = val a
--     , fit854 = val a
--     }

data ProfileError
  = InvalidWavelengthGroups
  | MissingProfileExtensions String
  | InvalidWCS ParseError
  deriving (Show, Exception, Eq)

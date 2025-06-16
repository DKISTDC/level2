{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NSO.Image.Fits.Profile where

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
import NSO.Image.Fits.Quantity (addDummyAxis, dataCommon)
import NSO.Image.Headers
import NSO.Image.Headers.DataCommon
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Image.Headers.WCS
import NSO.Image.Types.Profile
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


-- Profile FITS Generation -----------------------------------------------------------

data ProfileFrameFits (fit :: ProfileType) = ProfileFrameFits
  { header :: ProfileHeader fit
  , image :: ProfileImage fit
  }


profiles
  :: forall es
   . (Error ProfileError :> es)
  => SliceXY
  -> UTCTime
  -> Header
  -> Arms (Profile ProfileImage)
  -> Eff es (Arms (Profile ProfileFrameFits))
profiles slice now l1 = mapM profile
 where
  profile pair = do
    fit <- profileFrameFits pair.fit
    original <- profileFrameFits pair.original
    pure $ Profile{fit, original}

  profileFrameFits :: ProfileImage fit -> Eff es (ProfileFrameFits fit)
  profileFrameFits frame = do
    h <- profileHeader now slice l1 frame
    pure $ ProfileFrameFits h frame


profileHDUs :: Arms (Profile ProfileFrameFits) -> [DataHDU]
profileHDUs (Arms arms) = mconcat $ fmap pairHDUs arms
 where
  pairHDUs :: Profile ProfileFrameFits -> [DataHDU]
  pairHDUs pair =
    [ profileHDU pair.original
    , profileHDU pair.fit
    ]

  profileHDU :: (KnownText fit) => ProfileFrameFits fit -> DataHDU
  profileHDU p =
    let darr = encodeDataArray p.image.data_.array
        header = toHeader p.header
     in DataHDU{header, dataArray = addDummyAxis darr}


profileHeader
  :: (Error ProfileError :> es)
  => UTCTime
  -> SliceXY
  -> Header
  -> ProfileImage fit
  -> Eff es (ProfileHeader fit)
profileHeader now slice l1 frame = do
  common <- dataCommon now frame.data_
  wcs <- wcsHeader frame.arm slice l1
  pure $ ProfileHeader{common, wcs, meta = frame.arm}


data ProfileHeader (fit :: ProfileType) = ProfileHeader
  { meta :: ArmWavMeta fit
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


instance (KnownText fit) => ToHeader (ArmWavMeta fit) where
  toHeader meta =
    Header $
      fmap
        Keyword
        [ KeywordRecord (keyword @(ExtName "")) (String extName) Nothing
        , keywordRecord @(BType "spect.line.profile") BType
        , keywordRecord @(BUnit Dimensionless) BUnit
        ]
   where
    extName :: Text
    extName = knownText @fit <> " Profile " <> cs (show meta.line)


wcsHeader :: (Error ProfileError :> es) => ArmWavMeta fit -> SliceXY -> Header -> Eff es (WCSHeader ProfileAxes)
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
  -> ArmWavMeta fit
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


wcsWavelength :: (Monad m) => ArmWavMeta fit -> m (ProfileAxis alt n)
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


data ProfileError
  = InvalidWavelengthGroups
  | MissingProfileExtensions String
  | InvalidWCS ParseError
  deriving (Show, Exception, Eq)

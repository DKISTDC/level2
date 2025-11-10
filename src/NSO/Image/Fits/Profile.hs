{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NSO.Image.Fits.Profile where

import Control.Exception (Exception)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Effectful
import Effectful.Error.Static
import NSO.Image.Fits.Quantity (addDummyAxis, dataCommon)
import NSO.Image.Headers
import NSO.Image.Headers.DataCommon
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Image.Headers.WCS
import NSO.Image.Types.Frame (Arms (..), Stokes)
import NSO.Image.Types.Profile
import NSO.Prelude
import NSO.Types.Wavelength (SpectralLine (..), ionName)
import Telescope.Data.Axes (AxisOrder (..))
import Telescope.Data.DataCube
import Telescope.Data.KnownText
import Telescope.Data.WCS
import Telescope.Fits as Fits
import Telescope.Fits.Header (Header (..), HeaderRecord (..))


-- Profile FITS Generation -----------------------------------------------------------

data ProfileFrameFits fit = ProfileFrameFits
  { header :: ProfileHeader fit
  , image :: ProfileImage fit
  }


data ArmProfileFrameFits = ArmProfileFrameFits
  { fit :: ProfileFrameFits Fit
  , original :: ProfileFrameFits Original
  }


profilesForFrame
  :: forall es
   . (Error ProfileError :> es)
  => SliceXY
  -> UTCTime
  -> Header
  -> Arms ArmProfileImages
  -> Eff es (Arms ArmProfileFrameFits)
profilesForFrame slice now l1 pros = Arms <$> mapM profileArm pros.arms
 where
  profileArm :: ArmProfileImages -> Eff es ArmProfileFrameFits
  profileArm p = do
    fit <- profileFrameFits @Fit p.arm p.fit
    original <- profileFrameFits @Original p.arm p.original
    pure $ ArmProfileFrameFits{fit, original}

  profileFrameFits :: ArmWavMeta -> ProfileImage fit -> Eff es (ProfileFrameFits fit)
  profileFrameFits arm image = do
    h <- profileHeader now slice l1 arm image
    pure $ ProfileFrameFits h image


profileHDUs :: Arms ArmProfileFrameFits -> [DataHDU]
profileHDUs (Arms arms) = mconcat $ fmap pairHDUs $ NE.toList arms
 where
  pairHDUs :: ArmProfileFrameFits -> [DataHDU]
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
  -> ArmWavMeta
  -> ProfileImage fit
  -> Eff es (ProfileHeader fit)
profileHeader now slice l1 arm img = do
  common <- dataCommon now img.data_
  wcs <- wcsHeader arm slice l1
  pure $ ProfileHeader{common, wcs, meta = arm}


data ProfileHeader fit = ProfileHeader
  { meta :: ArmWavMeta
  , common :: DataCommon
  , wcs :: WCSHeader ProfileAxes
  }
  deriving (Generic)
instance (KnownText fit) => ToHeader (ProfileHeader fit) where
  toHeader h = writeHeader $ do
    let typ = ProfType (knownText @fit)
        ion = ProfIon h.meta.line.ion

    sectionHeader "Spectral Profile" "Headers describing the spectral profile"
    addKeywords $ hduInfo typ ion
    addKeywords h.common

    addKeywords $ Keyword $ keywordRecord typ
    addKeywords $ Keyword $ keywordRecord ion

    addKeywords h.wcs
   where
    hduInfo typ ion =
      fmap
        Keyword
        [ KeywordRecord (keyword @(ExtName "")) (extName typ ion) Nothing
        , keywordRecord @(BType "spect.line.profile") BType
        , keywordRecord @(BUnit Dimensionless) BUnit
        ]

    extName :: ProfType -> ProfIon -> Value
    extName (ProfType typ) (ProfIon ion) =
      String $ ionName ion <> " Profile " <> typ


wcsHeader :: (Error ProfileError :> es) => ArmWavMeta -> SliceXY -> Header -> Eff es (WCSHeader ProfileAxes)
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
  :: forall alt es
   . (Error ParseError :> es, KnownText alt)
  => SliceXY
  -> ArmWavMeta
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


wcsWavelength :: (Monad m) => ArmWavMeta -> m (ProfileAxis alt n)
wcsWavelength wp = do
  let crpix = Key $ realToFrac wp.pixel
      crval = Key (realToFrac wp.line.wavelength)
      cdelt = Key $ realToFrac wp.delta.value
      cunit = Key "nm"
      ctype = Key "AWAV"
  let keys = WCSAxisKeywords{..}
  let pcs = ProfilePCs{stokes = PC 0.0, dummyY = PC 0, slitX = PC 0, wavelength = PC 1.0}
  pure $ ProfileAxis{keys, pcs = Just pcs}


data ProfileError
  = InvalidWavelengthGroups
  | InvalidProfileType Text
  | MissingProfileExtensions String
  | MissingProfileType SpectralLine String
  | -- | InvalidSpectralLines ParseError
    InvalidWCS ParseError
  deriving (Show, Exception, Eq)

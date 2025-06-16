{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Fits.Frame where

import Data.ByteString qualified as BS
import Data.List.Ext
import Data.Massiv.Array ()
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import NSO.Image.Fits.Profile as Profile
import NSO.Image.Fits.Quantity as Quantity
import NSO.Image.Headers
import NSO.Image.Headers.Types (Depth, SliceXY, SlitX)
import NSO.Image.Primary
import NSO.Image.Types.Profile
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Inversion (Inversion)
import Telescope.Asdf as Asdf
import Telescope.Data.Axes (Axes (..), axesRowMajor)
import Telescope.Data.DataCube (dataCubeAxes)
import Telescope.Data.Parser (ParseError, parseFail)
import Telescope.Fits as Fits
import Telescope.Fits.Encoding (replaceKeywordLine)
import Telescope.Fits.Header.Class (parseKeyword)


-- we need to be able to get to a fits output
data L2FrameFits = L2FrameFits
  { primary :: PrimaryHeader
  , quantities :: Quantities QuantityFrameFits
  , profiles :: Arms (Profile ProfileFrameFits)
  }
  deriving (Generic)


data L2FrameInputs = L2FrameInputs
  { quantities :: Quantities (QuantityImage [SlitX, Depth])
  , profiles :: Arms (Profile ProfileImage)
  , l1Frame :: BinTableHDU
  }


filenameL2Fits :: Id Inversion -> UTCTime -> Path' Filename L2FrameFits
filenameL2Fits ii dt = Path $ cs $ frameFilename dt ii


generateL2FrameFits
  :: (Error QuantityError :> es, Error ProfileError :> es, Error PrimaryError :> es, GenRandom :> es)
  => UTCTime
  -> Id Inversion
  -> SliceXY
  -> L2FrameInputs
  -> Eff es L2FrameFits
generateL2FrameFits now i slice gf = do
  ph <- primaryHeader i gf.l1Frame.header
  qs <- quantities slice now gf.l1Frame.header gf.quantities
  ps <- profiles slice now gf.l1Frame.header gf.profiles
  pure $
    L2FrameFits
      { primary = ph
      , quantities = qs
      , profiles = ps
      }


-- | See the HDUOrder class below
frameToFits :: L2FrameFits -> Fits
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


newtype HDUIndex = HDUIndex Int
  deriving newtype (ToAsdf, Num)


class HDUOrder a where
  hduIndex :: HDUIndex
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

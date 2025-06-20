{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Fits.Frame where

import Data.ByteString qualified as BS
import Data.Massiv.Array ()
import Data.String.Interpolate (i)
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import NSO.Data.Scratch (Scratch (..))
import NSO.Image.Files qualified as Files
import NSO.Image.Fits.Profile as Profile
import NSO.Image.Fits.Quantity as Quantity
import NSO.Image.Headers
import NSO.Image.Headers.Types (Depth, SliceXY, SlitX)
import NSO.Image.Primary
import NSO.Image.Types.Profile
import NSO.Image.Types.Quantity
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Inversion)
import Telescope.Asdf as Asdf
import Telescope.Fits as Fits
import Telescope.Fits.Encoding (replaceKeywordLine)


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
instance Show L2FrameInputs where
  show inp =
    let ks = keywords inp.l1Frame.header
        ps = inp.profiles
     in [i| L2FrameInputs { quantities = , profiles = #{ps}, l1Frame = #{length ks}} |]


outputL2Fits :: Id Proposal -> Id Inversion -> UTCTime -> Path L2FrameFits
outputL2Fits ip ii dt =
  filePath (Files.outputL2Dir ip ii) $ filenameL2Fits ii dt


filenameL2Fits :: Id Inversion -> UTCTime -> Path' Filename L2FrameFits
filenameL2Fits ii dt = Path $ cs $ fitsFrameFilename dt ii


deleteL2FramesFits :: (Scratch :> es) => Id Proposal -> Id Inversion -> Eff es ()
deleteL2FramesFits propId invId = do
  send $ RemoveDir $ Files.outputL2Dir propId invId


generateL2FrameFits
  :: (Error QuantityError :> es, Error ProfileError :> es, Error PrimaryError :> es, GenRandom :> es)
  => UTCTime
  -> Id Inversion
  -> SliceXY
  -> L2FrameInputs
  -> Eff es L2FrameFits
generateL2FrameFits now invId slice gf = do
  ph <- primaryHeader invId gf.l1Frame.header
  qs <- quantities slice now gf.l1Frame.header gf.quantities
  ps <- profilesForFrame slice now gf.l1Frame.header gf.profiles
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

{-# LANGUAGE UndecidableInstances #-}

module NSO.Image.Frame
  ( generateL2Frame
  , frameToFits
  , frameMeta
  , L2Frame (..)
  , L2FrameMeta (..)
  , L2FrameInputs (..)
  , collateFrames
  , ProfileFit (..)
  , filenameL2Frame
  , encodeL2
  , SliceXY
  , PrimaryHeader (..)
  , Observation (..)
  , Key (..)
  ) where

import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Massiv.Array ()
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import NSO.Image.Error
import NSO.Image.Headers
import NSO.Image.Headers.Keywords (HeaderKeywords (..))
import NSO.Image.Headers.Parse (ParseKeyError (..))
import NSO.Image.Headers.Types (DateTime (..), Depth, Key (..), SliceXY, SlitX)
import NSO.Image.Profile
import NSO.Image.Quantities (Quantities (..), Quantity, QuantityData, QuantityHeader, quantities, quantityHDUs, quantityHeaders)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Inversion (Inversion)
import Telescope.Fits as Fits
import Telescope.Fits.Encoding (replaceKeywordLine)


data L2Frame = L2Frame
  { primary :: PrimaryHeader
  , quantities :: Quantities Quantity
  , profiles :: Profiles Profile
  }


data L2FrameMeta = L2FrameMeta
  { primary :: PrimaryHeader
  , quantities :: Quantities QuantityHeader
  , profiles :: Profiles ProfileHeader
  , path :: Path L2Frame
  }


data L2FrameInputs = L2FrameInputs
  { quantities :: Quantities (QuantityData [SlitX, Depth])
  , profileFit :: ProfileFrame Fit
  , profileOrig :: ProfileFrame Original
  , l1Frame :: BinTableHDU
  }


data PrimaryHeader = PrimaryHeader
  { observation :: Observation
  , telescope :: Telescope
  , datacenter :: Datacenter
  , dkist :: DKISTHeader
  , adaptive :: AdaptiveOptics
  , contributing :: ContribExpProp
  }


collateFrames :: (Error GenerateError :> es) => [Quantities (QuantityData [SlitX, Depth])] -> [ProfileFrame Fit] -> [ProfileFrame Original] -> [BinTableHDU] -> Eff es [L2FrameInputs]
collateFrames qs pfs pos ts
  | allFramesEqual = pure $ L.zipWith4 L2FrameInputs qs pfs pos ts
  | otherwise = throwError $ MismatchedFrames frameSizes
 where
  allFramesEqual :: Bool
  allFramesEqual =
    all (== length qs) $ allSizes frameSizes

  frameSizes :: FrameSizes
  frameSizes =
    FrameSizes
      { quantities = length qs
      , fit = length pfs
      , original = length pos
      , l1 = length ts
      }

  allSizes :: FrameSizes -> [Int]
  allSizes fs = [fs.quantities, fs.fit, fs.original, fs.l1]


filenameL2Frame :: Id Inversion -> DateTime -> Path' Filename L2Frame
filenameL2Frame ii (DateTime dt) = Path $ cs (T.toUpper $ T.map toUnderscore $ ii.fromId <> "_" <> dt) <> "_L2.fits"
 where
  toUnderscore :: Char -> Char
  toUnderscore '.' = '_'
  toUnderscore ':' = '_'
  toUnderscore '-' = '_'
  toUnderscore c = c


-- | Encode and insert framevol
encodeL2 :: Fits -> BS.ByteString
encodeL2 f' =
  let out = Fits.encode f'
      mb = fromIntegral (BS.length out) / 1000000
   in replaceKeywordLine "FRAMEVOL" (Float mb) (Just "[Mb]") out


generateL2Frame
  :: (Error GenerateError :> es, GenRandom :> es)
  => UTCTime
  -> Id Inversion
  -> SliceXY
  -> WavProfiles Original
  -> WavProfiles Fit
  -> L2FrameInputs
  -> Eff es (L2Frame, DateTime)
generateL2Frame now i slice wpo wpf gf =
  runErrorNoCallStackWith @ParseKeyError (throwError . ParseKeyError) $ do
    ph <- primaryHeader i gf.l1Frame
    qs <- quantities slice now gf.l1Frame.header gf.quantities
    ps <- profiles slice now gf.l1Frame.header wpo wpf gf.profileOrig gf.profileFit
    let dateBeg = ph.observation.dateBeg.ktype
    let frame = L2Frame{primary = ph, quantities = qs, profiles = ps}
    pure (frame, dateBeg)


frameMeta :: L2Frame -> Path L2Frame -> L2FrameMeta
frameMeta frame path =
  L2FrameMeta
    { primary = frame.primary
    , quantities = quantityHeaders frame.quantities
    , profiles = profileHeaders frame.profiles
    , path
    }


frameToFits :: L2Frame -> Fits
frameToFits frame =
  let prim = primaryHDU frame.primary
      images = quantityHDUs frame.quantities
      profs = profileHDUs frame.profiles
   in Fits prim $ fmap Image $ images <> profs


primaryHeader :: (Error ParseKeyError :> es, GenRandom :> es) => Id Inversion -> BinTableHDU -> Eff es PrimaryHeader
primaryHeader ii l1 = do
  observation <- observationHeader l1.header
  telescope <- telescopeHeader l1.header
  datacenter <- datacenterHeader l1.header ii
  dkist <- dkistHeader l1.header
  contributing <- contribExpProp l1.header
  adaptive <- adaptiveOpticsHeader l1.header
  pure $ PrimaryHeader{observation, telescope, datacenter, dkist, adaptive, contributing}


primaryHDU :: PrimaryHeader -> PrimaryHDU
primaryHDU h =
  let hs = runPureEff $ writeHeader allKeys
   in PrimaryHDU (Header hs) emptyDataArray
 where
  allKeys = do
    primKeys
    teleKeys
    datacenterKeys
    dkistKeys
    adaptiveKeys

  primKeys = do
    sectionHeader "Observation" "Keys describing the observation and general metadata"
    addKeywords $ headerKeywords @Observation h.observation

  teleKeys = do
    sectionHeader "Telescope" "Keys describing the pointing and op of the Telescope"
    addKeywords $ headerKeywords @Telescope h.telescope

  datacenterKeys = do
    sectionHeader "Datacenter" "Keys generated by the DKIST data center to describe processing performed, archiving or extra metadata"
    addKeywords $ headerKeywords @Datacenter h.datacenter
    addKeywords $ headerKeywords @ContribExpProp h.contributing

  dkistKeys = do
    sectionHeader "DKIST Operations" "Information about this configuration or operations of the facility when generating this data"
    addKeywords $ headerKeywords @DKISTHeader h.dkist

  adaptiveKeys = do
    sectionHeader "Adaptive Optics" "Keys describing aspects of the adaptive optics system"
    addKeywords $ headerKeywords @AdaptiveOptics h.adaptive

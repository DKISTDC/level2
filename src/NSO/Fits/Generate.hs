{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate
  ( generateL2Fits
  , L2Frame (..)
  , collateFrames
  , decodeQuantitiesFrames
  , decodeProfileFit
  , decodeProfileOrig
  , ProfileFit (..)
  , filenameL2Frame
  , encodeL2
  , SliceXY
  ) where

import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Massiv.Array ()
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Writer.Static.Local
import NSO.Fits.Generate.Error
import NSO.Fits.Generate.Headers
import NSO.Fits.Generate.Headers.Keywords (HeaderKeywords (..))
import NSO.Fits.Generate.Headers.Parse (ParseKeyError (..))
import NSO.Fits.Generate.Headers.Types (DateTime (..), Depth, Key (..), SliceXY, SlitX)
import NSO.Fits.Generate.Profile
import NSO.Fits.Generate.Quantities (Quantities (..), QuantitiesData, decodeQuantitiesFrames, quantities, quantitiesHDUs)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Inversion (Inversion)
import Telescope.Fits as Fits
import Telescope.Fits.Encoding (replaceKeywordLine)


data L2Frame = L2Frame
  { quantities :: QuantitiesData [SlitX, Depth]
  , profileFit :: ProfileFrame Fit
  , profileOrig :: ProfileFrame Original
  , l1Frame :: BinTableHDU
  }


data PrimaryHeader = PrimaryHeader
  { observation :: ObservationHeader
  , telescope :: TelescopeHeader
  , datacenter :: Datacenter
  , dkist :: DKISTHeader
  , adaptive :: AdaptiveOptics
  , contributing :: ContribExpProp
  }


collateFrames :: (Error GenerateError :> es) => [QuantitiesData [SlitX, Depth]] -> [ProfileFrame Fit] -> [ProfileFrame Original] -> [BinTableHDU] -> Eff es [L2Frame]
collateFrames qs pfs pos ts
  | allFramesEqual = pure $ L.zipWith4 L2Frame qs pfs pos ts
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


-- \| Encode and insert framevol
encodeL2 :: Fits -> BS.ByteString
encodeL2 f' =
  let out = Fits.encode f'
      mb = fromIntegral (BS.length out) / 1000000
   in replaceKeywordLine "FRAMEVOL" (Float mb) (Just "[Mb]") out


generateL2Fits
  :: (Error GenerateError :> es, GenRandom :> es)
  => UTCTime
  -> Id Inversion
  -> SliceXY
  -> WavProfiles Original
  -> WavProfiles Fit
  -> L2Frame
  -> Eff es (Fits, DateTime)
generateL2Fits now i slice wpo wpf gf =
  runErrorNoCallStackWith @ParseKeyError (throwError . ParseKeyError) $ do
    ph <- primaryHeader i gf.l1Frame
    prim <- primaryHDU ph

    qs <- quantities slice now gf.l1Frame.header gf.quantities
    imgs <- quantitiesHDUs qs

    profs <- profileHDUs slice now gf.l1Frame.header wpo wpf gf.profileOrig gf.profileFit
    let fits = Fits prim $ fmap Image $ imgs <> profs
    pure (fits, ph.observation.dateBeg.ktype)


primaryHeader :: (Error ParseKeyError :> es, GenRandom :> es) => Id Inversion -> BinTableHDU -> Eff es PrimaryHeader
primaryHeader ii l1 = do
  observation <- observationHeader l1.header
  telescope <- telescopeHeader l1.header
  datacenter <- datacenterHeader l1.header ii
  dkist <- dkistHeader l1.header
  contributing <- contribExpProp l1.header
  adaptive <- adaptiveOpticsHeader l1.header
  pure $ PrimaryHeader{observation, telescope, datacenter, dkist, adaptive, contributing}


primaryHDU :: (Error ParseKeyError :> es, GenRandom :> es) => PrimaryHeader -> Eff es PrimaryHDU
primaryHDU h = do
  hs <- execWriter allKeys
  pure $ PrimaryHDU (Header hs) emptyDataArray
 where
  allKeys = do
    primKeys
    teleKeys
    datacenterKeys
    dkistKeys
    adaptiveKeys

  primKeys = do
    sectionHeader "Observation" "Keys describing the observation and general metadata"
    addKeywords $ headerKeywords @ObservationHeader h.observation

  teleKeys = do
    sectionHeader "Telescope" "Keys describing the pointing and op of the Telescope"
    addKeywords $ headerKeywords @TelescopeHeader h.telescope

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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate.Frames where

import Data.ByteString qualified as BS
import Data.Kind
import Data.Massiv.Array as M hiding (mapM, mapM_, transposeOuter, zip)
import NSO.Fits.Generate.Results
import NSO.Fits.Generate.Types
import NSO.Prelude
import NSO.Types.Wavelength (Wavelength)
import Telescope.Fits as Fits


-- Profiles ---------------------------------------------------------------------------------------

data Original
data Fit


data ProfileFrame a = ProfileFrame
  { wav630 :: Results [SlitX, Wavelength 630, Stokes]
  , wav854 :: Results [SlitX, Wavelength 854, Stokes]
  }


decodeProfileFrames :: forall a m. (MonadThrow m) => BS.ByteString -> m [ProfileFrame a]
decodeProfileFrames inp = do
  f <- decode inp
  pro <- mainProfile f
  -- wvs <- wavs f
  wis <- wavIds f
  mapM (fmap (uncurry ProfileFrame) . splitWavelengths wis) $ profileFrames pro
 where
  mainProfile :: (MonadThrow m) => Fits -> m (Results [Stokes, Wavs, FrameY, SlitX])
  mainProfile f = do
    a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
    pure $ Results a

  -- wavs :: (MonadThrow m) => Fits -> m (Results '[Wavs])
  -- wavs f = do
  --   case f.extensions of
  --     (Image h : _) -> do
  --       Results <$> decodeArray @Ix1 h.dataArray
  --     _ -> throwM $ MissingProfileExtensions "Wavelength Values"

  wavIds :: (MonadThrow m) => Fits -> m (Results '[WavIds])
  wavIds f = do
    case f.extensions of
      [_, Image h] -> do
        Results <$> decodeArray @Ix1 h.dataArray
      _ -> throwM $ MissingProfileExtensions "Wavelength Values"


profileFrames :: Results [Stokes, Wavs, FrameY, SlitX] -> [Results [SlitX, Wavs, Stokes]]
profileFrames = fmap swapProfileDimensions . splitFrames


swapProfileDimensions :: Results [Stokes, Wavs, SlitX] -> Results [SlitX, Wavs, Stokes]
swapProfileDimensions =
  transposeMajor . transposeMinor3 . transposeMajor


-- TODO: implement splitWavelengths...
splitWavelengths :: (MonadThrow m) => Results '[WavIds] -> Results [a, Wavs, c] -> m (Results [a, Wavelength 630, c], Results [a, Wavelength 854, c])
splitWavelengths wavIds res = do
  wx <- indexBreak wavIds
  splitM1 wx res
 where
  indexBreak wds =
    case group (M.toList wds.array) of
      (w1 : _) -> pure $ length w1
      _ -> throwM InvalidWavelengthGroups


-- trace (show wavIds) $ Results arr

-- Quantities --------------------------------------------------------------------------------------

data Quantities (as :: [Type]) = Quantities
  { opticalDepth :: Results as
  , temperature :: Results as
  , electronPressure :: Results as
  , microTurbulence :: Results as
  , magStrength :: Results as
  , velocity :: Results as
  , magInclination :: Results as
  , magAzimuth :: Results as
  , geoHeight :: Results as
  , gasPressure :: Results as
  , density :: Results as
  }


decodeQuantitiesFrames :: (MonadIO m, MonadThrow m) => BS.ByteString -> m [Quantities [SlitX, Depth]]
decodeQuantitiesFrames inp = do
  res <- decodeInversionResults inp
  resultsQuantities res


decodeInversionResults :: (MonadThrow m) => BS.ByteString -> m (Results [Quantity, Depth, FrameY, SlitX])
decodeInversionResults inp = do
  f <- decode inp
  a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ Results a


resultsQuantities :: (MonadThrow m) => Results [Quantity, Depth, FrameY, SlitX] -> m [Quantities [SlitX, Depth]]
resultsQuantities res = do
  mapM splitQuantitiesM $ splitFrames res


splitQuantitiesM :: (MonadThrow m) => Results [Quantity, Depth, SlitX] -> m (Quantities [SlitX, Depth])
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: Results [Quantity, Depth, SlitX] -> Maybe (Quantities [SlitX, Depth])
splitQuantities res = do
  let qs = fmap transposeMajor $ outerList res
  [opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density] <- pure qs
  pure Quantities{..}


-- Frames -----------------------------------------------------------------------

-- | Splits any Data Cube into frames when it is the 3/4 dimension
splitFrames :: forall a b d. Results [a, b, FrameY, d] -> [Results [a, b, d]]
splitFrames res =
  fmap sliceFrame [0 .. numFrames res - 1]
 where
  numFrames :: Results [a, b, FrameY, d] -> Int
  numFrames (Results arr) =
    let Sz (_ :> _ :> nf :. _) = size arr
     in nf

  sliceFrame :: Int -> Results [a, b, d]
  sliceFrame n = sliceM2 n res


-- Errors ------------------------------------------

data GenerateError
  = InvalidFrameShape (Sz Ix3)
  | InvalidFits String
  | FrameOutOfBounds (Sz Ix4) Int
  | MissingProfileExtensions String
  | InvalidWavelengthGroups
  deriving (Show, Eq, Exception)

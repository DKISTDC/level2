{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate.Frames where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString qualified as BS
import Data.Kind
import Data.Massiv.Array (Ix2 (..), IxN (..), Sz (..))
import NSO.Fits.Generate.DimArray
import NSO.Fits.Generate.Types
import NSO.Prelude
import Telescope.Fits as Fits


-- trace (show wavIds) $ DimArray arr

-- Quantities --------------------------------------------------------------------------------------

data Quantities (as :: [Type]) = Quantities
  { opticalDepth :: DimArray as
  , temperature :: DimArray as
  , electronPressure :: DimArray as
  , microTurbulence :: DimArray as
  , magStrength :: DimArray as
  , velocity :: DimArray as
  , magInclination :: DimArray as
  , magAzimuth :: DimArray as
  , geoHeight :: DimArray as
  , gasPressure :: DimArray as
  , density :: DimArray as
  }


decodeQuantitiesFrames :: (MonadIO m, MonadThrow m) => BS.ByteString -> m [Quantities [SlitX, Depth]]
decodeQuantitiesFrames inp = do
  res <- decodeInversion inp
  resultsQuantities res


decodeInversion :: (MonadThrow m) => BS.ByteString -> m (DimArray [Quantity, Depth, FrameY, SlitX])
decodeInversion inp = do
  f <- decode inp
  a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ DimArray a


resultsQuantities :: (MonadThrow m) => DimArray [Quantity, Depth, FrameY, SlitX] -> m [Quantities [SlitX, Depth]]
resultsQuantities res = do
  mapM splitQuantitiesM $ splitFrames res


splitQuantitiesM :: (MonadThrow m) => DimArray [Quantity, Depth, SlitX] -> m (Quantities [SlitX, Depth])
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: DimArray [Quantity, Depth, SlitX] -> Maybe (Quantities [SlitX, Depth])
splitQuantities res = do
  let qs = fmap transposeMajor $ outerList res
  [opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density] <- pure qs
  pure Quantities{..}


-- Frames -----------------------------------------------------------------------

-- | Splits any Data Cube into frames when it is the 3/4 dimension
splitFrames :: forall a b d. DimArray [a, b, FrameY, d] -> [DimArray [a, b, d]]
splitFrames res =
  fmap sliceFrame [0 .. numFrames res - 1]
 where
  numFrames :: DimArray [a, b, FrameY, d] -> Int
  numFrames (DimArray arr) =
    let Sz (_ :> _ :> nf :. _) = size arr
     in nf

  sliceFrame :: Int -> DimArray [a, b, d]
  sliceFrame n = sliceM2 n res

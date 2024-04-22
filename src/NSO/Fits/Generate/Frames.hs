{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate.Frames where

import Data.ByteString qualified as BS
import Data.Kind
import Data.Massiv.Array hiding (mapM, mapM_, transposeOuter)
import Effectful
import Effectful.Writer.Static.Local
import NSO.Fits.Generate.Types
import NSO.Prelude
import Telescope.Fits as Fits
import Telescope.Fits.Types as Fits


-- Quantiies To Fits -------------------------------------------------

type Frame = [SlitX, Depth]


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


-- Parse Quantities ---------------------------------------------------------------------------------

readQuantitiesFrames :: (MonadIO m, MonadThrow m) => FilePath -> m [Quantities Frame]
readQuantitiesFrames fp = do
  inp <- liftIO $ BS.readFile fp
  res <- decodeResults inp
  resultsQuantities res


decodeResults :: (MonadThrow m) => BS.ByteString -> m (Results [Quantity, Depth, FrameY, SlitX])
decodeResults inp = do
  f <- decode inp
  a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ Results a


resultsQuantities :: (MonadThrow m) => Results [Quantity, Depth, FrameY, SlitX] -> m [Quantities Frame]
resultsQuantities res = do
  fs <- resultsByFrame res
  mapM splitQuantitiesM fs


resultsByFrame :: (MonadThrow m) => Results [Quantity, Depth, FrameY, SlitX] -> m [Results [Quantity, Depth, SlitX]]
resultsByFrame res =
  let maxFrame = numFrames res - 1
   in mapM (sliceFrame res) [0 .. maxFrame]
 where
  numFrames :: Results [Quantity, Depth, FrameY, SlitX] -> Int
  numFrames (Results arr) =
    let Sz (_ :> _ :> nf :. _) = size arr
     in nf

  sliceFrame :: (MonadThrow m) => Results [Quantity, Depth, FrameY, SlitX] -> Int -> m (Results [Quantity, Depth, SlitX])
  sliceFrame (Results arr) n =
    case arr <!?> (Dim 2, n) of
      Nothing -> throwM $ FrameOutOfBounds (size arr) n
      Just s -> pure $ Results s


splitQuantitiesM :: (MonadThrow m) => Results [Quantity, Depth, SlitX] -> m (Quantities Frame)
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: Results [Quantity, Depth, SlitX] -> Maybe (Quantities Frame)
splitQuantities res = do
  let qs = fmap (transpose2) $ outerList res
  [opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density] <- pure qs
  pure Quantities{..}


-- Results ------------------------------------------------------------------------------

newtype Results (as :: [Type]) = Results
  { array :: Array D (ResultsIx as) Float
  }


instance (Ragged L (ResultsIx as) Float) => Show (Results as) where
  show (Results a) = show a


class IsResults (as :: [Type]) where
  type ResultsIx as :: Type
  outerLength :: Results as -> Int


instance IsResults '[a] where
  type ResultsIx '[a] = Ix1
  outerLength (Results a) =
    let Sz s = size a in s


instance IsResults '[a, b] where
  type ResultsIx '[a, b] = Ix2
  outerLength (Results a) =
    let Sz (s :. _) = size a in s


instance IsResults '[a, b, c] where
  type ResultsIx '[a, b, c] = Ix3
  outerLength (Results a) =
    let Sz (s :> _) = size a in s


instance IsResults '[a, b, c, d] where
  type ResultsIx '[a, b, c, d] = Ix4
  outerLength (Results a) =
    let Sz (s :> _) = size a in s


outerList
  :: forall a as
   . (Lower (ResultsIx (a : as)) ~ ResultsIx as, Index (ResultsIx as), Index (ResultsIx (a : as)))
  => Results (a : as)
  -> [Results as]
outerList (Results a) = foldOuterSlice row a
 where
  row :: Array D (ResultsIx as) Float -> [Results as]
  row r = [Results r]


transpose2 :: Results [a, b] -> Results [b, a]
transpose2 (Results arr) = Results $ transposeInner arr


-- Errors ------------------------------------------

data GenerateError
  = InvalidFrameShape (Sz Ix3)
  | InvalidFits String
  | FrameOutOfBounds (Sz Ix4) Int
  deriving (Show, Eq, Exception)

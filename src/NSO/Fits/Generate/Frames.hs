{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate.Frames where

import Data.ByteString qualified as BS
import Data.Kind
import Data.Massiv.Array hiding (mapM, mapM_, transposeOuter)
import NSO.Fits.Generate.Types
import NSO.Prelude
import GHC.TypeLits (KnownNat, natVal)
import Telescope.Fits as Fits


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
  mapM splitQuantitiesM $ resultsByFrame res


resultsByFrame :: Results [Quantity, Depth, FrameY, SlitX] -> [Results [Quantity, Depth, SlitX]]
resultsByFrame res =
   fmap sliceFrame [0 .. numFrames res - 1]
 where
  numFrames :: Results [Quantity, Depth, FrameY, SlitX] -> Int
  numFrames (Results arr) =
    let Sz (_ :> _ :> nf :. _) = size arr
     in nf

  sliceFrame :: Int -> Results [Quantity, Depth, SlitX]
  sliceFrame n = sliceM2 n res


splitQuantitiesM :: (MonadThrow m) => Results [Quantity, Depth, SlitX] -> m (Quantities Frame)
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: Results [Quantity, Depth, SlitX] -> Maybe (Quantities Frame)
splitQuantities res = do
  let qs = fmap transposeMajor $ outerList res
  [opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density] <- pure qs
  pure Quantities{..}


-- Results ------------------------------------------------------------------------------

newtype Results (as :: [Type]) = Results
  { array :: Array D (ResultsIx as) Float
  }


instance (Index (ResultsIx as)) => Eq (Results as) where
  Results arr == Results arr2 = arr == arr2


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


transposeMajor
  :: (ResultsIx (a : b : xs) ~ ResultsIx (b : a : xs), Index (Lower (ResultsIx (b : a : xs))), Index (ResultsIx (b : a : xs)))
  => Results (a : b : xs)
  -> Results (b : a : xs)
transposeMajor (Results arr) = Results $ transposeInner arr


-- Slice the 
sliceM0
  :: (Lower (ResultsIx (a : xs)) ~ ResultsIx xs, Index (ResultsIx xs), Index (ResultsIx (a : xs)))
  => Int
  -> Results (a : xs)
  -> Results xs
sliceM0 a (Results arr) = Results (arr !> a)


-- Slice the 2nd major dimension
sliceM1
  :: forall a b xs
  . ( Lower (ResultsIx (a : b : xs)) ~ ResultsIx (a : xs)
     , Index (ResultsIx (a : xs))
     , Index (ResultsIx (a : b : xs))
     , KnownNat (Dimensions (ResultsIx (a : b : xs)))
     )
  => Int
  -> Results (a : b : xs)
  -> Results (a : xs)
sliceM1 b (Results arr) =
  let dims = fromIntegral $ natVal @(Dimensions (ResultsIx (a : b : xs))) Proxy
  in Results $ arr <!> (Dim (dims - 1), b)

-- Slice the 3rd major dimension
sliceM2
  :: forall a b c xs
  . ( Lower (ResultsIx (a : b : c : xs)) ~ ResultsIx (a : b : xs)
     , Index (ResultsIx (a : b : xs))
     , Index (ResultsIx (a : b : c : xs))
     , KnownNat (Dimensions (ResultsIx (a : b : c : xs)))
     )
  => Int
  -> Results (a : b : c : xs)
  -> Results (a : b : xs)
sliceM2 c (Results arr) =
  let dims = fromIntegral $ natVal @(Dimensions (ResultsIx (a : b : c : xs))) Proxy
  in Results $ arr <!> (Dim (dims - 2), c)



-- Errors ------------------------------------------

data GenerateError
  = InvalidFrameShape (Sz Ix3)
  | InvalidFits String
  | FrameOutOfBounds (Sz Ix4) Int
  deriving (Show, Eq, Exception)

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Data.Generate where

import Data.ByteString qualified as BS

-- import Data.Fits
import Data.Kind
import Data.Massiv.Array hiding (mapM, mapM_, transposeOuter)
import NSO.Prelude
import Telescope.Fits as Fits


------------------------------------------------------------------------------

testInput :: FilePath
testInput = "/Users/seanhess/Data/scan1807/inv_res_mod.fits"


test :: IO ()
test = do
  putStrLn "TEST"
  res <- readQuantities testInput

  let (q : qs) = res

  print $ q.temperature !> 1


------------------------------------------------------------------------------

-- TODO: generate a bunch of HDUs!
quantitiesToFits :: Quantities -> Fits
quantitiesToFits qs = _


readQuantities :: (MonadIO m, MonadThrow m) => FilePath -> m [Quantities]
readQuantities fp = do
  inp <- liftIO $ BS.readFile fp
  res <- decodeResults inp
  resultsQuantities res


decodeResults :: (MonadThrow m) => BS.ByteString -> m (Results [Quantity, Depth, FrameY, SlitX])
decodeResults inp = do
  f <- decode inp
  a <- decodeArray @Ix4 @Float f.primaryHDU.dataArray
  pure $ Results a


resultsQuantities :: (MonadThrow m) => Results [Quantity, Depth, FrameY, SlitX] -> m [Quantities]
resultsQuantities r = do
  let rbf = resultsByFrame r
  mapM splitQuantitiesM rbf


resultsByFrame :: Results [Quantity, Depth, FrameY, SlitX] -> [Results [Quantity, Depth, SlitX]]
resultsByFrame (Results arr) =
  let Sz (_ :> _ :> nf :. _) = size arr
   in fmap (Results . frame) [0 .. nf - 1]
 where
  frame n = arr <!> (Dim 2, n)


data Quantities = Quantities
  { opticalDepth :: Array D Ix2 Float
  , temperature :: Array D Ix2 Float
  , electronPressure :: Array D Ix2 Float
  , microTurbulence :: Array D Ix2 Float
  , magStrength :: Array D Ix2 Float
  , velocity :: Array D Ix2 Float
  , magInclination :: Array D Ix2 Float
  , magAzimuth :: Array D Ix2 Float
  , geoHeight :: Array D Ix2 Float
  , gasPressure :: Array D Ix2 Float
  , density :: Array D Ix2 Float
  }


data GenerateError
  = InvalidFrameShape (Sz Ix3)
  | InvalidFits String
  deriving (Show, Eq, Exception)


splitQuantitiesM :: (MonadThrow m) => Results [Quantity, Depth, SlitX] -> m Quantities
splitQuantitiesM rbf =
  case splitQuantities rbf of
    Nothing -> throwM $ InvalidFrameShape (size rbf.array)
    Just qs -> pure qs


splitQuantities :: Results [Quantity, Depth, SlitX] -> Maybe Quantities
splitQuantities res = do
  let qs = fmap (.array) $ outerList res
  [opticalDepth, temperature, electronPressure, microTurbulence, magStrength, velocity, magInclination, magAzimuth, geoHeight, gasPressure, density] <- pure qs
  pure Quantities{..}


-- quantitiesForFrames :: Results [FrameY, Quantity, Depth, SlitX] -> Maybe [Quantities]
-- quantitiesForFrames = mapM splitQuantities . outerList

-- O(2N)?
-- resultsByFrame :: (MonadThrow m, MonadUnliftIO m, PrimMonad m) => Results [Quantity, Depth, FrameY, SlitX] -> m (Results [FrameY, Quantity, Depth, SlitX])
-- resultsByFrame (Results a) = do
--   a' <- M.backpermuteM (newSize a) indexNewToOld a
--   pure $ Results a'
--  where
--   newSize a' =
--     let Sz s = size a'
--      in Sz $ indexOldToNew s
--
--   indexNewToOld (frameY :> quantity :> depth :. slitX) =
--     quantity :> depth :> frameY :. slitX
--
--   indexOldToNew (quantity :> depth :> frameY :. slitX) =
--     frameY :> quantity :> depth :. slitX

-- allFrames :: Results [FrameY, Quantity, Depth, SlitX] -> [Results [Quantity, Depth, SlitX]]
-- allFrames = outerList

outerList :: forall a as. (Lower (ResultsIx (a : as)) ~ ResultsIx as, Index (ResultsIx as), Index (ResultsIx (a : as))) => Results (a : as) -> [Results as]
outerList (Results a) = foldOuterSlice row a
 where
  row :: Array D (ResultsIx as) Float -> [Results as]
  row r = [Results r]


-- (|>) :: (Index (ResultsIx as), Index (ResultsIx (a : as)), Lower (ResultsIx (a : as)) ~ ResultsIx as) => Results (a : as) -> Int -> Results as
-- Results a |> n = Results $ a M.!> n
--
--
-- (|?>) :: (Index (ResultsIx as), Index (ResultsIx (a : as)), Lower (ResultsIx (a : as)) ~ ResultsIx as) => Results (a : as) -> Int -> Maybe (Results as)
-- Results a !?> n = do
--   sub <- a M.!?> n
--   pure $ Results sub

-- transposeOuter :: (ResultsIx (a : b : as) ~ ResultsIx (b : a : as), Index (ResultsIx (b : a : as)), Index (Lower (ResultsIx (b : a : as)))) => Results (a : b : as) -> Results (b : a : as)
-- transposeOuter (Results a) =
--   -- "inner" and "outer" are opposite my intuition in the library. Why "slice from the outside" but then those are the innermost dimensions?
--   Results (compute $ transposeInner a)

-- Results ------------------------------------------------------------------------------

newtype Results (as :: [Type]) = Results {array :: Array D (ResultsIx as) Float}


instance (Ragged L (ResultsIx as) Float) => Show (Results as) where
  show (Results a) = show a


data Depth
data SlitX
data FrameY
data Quantity
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

{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate.Results where

import Data.Kind
import Data.Massiv.Array as M hiding (mapM)
import GHC.TypeLits (natVal)
import NSO.Prelude


-- Results ------------------------------------------------------------------------------

newtype Results (as :: [Type]) = Results
  { array :: Array D (IndexOf as) Float
  }


instance (Index (IndexOf as)) => Eq (Results as) where
  Results arr == Results arr2 = arr == arr2


instance (Ragged L (IndexOf as) Float) => Show (Results as) where
  show (Results a) = show a


class HasIndex (as :: [Type]) where
  type IndexOf as :: Type


instance HasIndex '[a] where
  type IndexOf '[a] = Ix1


instance HasIndex '[a, b] where
  type IndexOf '[a, b] = Ix2


instance HasIndex '[a, b, c] where
  type IndexOf '[a, b, c] = Ix3


instance HasIndex '[a, b, c, d] where
  type IndexOf '[a, b, c, d] = Ix4


outerList
  :: forall a as
   . (Lower (IndexOf (a : as)) ~ IndexOf as, Index (IndexOf as), Index (IndexOf (a : as)))
  => Results (a : as)
  -> [Results as]
outerList (Results a) = foldOuterSlice row a
 where
  row :: Array D (IndexOf as) Float -> [Results as]
  row r = [Results r]


transposeMajor
  :: (IndexOf (a : b : xs) ~ IndexOf (b : a : xs), Index (Lower (IndexOf (b : a : xs))), Index (IndexOf (b : a : xs)))
  => Results (a : b : xs)
  -> Results (b : a : xs)
transposeMajor (Results arr) = Results $ transposeInner arr


transposeMinor4
  :: Results [a, b, c, d]
  -> Results [a, b, d, c]
transposeMinor4 (Results arr) = Results $ transposeOuter arr


transposeMinor3
  :: Results [a, b, c]
  -> Results [a, c, b]
transposeMinor3 (Results arr) = Results $ transposeOuter arr


--  Results $ -- foldOuterSlice each arr
-- where
--  each :: Array P Ix2 Float -> Array P Ix3 Float
--  each = _

-- Slice along the 1st major dimension
sliceM0
  :: ( Lower (IndexOf (a : xs)) ~ IndexOf xs
     , Index (IndexOf xs)
     , Index (IndexOf (a : xs))
     )
  => Int
  -> Results (a : xs)
  -> Results xs
sliceM0 a (Results arr) = Results (arr !> a)


-- Slice along the 2nd major dimension
sliceM1
  :: forall a b xs
   . ( Lower (IndexOf (a : b : xs)) ~ IndexOf (a : xs)
     , Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     )
  => Int
  -> Results (a : b : xs)
  -> Results (a : xs)
sliceM1 b (Results arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : xs))) Proxy
   in Results $ arr <!> (Dim (dims - 1), b)


-- Slice along the 3rd major dimension
sliceM2
  :: forall a b c xs
   . ( Lower (IndexOf (a : b : c : xs)) ~ IndexOf (a : b : xs)
     , Index (IndexOf (a : b : xs))
     , Index (IndexOf (a : b : c : xs))
     )
  => Int
  -> Results (a : b : c : xs)
  -> Results (a : b : xs)
sliceM2 c (Results arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : c : xs))) Proxy
   in Results $ arr <!> (Dim (dims - 2), c)


splitM0
  :: forall a ax ay xs m
   . ( Lower (IndexOf (a : xs)) ~ IndexOf xs
     , Index (IndexOf xs)
     , Index (IndexOf (a : xs))
     , IndexOf (a : xs) ~ IndexOf (ax : xs)
     , IndexOf (a : xs) ~ IndexOf (ay : xs)
     , MonadThrow m
     )
  => Int
  -> Results (a : xs)
  -> m (Results (ax : xs), Results (ay : xs))
splitM0 a (Results arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) a arr
  pure (Results arr1, Results arr2)


splitM1
  :: forall a b bx by xs m
   . ( Lower (IndexOf (a : b : xs)) ~ IndexOf (a : xs)
     , Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     , IndexOf (a : b : xs) ~ IndexOf (a : bx : xs)
     , IndexOf (a : b : xs) ~ IndexOf (a : by : xs)
     , MonadThrow m
     )
  => Int
  -> Results (a : b : xs)
  -> m (Results (a : bx : xs), Results (a : by : xs))
splitM1 b (Results arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) b arr
  pure (Results arr1, Results arr2)


sampleGen2 :: Ix2 -> Float
sampleGen2 (r :. c) = fromIntegral r * 10 + fromIntegral c


sampleGen3 :: Ix3 -> Float
sampleGen3 (d :> d2) = fromIntegral d * 100 + sampleGen2 d2


data X
data Y
data Z


sample2 :: Ix2 -> Results [Y, X]
sample2 ix = Results $ makeArray Seq (Sz ix) sampleGen2


sample3 :: Ix3 -> Results [Z, Y, X]
sample3 ix = Results $ makeArray Seq (Sz ix) sampleGen3

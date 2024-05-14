{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate.DimArray where

import Data.Kind
import Data.Massiv.Array as M hiding (mapM)
import GHC.TypeLits (natVal)
import NSO.Prelude


-- Results ------------------------------------------------------------------------------

newtype DimArray (as :: [Type]) = DimArray
  { array :: Array D (IndexOf as) Float
  }


instance (Index (IndexOf as)) => Eq (DimArray as) where
  DimArray arr == DimArray arr2 = arr == arr2


instance (Ragged L (IndexOf as) Float) => Show (DimArray as) where
  show (DimArray a) = show a


class HasIndex (as :: [Type]) where
  type IndexOf as :: Type


instance HasIndex '[] where
  type IndexOf '[] = Ix0


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
  => DimArray (a : as)
  -> [DimArray as]
outerList (DimArray a) = foldOuterSlice row a
 where
  row :: Array D (IndexOf as) Float -> [DimArray as]
  row r = [DimArray r]


transposeMajor
  :: (IndexOf (a : b : xs) ~ IndexOf (b : a : xs), Index (Lower (IndexOf (b : a : xs))), Index (IndexOf (b : a : xs)))
  => DimArray (a : b : xs)
  -> DimArray (b : a : xs)
transposeMajor (DimArray arr) = DimArray $ transposeInner arr


transposeMinor4
  :: DimArray [a, b, c, d]
  -> DimArray [a, b, d, c]
transposeMinor4 (DimArray arr) = DimArray $ transposeOuter arr


transposeMinor3
  :: DimArray [a, b, c]
  -> DimArray [a, c, b]
transposeMinor3 (DimArray arr) = DimArray $ transposeOuter arr


--  DimArray $ -- foldOuterSlice each arr
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
  -> DimArray (a : xs)
  -> DimArray xs
sliceM0 a (DimArray arr) = DimArray (arr !> a)


-- Slice along the 2nd major dimension
sliceM1
  :: forall a b xs
   . ( Lower (IndexOf (a : b : xs)) ~ IndexOf (a : xs)
     , Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     )
  => Int
  -> DimArray (a : b : xs)
  -> DimArray (a : xs)
sliceM1 b (DimArray arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : xs))) Proxy
   in DimArray $ arr <!> (Dim (dims - 1), b)


-- Slice along the 3rd major dimension
sliceM2
  :: forall a b c xs
   . ( Lower (IndexOf (a : b : c : xs)) ~ IndexOf (a : b : xs)
     , Index (IndexOf (a : b : xs))
     , Index (IndexOf (a : b : c : xs))
     )
  => Int
  -> DimArray (a : b : c : xs)
  -> DimArray (a : b : xs)
sliceM2 c (DimArray arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : c : xs))) Proxy
   in DimArray $ arr <!> (Dim (dims - 2), c)


splitM0
  :: forall a xs m
   . ( Index (IndexOf (a : xs))
     , MonadThrow m
     )
  => Int
  -> DimArray (a : xs)
  -> m (DimArray (a : xs), DimArray (a : xs))
splitM0 a (DimArray arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) a arr
  pure (DimArray arr1, DimArray arr2)


splitM1
  :: forall a b xs m
   . ( Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     , IndexOf (a : b : xs) ~ IndexOf (a : b : xs)
     , IndexOf (a : b : xs) ~ IndexOf (a : b : xs)
     , MonadThrow m
     )
  => Int
  -> DimArray (a : b : xs)
  -> m (DimArray (a : b : xs), DimArray (a : b : xs))
splitM1 b (DimArray arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) b arr
  pure (DimArray arr1, DimArray arr2)

-- sampleGen2 :: Ix2 -> Float
-- sampleGen2 (r :. c) = fromIntegral r * 10 + fromIntegral c
--
--
-- sampleGen3 :: Ix3 -> Float
-- sampleGen3 (d :> d2) = fromIntegral d * 100 + sampleGen2 d2
--
--
-- data DX'
-- data Y'
-- data Z'
--
--
-- sample2 :: Ix2 -> DimArray [Y, X]
-- sample2 ix = DimArray $ makeArray Seq (Sz ix) sampleGen2
--
--
-- sample3 :: Ix3 -> DimArray [Z, Y, X]
-- sample3 ix = DimArray $ makeArray Seq (Sz ix) sampleGen3

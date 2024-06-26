{-# LANGUAGE UndecidableInstances #-}

module NSO.Fits.Generate.DataCube where

import Data.Kind
import Data.Massiv.Array as M hiding (mapM)
import GHC.TypeLits (natVal)
import NSO.Prelude


-- Results ------------------------------------------------------------------------------

newtype DataCube (as :: [Type]) = DataCube
  { array :: Array D (IndexOf as) Float
  }


instance (Index (IndexOf as)) => Eq (DataCube as) where
  DataCube arr == DataCube arr2 = arr == arr2


instance (Ragged L (IndexOf as) Float) => Show (DataCube as) where
  show (DataCube a) = show a


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
  => DataCube (a : as)
  -> [DataCube as]
outerList (DataCube a) = foldOuterSlice row a
 where
  row :: Array D (IndexOf as) Float -> [DataCube as]
  row r = [DataCube r]


transposeMajor
  :: (IndexOf (a : b : xs) ~ IndexOf (b : a : xs), Index (Lower (IndexOf (b : a : xs))), Index (IndexOf (b : a : xs)))
  => DataCube (a : b : xs)
  -> DataCube (b : a : xs)
transposeMajor (DataCube arr) = DataCube $ transposeInner arr


transposeMinor4
  :: DataCube [a, b, c, d]
  -> DataCube [a, b, d, c]
transposeMinor4 (DataCube arr) = DataCube $ transposeOuter arr


transposeMinor3
  :: DataCube [a, b, c]
  -> DataCube [a, c, b]
transposeMinor3 (DataCube arr) = DataCube $ transposeOuter arr


--  DataCube $ -- foldOuterSlice each arr
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
  -> DataCube (a : xs)
  -> DataCube xs
sliceM0 a (DataCube arr) = DataCube (arr !> a)


-- Slice along the 2nd major dimension
sliceM1
  :: forall a b xs
   . ( Lower (IndexOf (a : b : xs)) ~ IndexOf (a : xs)
     , Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     )
  => Int
  -> DataCube (a : b : xs)
  -> DataCube (a : xs)
sliceM1 b (DataCube arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : xs))) Proxy
   in DataCube $ arr <!> (Dim (dims - 1), b)


-- Slice along the 3rd major dimension
sliceM2
  :: forall a b c xs
   . ( Lower (IndexOf (a : b : c : xs)) ~ IndexOf (a : b : xs)
     , Index (IndexOf (a : b : xs))
     , Index (IndexOf (a : b : c : xs))
     )
  => Int
  -> DataCube (a : b : c : xs)
  -> DataCube (a : b : xs)
sliceM2 c (DataCube arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : c : xs))) Proxy
   in DataCube $ arr <!> (Dim (dims - 2), c)


splitM0
  :: forall a xs m
   . ( Index (IndexOf (a : xs))
     , MonadThrow m
     )
  => Int
  -> DataCube (a : xs)
  -> m (DataCube (a : xs), DataCube (a : xs))
splitM0 a (DataCube arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) a arr
  pure (DataCube arr1, DataCube arr2)


splitM1
  :: forall a b xs m
   . ( Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     , IndexOf (a : b : xs) ~ IndexOf (a : b : xs)
     , IndexOf (a : b : xs) ~ IndexOf (a : b : xs)
     , MonadThrow m
     )
  => Int
  -> DataCube (a : b : xs)
  -> m (DataCube (a : b : xs), DataCube (a : b : xs))
splitM1 b (DataCube arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) b arr
  pure (DataCube arr1, DataCube arr2)

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
-- sample2 :: Ix2 -> DataCube [Y, X]
-- sample2 ix = DataCube $ makeArray Seq (Sz ix) sampleGen2
--
--
-- sample3 :: Ix3 -> DataCube [Z, Y, X]
-- sample3 ix = DataCube $ makeArray Seq (Sz ix) sampleGen3

{-# LANGUAGE UndecidableInstances #-}

module NSO.Image.NDCollection where

import NSO.Prelude
import Telescope.Asdf as Asdf


data NDCollection k a = NDCollection
  { toAligned :: [Int] -> k AlignedAxes
  , axes :: [AxisMeta]
  , items :: k a
  }
instance (ToAsdf (k a), ToAsdf (k AlignedAxes)) => ToAsdf (NDCollection k a) where
  schema _ = "tag:sunpy.org:ndcube/ndcube/ndcollection-1.0.0"
  toValue col =
    Object
      [ ("aligned_axes", toNode alignedAxes)
      , ("items", toNode col.items)
      ]
   where
    alignedAxes :: k AlignedAxes
    alignedAxes =
      let aligned = filter ((.aligned) . snd) $ zip [0 ..] col.axes :: [(Int, AxisMeta)]
          axns :: [Int] = fmap fst aligned :: [Int]
       in col.toAligned axns


newtype AxesMeta = AxesMeta [AxisMeta]
instance ToAsdf AxesMeta where
  toValue (AxesMeta axs) =
    Object
      [ ("axes", toNode $ fmap (.label) axs)
      , ("shape", toNode $ fmap (.size) axs)
      ]


data AxisMeta = AxisMeta
  { label :: Text
  , aligned :: Bool
  , size :: Int
  }


newtype AlignedAxes a = AlignedAxes [Int]
instance ToAsdf (AlignedAxes a) where
  toValue (AlignedAxes as) = toValue as


newtype AxisLabel = AxisLabel Text
  deriving newtype (ToAsdf, IsString)

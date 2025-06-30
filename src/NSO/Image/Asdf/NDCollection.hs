{-# LANGUAGE UndecidableInstances #-}

module NSO.Image.Asdf.NDCollection where

import NSO.Prelude
import Telescope.Asdf as Asdf


alignedAxes :: ([Int] -> f AlignedAxes) -> [AxisMeta] -> f AlignedAxes
alignedAxes toAligned axes =
  let aligned = filter ((.aligned) . snd) $ zip [0 ..] axes :: [(Int, AxisMeta)]
      axns :: [Int] = fmap fst aligned :: [Int]
   in toAligned axns


data AxisMeta = AxisMeta
  { label :: Text
  , aligned :: Bool
  }
instance ToAsdf AxisMeta where
  toValue am = toValue am.label


newtype AlignedAxes f = AlignedAxes [Int]
  deriving newtype (ToAsdf)


newtype AxisLabel = AxisLabel Text
  deriving newtype (ToAsdf, IsString)

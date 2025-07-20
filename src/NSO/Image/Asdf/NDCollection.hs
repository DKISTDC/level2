{-# LANGUAGE UndecidableInstances #-}

module NSO.Image.Asdf.NDCollection where

import NSO.Prelude
import Telescope.Asdf as Asdf


alignedAxes :: [AxisMeta] -> AlignedAxes
alignedAxes axes =
  let aligned = filter ((.aligned) . snd) $ zip [0 ..] axes :: [(Int, AxisMeta)]
      axns :: [Int] = fmap fst aligned :: [Int]
   in AlignedAxes axns


data AxisMeta = AxisMeta
  { label :: Text
  , aligned :: Bool
  }
instance ToAsdf AxisMeta where
  toValue am = toValue am.label


newtype AlignedAxes = AlignedAxes {axes :: [Int]}
  deriving newtype (ToAsdf)


newtype AlignedAxesF f = AlignedAxesF {axes :: [Int]}
  deriving newtype (ToAsdf)


newtype AxisLabel = AxisLabel Text
  deriving newtype (ToAsdf, IsString)

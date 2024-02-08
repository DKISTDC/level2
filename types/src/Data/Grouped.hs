module Data.Grouped where

import Data.List as L
import Data.List.NonEmpty as NE
import Prelude hiding (head)

newtype Grouped a b = Grouped {items :: NonEmpty b}
  deriving (Foldable, Functor)

sample :: Grouped a b -> b
sample g = NE.head g.items

grouped :: (Eq b, Ord b) => (a -> b) -> [a] -> [Grouped t a]
grouped f = fmap Grouped . group' . L.sortOn f
 where
  group' [] = []
  group' (a : as) =
    NE.groupWith f (a :| as)

toList :: Grouped a b -> [b]
toList (Grouped as) = NE.toList as

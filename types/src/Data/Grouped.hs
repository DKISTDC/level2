module Data.Grouped where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Prelude hiding (head, id, lookup)


newtype Grouped id a = Grouped
  { items :: NonEmpty a
  }
  deriving (Foldable, Functor)


sample :: Grouped a b -> b
sample g = NE.head g.items


grouped :: (Eq id, Ord id) => (a -> id) -> [a] -> [Grouped t a]
grouped f input =
  let items = group' . L.sortOn f $ input
   in fmap Grouped items
 where
  group' [] = []
  group' (a : as) =
    NE.groupWith f (a :| as)


toList :: Grouped a b -> [b]
toList (Grouped as) = NE.toList as


collate :: (a -> b -> Bool) -> (Grouped id a -> Grouped id b -> c) -> [Grouped id a] -> [Grouped id b] -> [c]
collate eq f gas gbs =
  mapMaybe each gas
 where
  each ga = do
    gb <- find (eq (sample ga)) gbs
    pure $ f ga gb


find :: (a -> Bool) -> [Grouped id a] -> Maybe (Grouped id a)
find p = L.find (p . sample)

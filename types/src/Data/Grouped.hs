module Data.Grouped where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Prelude hiding (head, id, lookup)


newtype Group id a = Group
  { items :: NonEmpty a
  }
  deriving (Foldable, Functor)


sample :: Group a b -> b
sample g = NE.head g.items


grouped :: (Eq id, Ord id) => (a -> id) -> [a] -> [Group id a]
grouped f input =
  let items = group' . L.sortOn f $ input
   in fmap Group items
 where
  group' [] = []
  group' (a : as) =
    NE.groupWith f (a :| as)


toList :: Group a b -> [b]
toList (Group as) = NE.toList as


collate :: (a -> b -> Bool) -> (Group id a -> Group id b -> c) -> [Group id a] -> [Group id b] -> [c]
collate eq f gas gbs =
  mapMaybe each gas
 where
  each ga = do
    gb <- find (eq (sample ga)) gbs
    pure $ f ga gb


find :: (a -> Bool) -> [Group id a] -> Maybe (Group id a)
find p = L.find (p . sample)

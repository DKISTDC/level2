module Data.List.Ext where

import Prelude


(!?) :: [a] -> Int -> Maybe a
as !? n =
  if n >= length as
    then Nothing
    else Just $ as !! n

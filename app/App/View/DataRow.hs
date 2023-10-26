module App.View.DataRow where

import App.Colors
import NSO.Prelude
import Web.UI

dataRows :: [a] -> (a -> View c ()) -> View c ()
dataRows as rw = forM_ (zip (cycle [True, False]) as) $ \(b, a) ->
  el (dataRow . alternateColor b) $ rw a
 where
  alternateColor b = if b then bg Light else id

dataCell :: Mod
dataCell = minWidth 100

dataRow :: Mod
dataRow = gap 10 . pad (All dataRowPadding)

dataRowPadding :: PxRem
dataRowPadding = 5

dataRowHeight :: PxRem
dataRowHeight = 16 + 2 * dataRowPadding

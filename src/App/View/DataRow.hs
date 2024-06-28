module App.View.DataRow where

import App.Colors
import NSO.Prelude
import Web.View


dataRows :: [a] -> (a -> View c ()) -> View c ()
dataRows as rw = forM_ (zip (cycle [True, False]) as) $ \(b, a) ->
  el (dataRow . alternateColor b) $ rw a
 where
  alternateColor b = if b then bg (light Light) else id


dataCell :: Mod
dataCell = minWidth 100

tagCell :: Mod
tagCell = minWidth 120


dataRow :: Mod
dataRow = gap 10 . pad (All $ PxRem dataRowPadding)


dataRowPadding :: PxRem
dataRowPadding = 5


dataRowHeight :: PxRem
dataRowHeight = 16 + 2 * dataRowPadding


table :: Mod
table = odd (bg White) . even (bg (light Light)) . textAlign Center


hd :: View id () -> View (TableHead id) ()
hd = th (pad 4 . bord . bold . bg Light)


cell :: View () () -> View dat ()
cell = td (pad 4 . bord)


bord :: Mod
bord = border 1 . borderColor (light Light)

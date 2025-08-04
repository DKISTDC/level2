module App.View.DataRow where

import App.Colors
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole


dataRows :: [a] -> (a -> View c ()) -> View c ()
dataRows as rw = forM_ (zip (cycle [True, False]) as) $ \(b, a) ->
  el ~ dataRow . alternateColor b $ rw a
 where
  alternateColor b = if b then bg (light Light) else id


dataCell :: (Styleable h) => CSS h -> CSS h
dataCell = minWidth 100


tagCell :: (Styleable h) => CSS h -> CSS h
tagCell = minWidth 120


dataRow :: (Styleable h) => CSS h -> CSS h
dataRow = gap 10 . pad (All $ PxRem dataRowPadding)


dataRowPadding :: PxRem
dataRowPadding = 5


dataRowHeight :: PxRem
dataRowHeight = 16 + 2 * dataRowPadding


table :: (Styleable h) => CSS h -> CSS h
table = odd (bg White) . even (bg (light Light)) . textAlign AlignCenter


hd :: View id () -> TableHead id ()
hd = th ~ pad 4 . bord . bold . bg Light


cell :: View dat () -> View dat ()
cell = td ~ pad 4 . bord


bord :: (Styleable h) => CSS h -> CSS h
bord = border 1 . borderColor (light Light)

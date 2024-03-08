module App.Style where

import App.Colors
import NSO.Prelude
import Web.View
import Web.View.Style


link :: Mod
link = color Primary . hover (color (light Primary) . underline)


page :: Mod
page = pad 20 . gap 25


header :: Mod
header = fontSize 24 . bold


subheader :: Mod
subheader = fontSize 18 . bold


card :: Mod
card = bg White . rounded 4 . truncate


cardHeader :: (Contrast c, ToColor c) => c -> Mod
cardHeader c =
  bg c . color (contrast c) . textAlign Center . pad 10


btn :: AppColor -> Mod
btn c =
  base
    . color (contrast c)
    . bg c
    . hover (bg (light c))
 where
  base = pad (XY 15 10)


btnOutline :: AppColor -> Mod
btnOutline c =
  base
    . border 2
    . borderColor c
    . color c
    . hover (borderColor (light c) . color (light c))
 where
  base = pad (XY 15 8)


underline :: Mod
underline =
  addClass $
    cls "under"
      & prop @Text "text-decoration" "underline"


input :: AppColor -> Mod
input c = pad 8 . border 1 . borderColor c


disabled :: Mod
disabled = opacity 0.5 . att "inert" ""

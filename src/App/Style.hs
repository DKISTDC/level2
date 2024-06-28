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


tag :: AppColor -> Mod
tag c =
  color (contrast c)
    . bg (light c)


tagOutline :: (ToColor c) => c -> Mod
tagOutline c =
  color c
    . borderColor c
    . border 2


btn :: AppColor -> Mod
btn c =
  btnBase
    . color (contrast c)
    . bg c
    . hover (bg (light c))
    . shadow
    . rounded 3


btnOutline :: AppColor -> Mod
btnOutline c =
  btnBase
    . border 2
    . borderColor c
    . color c
    . hover (borderColor (light c) . color (light c))


btnBase :: Mod
btnBase =
  pad (XY 15 10)
    . rounded 3
    . shadow


underline :: Mod
underline =
  addClass $
    cls "under"
      & prop @Text "text-decoration" "underline"


input :: Mod
input = pad 8 . border 1


disabled :: Mod
disabled = opacity 0.5 . att "inert" ""

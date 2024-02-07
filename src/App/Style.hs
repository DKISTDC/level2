module App.Style where

import App.Colors
import NSO.Prelude
import Web.View


link :: Mod
link = color Primary . hover (color (light Primary))


page :: Mod
page = pad 20 . gap 25


header :: Mod
header = fontSize 24 . bold


subheader :: Mod
subheader = fontSize 18 . bold


card :: Mod
card = bg White . rounded 4 . truncate


cardHeader :: AppColor -> Mod
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

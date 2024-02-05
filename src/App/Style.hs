module App.Style where

import App.Colors
import NSO.Prelude
import Web.View


link :: Mod
link = color Primary . hover (color PrimaryLight)


page :: Mod
page = pad 20 . gap 25


header :: Mod
header = fontSize 24 . bold


subheader :: Mod
subheader = fontSize 18 . bold


btn :: Mod
btn = color White . pad (XY 15 10) . bg Secondary . hover (bg SecondaryLight)

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

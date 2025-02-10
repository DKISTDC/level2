module App.Style where

import App.Colors
import NSO.Prelude
import Web.View
import Web.View.Style


link :: Mod c
link = color Primary . hover (color (light Primary) . underline)


page :: Mod c
page = pad 20 . gap 25


header :: Mod c
header = fontSize 24 . bold


code :: Mod c
code = fontSize 14


subheader :: Mod c
subheader = fontSize 18 . bold


card :: Mod c
card = bg White . rounded 4 . truncate


cardHeader :: (Contrast clr, ToColor clr) => clr -> Mod ctx
cardHeader c =
  bg c . color (contrast c) . textAlign AlignCenter . pad 10


tag :: AppColor -> Mod c
tag c =
  color (contrast c)
    . bg (light c)


tagOutline :: (ToColor clr) => clr -> Mod ctx
tagOutline c =
  color c
    . borderColor c
    . border 2


btn :: AppColor -> Mod c
btn c =
  btnBase
    . color (contrast c)
    . bg c
    . hover (bg (light c))
    . shadow ()
    . rounded 3


btnOutline :: AppColor -> Mod c
btnOutline c =
  btnBase
    . border 2
    . borderColor c
    . color c
    . hover (borderColor (light c) . color (light c))


btnLoading :: AppColor -> Mod c
btnLoading c =
  btnBase
    . border 2
    . borderColor c
    . color c
    . att "disabled" ""


btnBase :: Mod c
btnBase =
  pad (XY 15 10)
    . rounded 3
    . shadow ()


italic :: Mod c
italic =
  addClass $
    cls "italic"
      & prop @Text "font-style" "italic"


input :: Mod c
input = pad 8 . border 1


-- disabled :: Mod c
-- disabled = opacity 0.5 . att "inert" ""

disabled :: Mod c
disabled = noClick . opacity 0.5


flexWrap :: Mod c
flexWrap =
  addClass $
    cls "fwrap"
      & prop @Text "flex-wrap" "wrap"


alignMiddle :: Mod c
alignMiddle =
  addClass $
    cls "amid"
      & prop @Text "align-self" "center"


pointer :: Mod c
pointer =
  addClass $
    cls "point"
      & prop @Text "cursor" "pointer"


noClick :: Mod c
noClick = addClass $ cls "no-click" & prop @Text "pointer-events" "none"

module App.Style where

import App.Colors
import NSO.Prelude
import Web.Atomic.Attributes
import Web.Atomic.CSS


link :: (Styleable h) => CSS h -> CSS h
link = color Primary . hover (color (light Primary) . underline)


blank :: (Attributable h) => Attributes h -> Attributes h
blank = att "target" "_blank"


page :: (Styleable h) => CSS h -> CSS h
page = pad 20 . gap 25


header :: (Styleable h) => CSS h -> CSS h
header = fontSize 24 . bold


code :: (Styleable h) => CSS h -> CSS h
code = fontSize 14


subheader :: (Styleable h) => CSS h -> CSS h
subheader = fontSize 18 . bold


card :: (Styleable h) => CSS h -> CSS h
card = bg White . rounded 4 . overflow Clip


cardHeader :: (Contrast clr, ToColor clr) => clr -> (Styleable h) => CSS h -> CSS h
cardHeader c =
  bg c . color (contrast c) . textAlign AlignCenter . pad 10


tag :: (Styleable h) => AppColor -> CSS h -> CSS h
tag c =
  color (contrast c)
    . bg (light c)


tagOutline :: (Styleable h) => (ToColor clr) => clr -> CSS h -> CSS h
tagOutline c =
  color c
    . borderColor c
    . border 2


btn :: (Styleable h) => AppColor -> CSS h -> CSS h
btn c =
  btnBase
    . color (contrast c)
    . bg c
    . hover (bg (light c))
    . shadow ()
    . rounded 3


btnOutline :: (Styleable h) => AppColor -> CSS h -> CSS h
btnOutline c =
  btnBase
    . border 2
    . borderColor c
    . color c
    . hover (borderColor (light c) . color (light c))


btnLoading :: (Styleable h) => AppColor -> CSS h -> CSS h
btnLoading c =
  btnBase
    . border 2
    . borderColor c
    . color c


btnBase :: (Styleable h) => CSS h -> CSS h
btnBase =
  pad (XY 15 10)
    . rounded 3
    . shadow ()


input :: (Styleable h) => CSS h -> CSS h
input = pad 8 . border 1


-- disabled :: Mod c
-- disabled = opacity 0.5 . att "inert" ""

disabled :: (Styleable h) => CSS h -> CSS h
disabled = noClick . opacity 0.5


alignMiddle :: (Styleable h) => CSS h -> CSS h
alignMiddle =
  utility "amid" ["align-self" :. "center"]


noClick :: (Styleable h) => CSS h -> CSS h
noClick = utility "no-click" ["pointer-events" :. "none"]


noWrap :: (Styleable h) => CSS h -> CSS h
noWrap = utility "nowrap" ["text-wrap" :. "nowrap"]


big :: (Styleable h) => (CSS h -> CSS h) -> CSS h -> CSS h
big = media (MinWidth 1000)


small :: (Styleable h) => (CSS h -> CSS h) -> CSS h -> CSS h
small = media (MaxWidth 1000)

module App.View.Loading where

import App.Colors
import App.Style (card)
import App.View.Icons (skeleton)
import NSO.Prelude
import Web.Hyperbole


loadingCard :: View c ()
loadingCard = do
  el (width 400) skeleton

module App.View.Loading where

import App.View.Icons (skeleton)
import Web.Hyperbole


loadingCard :: View c ()
loadingCard = do
  el (width 400) skeleton

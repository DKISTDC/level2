module App.View.Loading where

import App.Style (card)
import App.View.Icons (skeleton)
import NSO.Prelude
import Web.Hyperbole


loadingCard :: View c ()
loadingCard = do
  el (pad 15 . card) $ do
    skeleton

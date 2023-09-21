module App.Page.Dashboard where

import NSO.Prelude
import Web.Scotty

route :: ScottyM ()
route = do
  get "/" $ do
    html "THIS IS MY MAIN PAGE YO"

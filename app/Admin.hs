{-# LANGUAGE QuasiQuotes #-}

module Admin where

import Data.String.Interpolate
import Data.Text.Lazy qualified as L
import Lucid
import NSO.Prelude
import Web.Scotty

-- import Lucid.Htmx
-- import Text.RawString.QQ

main :: IO ()
main = scotty 3000 $ do
  post "/clicked" $ do
    html $ renderText $ div_ "SWAP"

  get "/contact/1" $ do
    html $
      documentText
        [i|
          <div hx-target="this" hx-swap="outerHTML">
            <div><label>First Name</label>: Joe</div>
            <div><label>Last Name</label>: Blow</div>
            <div><label>Email</label>: joe@blow.com</div>
            <button hx-get="/contact/1/edit" class="btn btn-primary">
            Click To Edit
            </button>
          </div>
      |]

  get "/contact/1/edit" $ do
    html
      [i|
        <form hx-put="/contact/1" hx-target="this" hx-swap="outerHTML">
          <div>
            <label>First Name</label>
            <input type="text" name="firstName" value="Joe">
          </div>
          <div class="form-group">
            <label>Last Name</label>
            <input type="text" name="lastName" value="Blow">
          </div>
          <div class="form-group">
            <label>Email Address</label>
            <input type="email" name="email" value="joe@blow.com">
          </div>
          <button class="btn">Submit</button>
          <button class="btn" hx-get="/contact/1">Cancel</button>
        </form>
      |]

  put "/contact/1" $ do
    liftIO $ putStrLn "SAVE!"
    ps <- params
    liftIO $ print ps

document :: Html () -> Html ()
document b = do
  html_ $ do
    -- <script src="https://unpkg.com/htmx.org@1.9.5" integrity=" crossorigin="anonymous"></script>
    head_ $ script_ [src_ "https://unpkg.com/htmx.org@1.9.5", integrity_ "sha384-xcuj3WpfgjlKF+FXhSQFQ0ZNr39ln+hwjN3npfM9VBnUskLolQAcN80McRIVOPuO", crossorigin_ "anonymous"] ("" :: Text)
    body_ b

documentText :: L.Text -> L.Text
documentText b = do
  [i| <html>
    <head>
      <script src="https://unpkg.com/htmx.org@1.9.5" integrity=" crossorigin="anonymous"></script>
    </head>
    <body>
      #{b}
    </body>
  |]

module App.View.LiveInput where

import App.Style qualified as Style
import App.View.Icons qualified as Icons
import NSO.Prelude
import Web.Hyperbole
import Web.View.Style (addClass, cls, prop)


liveInput :: (ViewAction (Action id)) => (Text -> Action id) -> Mod id -> View id ()
liveInput toAction f = do
  row relative $ do
    search toAction 250 (f . Style.input . onRequest Style.disabled)
    row (absolute . right . Style.noClick) $ do
      el (hide . onRequest flexRow) loader
 where
  relative = addClass $ cls "pos-rel" & prop @Text "position" "relative"
  absolute = addClass $ cls "pos-abs" & prop @Text "position" "absolute"
  right =
    addClass $
      cls "pos-right"
        & prop @PxRem "top" 5
        & prop @PxRem "right" 5
        & prop @PxRem "bottom" 5


liveTextArea :: (ViewAction (Action id)) => (Text -> Action id) -> Mod id -> Text -> View id ()
liveTextArea toAction f t = do
  stack id $ do
    layer id $ tag "textarea" (f . onInput toAction 500 . Style.input) (text t)
    layer (Style.noClick . pad 5 . hide . onRequest flexRow) $ do
      space
      el id loader


loader :: View c ()
loader = el (grow . width 32 . height 32) Icons.spinnerCircle

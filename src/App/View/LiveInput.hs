module App.View.LiveInput where

import App.Style qualified as Style
import App.View.Icons qualified as Icons
import NSO.Prelude
import Web.Hyperbole
import Web.View.Style (addClass, cls, prop)


liveInput :: (ViewAction (Action id)) => (Text -> Action id) -> Mod id -> View id ()
liveInput toAction f = do
  row relative $ do
    search toAction 250 (f . Style.input . onLoading Style.disabled)
    row (absolute . right . noClick) $ do
      el (hide . onRequest flexRow) loader
 where
  relative = addClass $ cls "pos-rel" & prop @Text "position" "relative"
  absolute = addClass $ cls "pos-abs" & prop @Text "position" "absolute"
  noClick = addClass $ cls "no-click" & prop @Text "pointer-events" "none"
  loader = el grow Icons.spinnerCircle
  right =
    addClass $
      cls "pos-right"
        & prop @PxRem "top" 5
        & prop @PxRem "right" 5
        & prop @PxRem "bottom" 5


onLoading :: Mod c -> Mod c
onLoading l = do
  parent "hyp-loading" l

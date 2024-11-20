module App.View.LiveInput where

import NSO.Prelude
import Web.Hyperbole
import App.View.Icons qualified as Icons
import App.Style qualified as Style
import Web.View.Style (addClass, cls, prop)


liveInput :: (HyperView id) => (Text -> Action id) -> Mod -> View id ()
liveInput toAction f = do
  row relative $ do
    search toAction 250 (f . Style.input . onLoading Style.disabled)
    row (absolute . right . noClick) $ do
      onRequest loader none
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


onLoading :: Mod -> Mod
onLoading l = do
  parent "hyp-loading" l

module App.View.LiveInput where

import App.Style qualified as Style
import App.View.Icons qualified as Icons
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole


liveInput :: (ViewAction (Action id)) => (Text -> Action id) -> View id ()
liveInput toAction = do
  row ~ position Relative $ do
    search toAction 250 ~ Style.input . whenLoading Style.disabled
    row ~ position Absolute . alignRight . Style.noClick $ do
      el ~ display None . whenLoading flexRow $ loader
 where
  alignRight = top 5 . right 5 . bottom 5


liveTextArea :: (ViewAction (Action id)) => (Text -> Action id) -> Text -> View id ()
liveTextArea toAction t = do
  el ~ stack $ do
    tag "textarea" @ onInput toAction 500 ~ Style.input $ text t
    row ~ Style.noClick . pad 5 . display None . whenLoading (display Flex) $ do
      space
      loader


loader :: View c ()
loader = el ~ grow . width 32 . height 32 $ Icons.spinnerCircle

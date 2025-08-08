module App.View.Loading where

import App.Style qualified as Style
import App.View.Icons (skeleton)
import App.View.Icons qualified as Icons
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole


loadingCard :: View c ()
loadingCard = do
  el ~ width 400 $ skeleton


inputLoader :: View id () -> View id ()
inputLoader inp = do
  el ~ stack $ do
    inp ~ whenLoading Style.disabled
    row ~ Style.noClick . pad 5 $ do
      space
      el ~ display None . whenLoading (display Block) $ loader


liveTextArea :: (ViewAction (Action id)) => (Text -> Action id) -> Text -> View id ()
liveTextArea toAction t = do
  el ~ stack $ do
    tag "textarea" @ onInput toAction 500 ~ Style.input $ text t
    row ~ Style.noClick . pad 5 . display None . whenLoading (display Flex) $ do
      space
      loader


loader :: View c ()
loader = el ~ grow . width 32 . height 32 $ Icons.spinnerCircle

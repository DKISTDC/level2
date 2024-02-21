module App.Types where

import Data.Tagged
import NSO.Prelude
import Network.Wai.Handler.Warp (Port)


-- | The domain of our app
type AppDomain = Tagged "AppDomain" Text


data App = App
  { port :: Port
  , domain :: AppDomain
  }

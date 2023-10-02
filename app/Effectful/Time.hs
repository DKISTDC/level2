module Effectful.Time
  ( UTCTime
  , Time
  , currentTime
  , runTime
  ) where

import Data.Time.Clock
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Prelude

data Time :: Effect where
  CurrentTime :: Time m UTCTime

type instance DispatchOf Time = 'Dynamic

currentTime :: (Time :> es) => Eff es UTCTime
currentTime = send CurrentTime

runTime
  :: (IOE :> es)
  => Eff (Time : es) a
  -> Eff es a
runTime = interpret $ \_ -> \case
  CurrentTime -> liftIO getCurrentTime

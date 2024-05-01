module Effectful.GenRandom
  ( GenRandom (..)
  , Random
  , randomValue
  , randomRange
  , randomFromList
  , runGenRandom
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import System.Random (Random, randomIO, randomRIO)
import Prelude


data GenRandom :: Effect where
  RandValue :: (Random a) => GenRandom m a
  RandRange :: (Random a) => a -> a -> GenRandom m a
  RandFromList :: (Random a) => [a] -> GenRandom m a


type instance DispatchOf GenRandom = 'Dynamic


randomValue :: (GenRandom :> es, Random a) => Eff es a
randomValue = send RandValue


randomRange :: (GenRandom :> es) => (Random a) => a -> a -> Eff es a
randomRange mn mx = send $ RandRange mn mx


randomFromList :: (GenRandom :> es) => (Random a) => [a] -> Eff es a
randomFromList as = send $ RandFromList as


runGenRandom
  :: (IOE :> es)
  => Eff (GenRandom : es) a
  -> Eff es a
runGenRandom = interpret $ \_ -> \case
  RandValue -> liftIO randomIO
  RandRange mn mx -> liftIO $ randomRIO (mn, mx)
  RandFromList as -> do
    index <- liftIO $ randomRIO (0, length as - 1)
    pure $ as !! index

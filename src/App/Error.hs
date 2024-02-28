module App.Error where

import Control.Exception
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Prelude
import Web.Hyperbole
import Web.Hyperbole.Effect


data AppError
  = ValidationError String
  deriving (Show, Exception)


expectFound :: (Hyperbole :> es) => [a] -> Eff es (NonEmpty a)
expectFound [] = notFound
expectFound (a : as) = pure $ a :| as


expectAuth :: (Hyperbole :> es) => Maybe a -> Eff es a
expectAuth Nothing = send $ RespondEarly $ Err ErrAuth
expectAuth (Just a) = pure a

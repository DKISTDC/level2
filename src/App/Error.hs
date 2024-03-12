module App.Error where

import Control.Exception
import Effectful
import NSO.Prelude
import Web.Hyperbole


data AppError
  = ValidationError String
  deriving (Show, Exception)


expectFound :: (Hyperbole :> es) => [a] -> Eff es (NonEmpty a)
expectFound [] = notFound
expectFound (a : as) = pure $ a :| as

module App.Error where

import Control.Exception
import Data.Foldable
import Effectful
import NSO.Prelude
import Web.Hyperbole


data AppError
  = ValidationError String
  deriving (Show, Exception)


expectFound :: (Hyperbole :> es, Foldable t) => t a -> Eff es (NonEmpty a)
expectFound as =
  case toList as of
    [] -> notFound
    (h : rest) -> pure $ h :| rest

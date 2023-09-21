module NSO.Types where

import NSO.Prelude

newtype Endpoint = Endpoint Text
  deriving (Show, Eq)

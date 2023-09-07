module NSO.Types where

import Data.Text

newtype Endpoint = Endpoint Text
  deriving (Show, Eq)

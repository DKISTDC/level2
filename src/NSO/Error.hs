module NSO.Error where

import Control.Exception
import NSO.Prelude


data DataError
  = ValidationError String
  deriving (Show, Exception)

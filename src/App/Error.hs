module App.Error where

import Control.Exception
import NSO.Prelude


data AppError
  = ValidationError String
  deriving (Show, Exception)

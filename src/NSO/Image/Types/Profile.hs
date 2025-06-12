module NSO.Image.Types.Profile where

import Telescope.Data.KnownText


data ProfileType
  = Original
  | Fit


instance KnownText Original where
  knownText = "Original"
instance KnownText Fit where
  knownText = "Fit"

module NSO.Image.Asdf.Ref where

import NSO.Image.Types.Quantity
import NSO.Prelude
import Telescope.Asdf
import Telescope.Data.KnownText


data Ref ref = Ref
instance (KnownText ref) => ToAsdf (Ref ref) where
  toValue _ =
    Alias $ Anchor $ knownText @ref
instance (KnownText ref) => KnownText (Ref ref) where
  knownText = knownText @ref


instance ToAsdf (Quantities Ref)

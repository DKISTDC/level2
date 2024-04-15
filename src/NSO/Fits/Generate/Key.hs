module NSO.Fits.Generate.Key where

import Data.Kind
import GHC.TypeLits
import NSO.Prelude


data Key (typ :: Type) (description :: Symbol) where
  KeySeconds :: Float -> Key Seconds d
  KeyDeg :: Float -> Key Deg d
  KeyBool :: Bool -> Key Bool d


newtype MB = MB Float
newtype Seconds = Seconds Float
newtype Deg = Deg Float
data Constant (s :: Symbol) = Constant

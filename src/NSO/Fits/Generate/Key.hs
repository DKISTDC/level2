{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.Key where

import Data.Kind
import GHC.TypeLits
import NSO.Prelude


data Key typ (description :: Symbol)
  = KFloat Float
  | KString String
  | KBool Bool
  | KConst


data Doc key (description :: Symbol) = Doc


-- KeySeconds :: Float -> Key Seconds d
-- KeyDeg :: Float -> Key Deg d
-- KeyBool :: Bool -> Key Bool d
-- data KeyUnit
--   = MB Float
--   | Seconds Float
--   | Degrees Float
--   | ExtName String

newtype MB = MB Float
newtype Seconds = Seconds Float
newtype Degrees = Degrees Float
newtype ExtName = ExtName Float


data UCD
  = MagField
  | DopplerVeloc


-- well, they are ALL floats
data BUnit
  = Dimensionless
  | Kelvin
  | N_m2
  | Km_s
  | Tesla
  | Deg
  | Km
  | Kg_m3


-- newtype MB = MB Float
-- newtype Seconds = Seconds Float
-- newtype Deg = Deg Float
-- newtype ExtName = ExtName String
data Constant (s :: Symbol) = Constant


class KeyValue a where
  keyValue :: String
instance KeyValue MagField where
  keyValue = "phys.magField"

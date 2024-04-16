{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.Key where

import Data.Kind
import GHC.TypeLits
import NSO.Prelude


-- data Key typ (description :: Symbol)
--   = KFloat Float
--   | KString String
--   | KBool Bool
--   | KConst

-- they probably have to be the same order
data Doc ktype (description :: Symbol) = Doc
data Key ktype (comment :: Symbol) = Key ktype


-- data Key ktype = Key

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
newtype ExtName = ExtName Text


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
instance Show BUnit where
  show Dimensionless = ""
  show Kelvin = "K"
  show N_m2 = "N/m^2"
  show Km_s = "km/s"
  show Tesla = "T"
  show Deg = "deg"
  show Km = "km"
  show Kg_m3 = "kg/m^3"


-- newtype MB = MB Float
-- newtype Seconds = Seconds Float
-- newtype Deg = Deg Float
-- newtype ExtName = ExtName String
data Constant (s :: Symbol) = Constant

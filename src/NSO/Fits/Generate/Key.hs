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
-- data Doc ktype (description :: Symbol) = Doc
--   deriving (Generic)
data Key (keyword :: Symbol) ktype (description :: Symbol) = Key ktype
data KeyList (items :: [Type])


data Constant c = Constant


-- data Key ktype = Key

-- KeySeconds :: Float -> Key Seconds d
-- KeyDeg :: Float -> Key Deg d
-- KeyBool :: Bool -> Key Bool d
-- data KeyUnit
--   = MB Float
--   | Seconds Float
--   | Degrees Float
--   | ExtName String

newtype MB = MB Float deriving (Generic)
newtype Seconds = Seconds Float deriving (Generic)
newtype Degrees = Degrees Float deriving (Generic)
data ExtName (ext :: Symbol) = ExtName
  deriving (Generic)


data BType (ucd :: UCD) = BType deriving (Generic)
data BUnit (unit :: Unit) = BUnit deriving (Generic)


data UCD
  = MagField
  | DopplerVeloc
  | OpticalDepth
  | Temperature
  | ElectronPressure
  deriving (Generic)


fromUCD :: UCD -> String
fromUCD MagField = "phys.magField"
fromUCD DopplerVeloc = "phys.dopplerVeloc"
fromUCD OpticalDepth = "phys.absorption.opticalDepth"
fromUCD Temperature = "phys.temperature"
fromUCD ElectronPressure = "phys.electron;phys.pressure"


-- well, they are ALL floats
data Unit
  = Dimensionless
  | Kelvin
  | N_m2
  | Km_s
  | Tesla
  | Deg
  | Km
  | Kg_m3
  deriving (Generic)
instance Show Unit where
  show Dimensionless = ""
  show Kelvin = "K"
  show N_m2 = "N/m^2"
  show Km_s = "km/s"
  show Tesla = "T"
  show Deg = "deg"
  show Km = "km"
  show Kg_m3 = "kg/m^3"

-- class Woot a where
--   woot :: a -> String
--
--
-- class Name a where
--   name :: a -> String
--
--
-- class ConstantValue a where
--   value :: Proxy a -> String
--
--
-- data Thing a = Thing a
-- data Constant' val = Constant'
--
--
-- instance {-# OVERLAPPABLE #-} (Name a) => Woot (Thing a) where
--   woot (Thing a) = name a
--
--
-- instance {-# OVERLAPPABLE #-} (ConstantValue a) => Woot (Thing (Constant' a)) where
--   woot _ = value @a Proxy
--
--
-- instance ConstantValue () where
--   value _ = "()"
--
--
-- test :: String
-- test = woot (Thing (Constant' @()))

module NSO.Fits.Generate.Types where

import GHC.TypeLits
import NSO.Prelude


data NamedKey (keyword :: Symbol) ktype (description :: Symbol) = NamedKey ktype
data Key ktype (description :: Symbol) = Key ktype
  deriving (Generic)


data Constant c = Constant


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

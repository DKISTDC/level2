{-# OPTIONS_GHC -fno-warn-orphans #-}

module NSO.Image.Types.Quantity where

import NSO.Image.Headers.DataCommon
import NSO.Image.Headers.Types
import NSO.Prelude
import Telescope.Data.KnownText


type OpticalDepth =
  DataHDUInfo
    "Log of Optical Depth at 500nm"
    "phys.absorption.opticalDepth"
    'Dimensionless
instance KnownText OpticalDepth where
  knownText = "OpticalDepth"


type Temperature =
  DataHDUInfo
    "Temperature"
    "phys.temperature"
    'Kelvin
instance KnownText Temperature where
  knownText = "Temperature"


type ElectronPressure =
  DataHDUInfo
    "Electron Pressure"
    "phys.electron;phys.pressure"
    'N_m2
instance KnownText ElectronPressure where
  knownText = "ElectronPressure"


type Microturbulence =
  DataHDUInfo
    "Microturbulence"
    "phys.veloc.microTurb"
    'Km_s
instance KnownText Microturbulence where
  knownText = "Microturbulence"


type MagStrength =
  DataHDUInfo
    "Magnetic Field Strength"
    "phys.magField"
    Tesla
instance KnownText MagStrength where
  knownText = "MagStrength"


type Velocity =
  DataHDUInfo
    "Line-of-sight Velocity"
    "spect.dopplerVeloc"
    Km_s
instance KnownText Velocity where
  knownText = "Velocity"


type MagInclination =
  DataHDUInfo
    "Magnetic Field Inclination (w.r.t. line-of-sight)"
    "phys.magField;pos.angDistance"
    Deg
instance KnownText MagInclination where
  knownText = "MagInclination"


type MagAzimuth =
  DataHDUInfo
    "Magnetic Field Azimuth (w.r.t. line-of-sight)"
    "phys.magField;pos.azimuth"
    Deg
instance KnownText MagAzimuth where
  knownText = "MagAzimuth"


type GeoHeight =
  DataHDUInfo
    "Geometric Height above solar surface (tau ~ 1 at 500nm)"
    "phys.distance"
    Km
instance KnownText GeoHeight where
  knownText = "GeoHeight"


type GasPressure =
  DataHDUInfo
    "Gas Pressure"
    "phys.pressure"
    N_m2
instance KnownText GasPressure where
  knownText = "GasPressure"


type Density =
  DataHDUInfo
    "Density"
    "phys.density"
    Kg_m3
instance KnownText Density where
  knownText = "Density"


data Quantities (f :: Type -> Type) = Quantities
  { opticalDepth :: f OpticalDepth
  , temperature :: f Temperature
  , electronPressure :: f ElectronPressure
  , microTurbulence :: f Microturbulence
  , magStrength :: f MagStrength
  , velocity :: f Velocity
  , magInclination :: f MagInclination
  , magAzimuth :: f MagAzimuth
  , geoHeight :: f GeoHeight
  , gasPressure :: f GasPressure
  , density :: f Density
  }
  deriving (Generic)


data Quantity

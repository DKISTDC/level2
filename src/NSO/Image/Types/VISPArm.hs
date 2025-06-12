{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Types.VISPArm where

import NSO.Prelude
import NSO.Types.Wavelength (CaIILine (..), SpectralLine (..))


data VISPArmLine
  = ArmFeI
  | ArmCa854
  | ArmNaD


data VISPArm (arm :: VISPArmLine) unit


class ArmProfile (arm :: VISPArmLine) where
  armSpectralLine :: SpectralLine
instance ArmProfile ArmFeI where
  armSpectralLine = FeI
instance ArmProfile ArmCa854 where
  armSpectralLine = CaII CaII_854
instance ArmProfile ArmNaD where
  armSpectralLine = NaD


data VISPArms (f :: VISPArmLine -> Type) = VISPArms
  { armFeI :: f ArmFeI
  , armCaII :: f ArmCa854
  , armNaD :: f ArmNaD
  }

module NSO.Data.Spectral where

import NSO.Data.Types
import NSO.Prelude

data SpectralLine
  = HeI
  | Ha
  | FeI
  | CaII CaIILine

instance Show SpectralLine where
  show HeI = "HeI"
  show Ha = "Hα"
  show FeI = "FeI"
  show (CaII l) = "CaII " <> show l

data CaIILine
  = CaII_849
  | CaII_854
  | CaII_866
  deriving (Bounded, Enum)

instance Show CaIILine where
  show CaII_849 = "849"
  show CaII_854 = "854"
  show CaII_866 = "866"

midPoint :: SpectralLine -> Wavelength Nm
midPoint HeI = Wavelength 108.30
midPoint Ha = Wavelength 656.2
midPoint FeI = Wavelength 630.2
midPoint (CaII CaII_849) = Wavelength 849.8
midPoint (CaII CaII_854) = Wavelength 854.2
midPoint (CaII CaII_866) = Wavelength 866.2

matchesLine :: Wavelength Nm -> Wavelength Nm -> SpectralLine -> Bool
matchesLine mn mx s =
  let md = midPoint s
   in mn <= md && md <= mx

identifyLine :: Wavelength Nm -> Wavelength Nm -> Maybe SpectralLine
identifyLine mn mx = find (matchesLine mn mx) allLines
 where
  allLines = [HeI, Ha, FeI] <> fmap CaII [minBound .. maxBound]

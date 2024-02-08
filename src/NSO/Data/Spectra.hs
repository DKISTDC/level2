module NSO.Data.Spectra where

import Data.List.NonEmpty qualified as NE
import NSO.Data.Datasets
import NSO.Prelude


-- | See https://bitbucket.org/dkistdc/dkist-spectral-lines/src/main/dkist_spectral_lines/lines.py
identifyLine :: Wavelength Nm -> Wavelength Nm -> Maybe SpectralLine
identifyLine mn mx = find matchesLine allLines
 where
  allLines = [HeI, Ha, FeI] <> fmap CaII [minBound .. maxBound]

  midPoint :: SpectralLine -> Wavelength Nm
  midPoint HeI = Wavelength 1083.0
  midPoint Ha = Wavelength 656.2
  midPoint FeI = Wavelength 630.2
  midPoint (CaII CaII_849) = Wavelength 849.8
  midPoint (CaII CaII_854) = Wavelength 854.2
  midPoint (CaII CaII_866) = Wavelength 866.2

  matchesLine :: SpectralLine -> Bool
  matchesLine s =
    let md = midPoint s
     in mn <= md && md <= mx


identifyLines :: NonEmpty Dataset -> [SpectralLine]
identifyLines = mapMaybe (\d -> identifyLine d.wavelengthMin d.wavelengthMax) . NE.toList

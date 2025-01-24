module NSO.Data.Spectra where

import NSO.Data.Datasets hiding (find)
import NSO.Prelude


-- | See https://bitbucket.org/dkistdc/dkist-spectral-lines/src/main/dkist_spectral_lines/lines.py
lineForWaves :: Wavelength Nm -> Wavelength Nm -> Maybe SpectralLine
lineForWaves mn mx = find matchesLine allLines
 where
  allLines = [HeI, Ha, FeI] <> fmap CaII [minBound .. maxBound]

  matchesLine :: SpectralLine -> Bool
  matchesLine s =
    let md = midPoint s
     in mn <= md + wiggleRoom && md <= mx
   where
    wiggleRoom :: Wavelength Nm
    wiggleRoom = Wavelength 1


identifyLines :: [Dataset] -> [SpectralLine]
identifyLines = mapMaybe identifyLine


identifyLine :: Dataset -> Maybe SpectralLine
identifyLine d = lineForWaves d.wavelengthMin d.wavelengthMax


midPoint :: SpectralLine -> Wavelength Nm
midPoint HeI = Wavelength 1083.0
midPoint Ha = Wavelength 656.2
midPoint FeI = Wavelength 630.149
midPoint (CaII CaII_849) = Wavelength 849.8
midPoint (CaII CaII_854) = Wavelength 854.2
midPoint (CaII CaII_866) = Wavelength 866.2

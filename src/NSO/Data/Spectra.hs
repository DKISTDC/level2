module NSO.Data.Spectra where

import NSO.Data.Datasets hiding (find)
import NSO.Prelude


-- | See https://bitbucket.org/dkistdc/dkist-spectral-lines/src/main/dkist_spectral_lines/lines.py
lineForWaves :: Wavelength Nm -> Wavelength Nm -> Maybe SpectralLine
lineForWaves mn mx = find matchesLine allLines
 where
  allLines = [minBound .. maxBound]

  matchesLine :: SpectralLine -> Bool
  matchesLine s =
    let md = midPoint s
     in mn <= md + wiggleRoom && md - wiggleRoom <= mx
   where
    wiggleRoom :: Wavelength Nm
    wiggleRoom = Wavelength 1


identifyLines :: [Dataset] -> [SpectralLine]
identifyLines = mapMaybe identifyLine


identifyLine :: Dataset -> Maybe SpectralLine
identifyLine d = lineForWaves d.wavelengthMin d.wavelengthMax


midPoint :: SpectralLine -> Wavelength Nm
midPoint NaID = Wavelength 589.899
midPoint FeI630 = Wavelength 630.150
midPoint CaII854 = Wavelength 854.209

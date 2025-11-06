module NSO.Data.Spectra where

import NSO.Data.Datasets hiding (find)
import NSO.Prelude
import NSO.Types.Wavelength


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
-- range: all 3 lines: 1nm below and above, minimum
midPoint NaID = Wavelength 589.3 -- 589.6, 589.3
-- range: at least half a nanometer below and above
midPoint FeI630 = Wavelength 630.3 -- midpoint between lines
-- range: 1nm below and above
midPoint CaII854 = Wavelength 854.209 -- actually the line, not just the midpoint


isLine :: SpectralLine -> Dataset -> Bool
isLine l d =
  identifyLine d == Just l

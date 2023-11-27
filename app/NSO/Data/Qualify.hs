module NSO.Data.Qualify where

import Data.Grouped
import NSO.Data.Dataset
import NSO.Data.Spectra
import NSO.Prelude
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength

-- TODO: Rigorously. 2D cartesian plane. Pythagorean triangle. That can be more than 900
-- TODO: send Frazer a bad one
-- BUG: 596 isn't real. There's a bug with the way I'm calculating the tags
--
--
-- up to 8-10 spectral lines with the VISP at this point
-- 946-948nm
-- 596nm
isOnDisk :: Maybe BoundingBox -> Bool
isOnDisk Nothing = False
isOnDisk (Just bb) =
  let (xu, yu) = bb.upperRight
      (xl, yl) = bb.lowerLeft
   in not
        $ (xl < -radius)
        || (yl < -radius)
        || (xu > radius)
        || (yu > radius)
 where
  -- rough radius of the sun in arcseconds
  -- https://nssdc.gsfc.nasa.gov/planetary/factsheet/sunfact.html
  radius = 900

isQualified :: Grouped InstrumentProgram Dataset -> Bool
isQualified = either (const False) (const True) . qualify

qualify :: Grouped InstrumentProgram Dataset -> Either String ()
qualify g = do
  case (sample g).instrument of
    VISP -> qualifyVISP g
    VBI -> qualifyVBI g

qualifyVISP :: Grouped InstrumentProgram Dataset -> Either String ()
qualifyVISP g = do
  -- let ds = NE.toList ip.datasets
  let sls = identifyLines g.items
  check "On Disk" $ qualifyOnDisk g
  check "FeI" $ qualifyLine FeI sls
  check "CaII 854" $ qualifyLine (CaII CaII_854) sls
  check "Stokes" $ qualifyStokes g
  check "Health" $ qualifyHealth g
  check "GOS" $ qualifyGOS g
  check "AO" $ qualifyAO g
 where
  check e b = if b then pure () else Left e

qualifyVBI :: Grouped InstrumentProgram Dataset -> Either String ()
qualifyVBI _ = Left "VBI Not supported"

qualifyStokes :: Grouped InstrumentProgram Dataset -> Bool
qualifyStokes g = all (\d -> d.stokesParameters == StokesParameters [I, Q, U, V]) g.items

qualifyOnDisk :: Grouped InstrumentProgram Dataset -> Bool
qualifyOnDisk g = all (\d -> isOnDisk d.boundingBox) g.items

qualifyLine :: SpectralLine -> [SpectralLine] -> Bool
qualifyLine sl sls = sl `elem` sls

-- Frazer: metric have to meet. Have to meet in each of the wavelength channels. As opposed to the set combined.
qualifyHealth :: Grouped InstrumentProgram Dataset -> Bool
qualifyHealth = all (hasPctGood 0.75)
 where
  hasPctGood :: Float -> Dataset -> Bool
  hasPctGood p d = (fromIntegral (fromMaybe 0 d.health.good) / fromIntegral d.frameCount) >= p

qualifyGOS :: Grouped InstrumentProgram Dataset -> Bool
qualifyGOS g = all allOpen g.items
 where
  allOpen d = fromMaybe 0 d.gosStatus.open == fromIntegral d.frameCount

qualifyAO :: Grouped InstrumentProgram Dataset -> Bool
qualifyAO = all (hasPctLocked 0.75)
 where
  hasPctLocked :: Float -> Dataset -> Bool
  hasPctLocked p d = (fromIntegral d.aoLocked / fromIntegral d.frameCount) >= p

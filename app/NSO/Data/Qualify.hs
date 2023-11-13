module NSO.Data.Qualify where

import Data.Grouped
import NSO.Data.Dataset
import NSO.Data.Spectra
import NSO.Prelude
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength

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

-- TODO: implement once available
qualifyHealth :: Grouped InstrumentProgram Dataset -> Bool
qualifyHealth _ = True

qualifyGOS :: Grouped InstrumentProgram Dataset -> Bool
qualifyGOS _ = True

qualifyAO :: Grouped InstrumentProgram Dataset -> Bool
qualifyAO _ = True
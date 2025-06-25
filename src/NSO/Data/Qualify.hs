module NSO.Data.Qualify where

import Data.Grouped
import Data.List.NonEmpty qualified as NE
import Data.Time
import Data.Time.Calendar.OrdinalDate (DayOfYear, toOrdinalDate)
import NSO.Data.Datasets
import NSO.Data.Spectra
import NSO.Prelude
import NSO.Types.InstrumentProgram


isOnDisk :: DayOfYear -> BoundingBox -> Bool
isOnDisk d = isOnDisk' (solarRadiusDay d)


isOnDisk' :: Arcseconds -> BoundingBox -> Bool
isOnDisk' radius bb =
  all isCoordOnDisk $ boundingPoints bb
 where
  -- https://nssdc.gsfc.nasa.gov/planetary/factsheet/sunfact.html
  -- http://localhost:3000/program/id.107813.329344 - goes up to 970 in current data.
  -- Han said all datasets should be on disk so far
  isCoordOnDisk :: Coordinate Arcseconds -> Bool
  isCoordOnDisk c = boxRadius c < radius


boxRadius :: Coordinate Arcseconds -> Arcseconds
boxRadius (x, y) = sqrt (x ** 2 + y ** 2) :: Arcseconds


isQualified :: Group (Id InstrumentProgram) Dataset -> Bool
isQualified = either (const False) (const True) . qualify


qualify :: Group (Id InstrumentProgram) Dataset -> Either String ()
qualify g = do
  case (sample g).instrument of
    VISP -> qualifyVISP g
    VBI -> qualifyVBI g
    CRYO_NIRSP -> qualifyCryoNIRSP g


qualifyVISP :: Group (Id InstrumentProgram) Dataset -> Either String ()
qualifyVISP g = do
  -- let ds = NE.toList ip.datasets
  let sls = identifyLines (NE.toList g.items)
  check "On Disk" $ qualifyOnDisk g
  check "FeI 630" $ qualifyLine FeI630 sls
  check "CaII 854" $ qualifyLine CaII854 sls
  check "Stokes" $ qualifyStokes g
  check "Health" $ qualifyHealth g
  check "GOS" $ qualifyGOS g
  check "AO" $ qualifyAO g
 where
  check e b = if b then pure () else Left e


qualifyVBI :: Group (Id InstrumentProgram) Dataset -> Either String ()
qualifyVBI _ = Left "VBI Not supported"


qualifyCryoNIRSP :: Group (Id InstrumentProgram) Dataset -> Either String ()
qualifyCryoNIRSP _ = Left "CRYO_NIRSP Not supported"


qualifyStokes :: Group (Id InstrumentProgram) Dataset -> Bool
qualifyStokes g = all (\d -> d.stokesParameters == StokesParameters [I, Q, U, V]) g.items


qualifyOnDisk :: Group (Id InstrumentProgram) Dataset -> Bool
qualifyOnDisk g = all (\d -> bbOnDisk d.startTime d.boundingBox) g.items
 where
  bbOnDisk _ Nothing = False
  bbOnDisk t (Just bb) = isOnDisk (dayOfYear t) bb


qualifyLine :: SpectralLine -> [SpectralLine] -> Bool
qualifyLine sl sls = sl `elem` sls


-- Frazer: metric have to meet. Have to meet in each of the wavelength channels. As opposed to the set combined.
qualifyHealth :: Group (Id InstrumentProgram) Dataset -> Bool
qualifyHealth = all (hasPctGood 0.75)
 where
  hasPctGood :: Float -> Dataset -> Bool
  hasPctGood p d = (fromIntegral (fromMaybe 0 d.health.good) / fromIntegral d.frameCount) >= p


qualifyGOS :: Group (Id InstrumentProgram) Dataset -> Bool
qualifyGOS g = all allOpen g.items
 where
  allOpen d = fromMaybe 0 d.gosStatus.open == fromIntegral d.frameCount


qualifyAO :: Group (Id InstrumentProgram) Dataset -> Bool
qualifyAO = all (hasPctLocked 0.75)
 where
  hasPctLocked :: Float -> Dataset -> Bool
  hasPctLocked p d = (fromIntegral d.aoLocked / fromIntegral d.frameCount) >= p


-- SOLAR RADIUS -------------------------------
-- Constants

radiusOfSun :: Kilometers
radiusOfSun = 696340 -- Radius of the Sun in kilometers


meanDistanceSun :: Kilometers
meanDistanceSun = 149600000 -- Mean distance from Earth to Sun in kilometers


eccentricity :: Double
eccentricity = 0.0167 -- Eccentricity of Earth's orbit


dayOfPerihelion :: DayOfYear
dayOfPerihelion = 3 -- Day of the year when Earth is closest to the Sun (January 3rd)


dayOfAphelion :: DayOfYear
dayOfAphelion = 185


-- Calculate distance to the sun on a given day of the year
distanceToSun :: DayOfYear -> Kilometers
distanceToSun day = Kilometers $ meanDistanceSun.value * (1 - eccentricity * cos (2 * pi * (fromIntegral day - fromIntegral dayOfPerihelion) / 365))


-- Calculate angular diameter in radians
angularDiameter :: Kilometers -> Radians
angularDiameter dist = 2 * atan (Radians $ radiusOfSun.value / dist.value)


-- Calculate apparent radius in arcseconds
solarRadiusDay :: DayOfYear -> Arcseconds
solarRadiusDay day =
  let distance = distanceToSun day
      angDiameter = angularDiameter distance
   in radiansToArcseconds angDiameter / 2


dayOfYear :: UTCTime -> DayOfYear
dayOfYear time =
  let day = utctDay time
      (_, doy) = toOrdinalDate day
   in doy

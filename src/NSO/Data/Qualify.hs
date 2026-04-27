module NSO.Data.Qualify where

import Data.List.NonEmpty qualified as NE
import Data.Time
import Data.Time.Calendar.OrdinalDate (DayOfYear, toOrdinalDate)
import NSO.Data.Datasets
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Wavelength


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


isQualified :: NonEmpty Dataset -> Bool
isQualified = either (const False) (const True) . qualify


qualify :: NonEmpty Dataset -> Either String ()
qualify ds = do
  case (head ds).instrument of
    VISP -> qualifyVISP ds
    VBI -> qualifyVBI ds
    CRYO_NIRSP -> qualifyCryoNIRSP ds
    DL_NIRSP -> qualifyDLDIRSP ds


qualifyVISP :: NonEmpty Dataset -> Either String ()
qualifyVISP ds = do
  -- let ds = NE.toList ip.datasets
  let sls = concatMap (.spectralLines) $ NE.toList ds
  check "On Disk" $ qualifyOnDisk ds
  check "FeI 630" $ qualifyLine FeI sls
  -- check "CaII 854" $ qualifyLine CaII sls
  check "Stokes" $ qualifyStokes ds
  check "Health" $ qualifyHealth ds
  check "GOS" $ qualifyGOS ds
  check "AO" $ qualifyAO ds
 where
  check e b = if b then pure () else Left e


qualifyVBI :: NonEmpty Dataset -> Either String ()
qualifyVBI _ = Left "VBI Not supported"


qualifyCryoNIRSP :: NonEmpty Dataset -> Either String ()
qualifyCryoNIRSP _ = Left "CRYO-NIRSP Not supported"


qualifyDLDIRSP :: NonEmpty Dataset -> Either String ()
qualifyDLDIRSP _ = Left "DL-NIRSP Not supported"


qualifyStokes :: NonEmpty Dataset -> Bool
qualifyStokes = all (\d -> d.stokesParameters == StokesParameters [I, Q, U, V])


qualifyOnDisk :: NonEmpty Dataset -> Bool
qualifyOnDisk = all (\d -> bbOnDisk d.startTime d.boundingBox)
 where
  bbOnDisk _ Nothing = False
  bbOnDisk t (Just bb) = isOnDisk (dayOfYear t) bb


qualifyLine :: Ion -> [SpectralLine] -> Bool
qualifyLine is sls = is `elem` fmap (.ion) sls


-- Frazer: metric have to meet. Have to meet in each of the wavelength channels. As opposed to the set combined.
qualifyHealth :: NonEmpty Dataset -> Bool
qualifyHealth = all (hasPctGood 0.75)
 where
  hasPctGood :: Float -> Dataset -> Bool
  hasPctGood p d = (fromIntegral (fromMaybe 0 d.health.good) / fromIntegral d.frameCount) >= p


qualifyGOS :: NonEmpty Dataset -> Bool
qualifyGOS = all allOpen
 where
  allOpen d = fromMaybe 0 d.gosStatus.open == fromIntegral d.frameCount


qualifyAO :: NonEmpty Dataset -> Bool
qualifyAO = all (hasPctLocked 0.75)
 where
  hasPctLocked :: Float -> Dataset -> Bool
  hasPctLocked p d = (fromIntegral d.aoLocked / fromIntegral d.frameCount) >= p


isWavNarrow :: Ion -> Wavelength Nm -> Bool
isWavNarrow ion w =
  -- per Han, these are mid-points of an acceptable window to be able to use the spectral line image
  case ion of
    FeI -> narrow 630.3 0.5
    CaII -> narrow 854.2 1.0
    NaI -> narrow 589.3 1.0
    _ -> False
 where
  narrow mid dw =
    abs (mid - w) < dw


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

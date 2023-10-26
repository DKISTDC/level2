module NSO.Data.Qualify where

import Data.List.NonEmpty qualified as NE
import NSO.Data.Dataset
import NSO.Data.Types
import NSO.Prelude

data SpectralLine
  = HeI
  | Ha
  | FeI
  | CaII CaIILine
  deriving (Eq)

instance Show SpectralLine where
  show HeI = "HeI"
  show Ha = "Hα"
  show FeI = "FeI"
  show (CaII l) = "CaII " <> show l

data CaIILine
  = CaII_849
  | CaII_854
  | CaII_866
  deriving (Bounded, Enum, Eq)

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

identifyLines :: [Dataset] -> [SpectralLine]
identifyLines = mapMaybe (\d -> identifyLine d.wavelengthMin d.wavelengthMax)

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

qualify :: InstrumentProgram -> Either String ()
qualify ip = do
  case (head ip.datasets).instrument of
    VISP -> qualifyVISP ip
    VBI -> qualifyVBI ip

qualifyVISP :: InstrumentProgram -> Either String ()
qualifyVISP ip = do
  let ds = NE.toList ip.datasets
      sls = identifyLines ds
  check "On Disk" $ qualifyOnDisk ds
  check "FeI" $ qualifyLine FeI sls
  check "CaII 854" $ qualifyLine (CaII CaII_854) sls
  check "Stokes" $ qualifyStokes ds
 where
  check e b = if b then pure () else Left e

qualifyVBI :: InstrumentProgram -> Either String ()
qualifyVBI _ = Left "VBI Not supported"

qualifyStokes :: [Dataset] -> Bool
qualifyStokes = all (\d -> d.stokesParameters == StokesParameters [I, Q, U, V])

qualifyOnDisk :: [Dataset] -> Bool
qualifyOnDisk = all (\d -> isOnDisk d.boundingBox)

qualifyLine :: SpectralLine -> [SpectralLine] -> Bool
qualifyLine sl sls = sl `elem` sls

isQualified :: InstrumentProgram -> Bool
isQualified = either (const False) (const True) . qualify

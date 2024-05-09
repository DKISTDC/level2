module NSO.Fits.Generate.Headers.WCS where

import Data.Fits (KeywordRecord (..), toFloat, toInt, toText)
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Text (pack)
import Effectful
import Effectful.Error.Static
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Headers.Doc as Doc
import NSO.Fits.Generate.Headers.Keywords
import NSO.Fits.Generate.Headers.LiftL1
import NSO.Fits.Generate.Headers.Types
import NSO.Prelude
import Telescope.Fits as Fits


-- WCS --------------------------------------------------------------

data WCSCommon = WCSCommon
  { wcsvalid :: Key (Constant True) "WCI data are correct"
  , wcsname :: Key (Constant "Helioprojective Cartesian") "Helioprojective Cartesian"
  , wcsaxes :: Key Int "Number of axes in the Helioprojective Cartesian WCS description" -- MUST precede other WCS keywords
  , lonpole :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  , wcsnamea :: Key (Constant "Equatorial equinox J2000") "Equatorial equinox J2000"
  , wcsaxesa :: Key Int "Number of axes in the Equatorial equinox J2000 WCS description" -- MUST precede other WCS keywords
  , lonpolea :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


data WCSAxisKeywords (alt :: WCSAlt) (n :: Nat) = WCSAxisKeywords
  { ctype :: Key Text "A string value labeling the type of axis n"
  , cunit :: Key Text "The unit of the value contained in CDELTn"
  , crpix :: Key Float "The value field shall contain a floating point number, identifying the location of a reference point along axis n"
  , crval :: Key Float "The value field shall contain a floating point number, giving the value of the coordinate specified by the CTYPEn keyword at the reference point CRPIXn"
  , cdelt :: Key Float "Pixel scale of the world coordinate at the reference point along axis n"
  }
  deriving (Generic)
instance (KnownNat n, KnownValue alt) => HeaderKeywords (WCSAxisKeywords alt n) where
  headerKeywords =
    fmap modKey . genHeaderKeywords . from
   where
    modKey KeywordRecord{_keyword, _value, _comment} = KeywordRecord{_keyword = addA $ addN _keyword, _value, _comment}
    addN k = k <> pack (show (natVal @n Proxy))
    addA k = k <> knownValueText @alt


data WCSAxis (alt :: WCSAlt) (n :: Nat) = WCSAxis
  { keys :: WCSAxisKeywords alt n
  , pcs :: QuantityAxes (PC alt n)
  }
instance (KnownValue alt, KnownNat n) => HeaderKeywords (WCSAxis alt n) where
  headerKeywords ax =
    headerKeywords ax.keys
      <> headerKeywords ax.pcs


data PC (alt :: WCSAlt) (i :: Nat) (j :: Nat) = PC Float
instance (KnownValue alt, KnownNat i, KnownNat j) => KeywordInfo (PC alt i j) where
  keyword = "PC" <> showN @i Proxy <> "_" <> showN @j Proxy <> knownValueText @alt
   where
    showN :: forall n. (KnownNat n) => Proxy n -> Text
    showN p = pack (show $ natVal p)
  keytype = "PCi_j"
  description = "Linear transformation matrix used with the coordinate system"
  keyValue (PC n) = Float n


-- these types correspond to the order of the transformation
type DummyYN = 3
type SlitXN = 2
type DepthN = 1


wcsCommon :: forall es. (Error LiftL1Error :> es) => Header -> Eff es WCSCommon
wcsCommon l1 = do
  lonpole <- Degrees <$> requireL1 "LONPOLE" toFloat l1
  lonpolea <- Degrees <$> requireL1 "LONPOLEA" toFloat l1
  pure $
    WCSCommon
      { wcsvalid = Key Constant
      , wcsname = Key Constant
      , wcsaxes = Key 3
      , lonpole = Key lonpole
      , wcsnamea = Key Constant
      , wcsaxesa = Key 3
      , lonpolea = Key lonpolea
      }


data QuantityAxes f = QuantityAxes
  { dummyY :: f DummyYN
  , slitX :: f SlitXN
  , depth :: f DepthN
  }
  deriving (Generic)
instance (KnownValue alt, KnownNat n) => HeaderKeywords (QuantityAxes (PC alt n))
instance (KnownValue alt) => HeaderKeywords (QuantityAxes (WCSAxis alt)) where
  headerKeywords a =
    headerKeywords @(WCSAxis alt DepthN) a.depth
      <> headerKeywords @(WCSAxis alt SlitXN) a.slitX
      <> headerKeywords @(WCSAxis alt DummyYN) a.dummyY


requireWCS :: forall alt n es. (Error LiftL1Error :> es, KnownValue alt) => Int -> Header -> Eff es (WCSAxisKeywords alt n)
requireWCS n l1 = do
  crpix <- Key <$> requireL1 (keyN "CRPIX") toFloat l1
  crval <- Key <$> requireL1 (keyN "CRVAL") toFloat l1
  cdelt <- Key <$> requireL1 (keyN "CDELT") toFloat l1
  cunit <- Key <$> requireL1 (keyN "CUNIT") toText l1
  ctype <- Key <$> requireL1 (keyN "CTYPE") toText l1
  pure $ WCSAxisKeywords{cunit, ctype, crpix, crval, cdelt}
 where
  keyN k = k <> pack (show n) <> knownValueText @alt


requirePCs :: forall alt n es. (Error LiftL1Error :> es, KnownValue alt) => Int -> Header -> Eff es (QuantityAxes (PC alt n))
requirePCs n l1 = do
  dummyY <- PC <$> requireL1 (pcN n 3) toFloat l1
  slitX <- PC <$> requireL1 (pcN n 1) toFloat l1
  pure $ QuantityAxes{dummyY, slitX, depth = PC 0}
 where
  pcN :: Int -> Int -> Text
  pcN i j = "PC" <> pack (show i) <> "_" <> pack (show j) <> knownValueText @alt


wcsDummyY :: (Error LiftL1Error :> es, KnownValue alt) => Header -> Eff es (WCSAxis alt DummyYN)
wcsDummyY l1 = do
  keys <- requireWCS 3 l1
  pcs <- requirePCs 3 l1
  pure $ WCSAxis{keys, pcs}


wcsSlitX :: forall alt es. (Error LiftL1Error :> es, KnownValue alt) => Sz Ix2 -> Header -> Eff es (WCSAxis alt SlitXN)
wcsSlitX sz l1 = do
  scaleUp <- upFactor sz

  keys <- requireWCS @alt 1 l1
  pcs <- requirePCs @alt 1 l1
  pure $ WCSAxis{keys = scale scaleUp keys, pcs}
 where
  upFactor :: Sz Ix2 -> Eff es Float
  upFactor (Sz (newx :. _)) = do
    oldx <- requireL1 "ZNAXIS1" toInt l1
    pure $ fromIntegral oldx / fromIntegral newx

  scale up ax =
    -- The L1 files have a large number of Slit X pixels.
    -- Our data is binned along the X axis, so we need to scale the CRPIXn and CDELTn headers
    -- to compensate for this
    WCSAxisKeywords
      { cunit = ax.cunit
      , ctype = ax.ctype
      , crval = ax.crval
      , crpix = scaleCrPix ax.crpix
      , cdelt = scaleCDelt ax.cdelt
      }
   where
    scaleCrPix (Key cp) = Key $ (cp - 1) / up + 1
    scaleCDelt (Key cd) = Key $ cd * up

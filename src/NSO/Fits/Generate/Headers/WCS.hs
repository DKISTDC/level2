module NSO.Fits.Generate.Headers.WCS where

import Data.Fits (KeywordRecord (..), toFloat, toInt, toText)
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Text (pack, unpack)
import Debug.Trace
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
import Text.Read (readMaybe)


newtype X = X Int
newtype Y = Y Int


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


-- data WCSAxis (alt :: WCSAlt) axes (n :: Nat) = WCSAxis
--   { keys :: WCSAxisKeywords alt n
--   , pcs :: axes (PC alt n)
--   }
-- instance (KnownValue alt, KnownNat n, HeaderKeywords (axes (PC alt n))) => HeaderKeywords (WCSAxis alt axes n) where
--   headerKeywords ax =
--     headerKeywords ax.keys
--       <> headerKeywords ax.pcs

data PC (alt :: WCSAlt) (i :: Nat) (j :: Nat) = PC Float
instance (KnownValue alt, KnownNat i, KnownNat j) => KeywordInfo (PC alt i j) where
  keyword = "PC" <> showN @i Proxy <> "_" <> showN @j Proxy <> knownValueText @alt
   where
    showN :: forall n. (KnownNat n) => Proxy n -> Text
    showN p = pack (show $ natVal p)
  keytype = "PCi_j"
  description = "Linear transformation matrix used with the coordinate system"
  keyValue (PC n) = Float n


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


findYAxis :: (Error LiftL1Error :> es) => Header -> Eff es Y
findYAxis = fmap Y <$> findCtypeAxis "HPLN-TAN"


findXAxis :: (Error LiftL1Error :> es) => Header -> Eff es X
findXAxis = fmap X <$> findCtypeAxis "HPLT-TAN"


findCtypeAxis :: (Error LiftL1Error :> es) => Text -> Header -> Eff es Int
findCtypeAxis ctype l1 = do
  case findL1 toCtypeN l1 of
    Nothing -> throwError $ MissingCType (unpack ctype)
    Just k -> pure k
 where
  toCtypeN :: KeywordRecord -> Maybe Int
  toCtypeN k = do
    guard (k._value == String ctype)
    readMaybe $ drop 5 $ unpack k._keyword


-- super confusing... the N is whatever you current axis is?
requirePCs :: forall alt n x y es. (Error LiftL1Error :> es, KnownValue alt) => Int -> Header -> Eff es (PC alt n y, PC alt n x)
requirePCs n l1 = do
  xn <- findCtypeAxis "HPLT-TAN" l1
  yn <- findCtypeAxis "HPLN-TAN" l1
  dummyY <- PC <$> requireL1 (pcN n yn) toFloat l1
  slitX <- PC <$> requireL1 (pcN n xn) toFloat l1
  pure (dummyY, slitX)
 where
  pcN :: Int -> Int -> Text
  pcN i j = "PC" <> pack (show i) <> "_" <> pack (show j) <> knownValueText @alt


-- needs to construct PCs for you
-- unless it can return them separately?
wcsDummyYKeys :: (KnownValue alt, Error LiftL1Error :> es) => Header -> Eff es (WCSAxisKeywords alt n)
wcsDummyYKeys l1 = do
  let l1DummyY = 3
  requireWCS l1DummyY l1


wcsDummyYPCs :: (KnownValue alt, Error LiftL1Error :> es) => Header -> Eff es (PC alt y y, PC alt y x)
wcsDummyYPCs l1 = do
  let l1DummyY = 3
  requirePCs l1DummyY l1


wcsSlitXPCs :: (KnownValue alt, Error LiftL1Error :> es) => Header -> Eff es (PC alt x y, PC alt x x)
wcsSlitXPCs l1 = do
  let l1SlitX = 1
  requirePCs l1SlitX l1


wcsSlitXKeys :: forall alt n es. (Error LiftL1Error :> es, KnownValue alt) => Int -> Header -> Eff es (WCSAxisKeywords alt n)
wcsSlitXKeys newx l1 = do
  let l1SlitX = 1
  scaleUp <- upFactor

  keys <- requireWCS @alt l1SlitX l1

  -- (y, x) <- requirePCs l1SlitX l1
  -- pure $ WCSAxis{keys = scale scaleUp keys, pcs = toPCs y x}
  pure $ scale scaleUp keys
 where
  upFactor :: Eff es Float
  upFactor = do
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

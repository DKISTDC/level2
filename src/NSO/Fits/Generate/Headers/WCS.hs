{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.Headers.WCS where

import Data.Fits (KeywordRecord (..), toFloat, toInt, toText)
import Data.Text (pack, unpack)
import Effectful
import Effectful.Error.Static
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Headers.Doc as Doc
import NSO.Fits.Generate.Headers.Keywords
import NSO.Fits.Generate.Headers.LiftL1
import NSO.Fits.Generate.Headers.Types
import NSO.Prelude
import Telescope.Fits as Fits hiding (Axis)
import Text.Read (readMaybe)


-- ints?

newtype Axis a = Axis Int
  deriving (Show, Eq)


data X
data Y
data Wav


type family AxisNumber a ax :: Natural


class AxisOrder a ax where
  axisN :: Natural


-- axisIndex :: Proxy a -> Proxy ax -> Natural

data PCL1 s (alt :: WCSAlt) = PCL1
  { xx :: PC s alt X X
  , xy :: PC s alt X Y
  , yy :: PC s alt Y Y
  , yx :: PC s alt Y X
  }


-- WCS --------------------------------------------------------------

data Test = Test
  { test :: Key Int "wahoo"
  }
  deriving (Generic, HeaderKeywords)


data WCSCommon = WCSCommon
  { wcsvalid :: Key (Constant True) "WCI data are correct"
  , wcsname :: Key (Constant "Helioprojective Cartesian") "Helioprojective Cartesian"
  , wcsaxes :: Key Int "Number of axes in the Helioprojective Cartesian WCS description" -- MUST precede other WCS keywords
  , lonpole :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


data WCSCommonA = WCSCommonA
  { wcsnamea :: Key (Constant "Equatorial equinox J2000") "Equatorial equinox J2000"
  , wcsaxesa :: Key Int "Number of axes in the Equatorial equinox J2000 WCS description" -- MUST precede other WCS keywords
  , lonpolea :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


data WCSAxisKeywords s (alt :: WCSAlt) ax = WCSAxisKeywords
  { ctype :: Key Text "A string value labeling the type of axis n"
  , cunit :: Key Text "The unit of the value contained in CDELTn"
  , crpix :: Key Float "The value field shall contain a floating point number, identifying the location of a reference point along axis n"
  , crval :: Key Float "The value field shall contain a floating point number, giving the value of the coordinate specified by the CTYPEn keyword at the reference point CRPIXn"
  , cdelt :: Key Float "Pixel scale of the world coordinate at the reference point along axis n"
  }
  deriving (Generic)
instance (AxisOrder s ax, KnownValue alt) => HeaderKeywords (WCSAxisKeywords s alt ax) where
  headerKeywords =
    fmap modKey . genHeaderKeywords . from
   where
    modKey KeywordRecord{_keyword, _value, _comment} = KeywordRecord{_keyword = addA $ addN _keyword, _value, _comment}
    addN k = k <> pack (show (axisN @s @ax))
    addA k = k <> knownValueText @alt


-- data WCSAxis (alt :: WCSAlt) axes (n :: Nat) = WCSAxis
--   { keys :: WCSAxisKeywords alt n
--   , pcs :: axes (PC alt n)
--   }
-- instance (KnownValue alt, KnownNat n, HeaderKeywords (axes (PC alt n))) => HeaderKeywords (WCSAxis alt axes n) where
--   headerKeywords ax =
--     headerKeywords ax.keys
--       <> headerKeywords ax.pcs

-- it's a mapping of one axis type to another
data PC s (alt :: WCSAlt) ai aj = PC Float
  deriving (Show, Eq)
instance (KnownValue alt, AxisOrder s ai, AxisOrder s aj) => KeywordInfo (PC s alt ai aj) where
  keyword = "PC" <> pack (show (axisN @s @ai)) <> "_" <> pack (show (axisN @s @aj)) <> knownValueText @alt
  keytype = "PCi_j"
  description = "Linear transformation matrix used with the coordinate system"
  keyValue (PC n) = Float n
instance (KnownValue alt, AxisOrder s ai, AxisOrder s aj) => HeaderKeywords (PC s alt ai aj) where
  headerKeywords p = [keywordRecord p]


wcsCommon :: forall es. (Error LiftL1Error :> es) => Header -> Eff es WCSCommon
wcsCommon l1 = do
  lonpole <- Degrees <$> requireL1 "LONPOLE" toFloat l1
  pure $
    WCSCommon
      { wcsvalid = Key Constant
      , wcsname = Key Constant
      , wcsaxes = Key 3
      , lonpole = Key lonpole
      }


wcsCommonA :: forall es. (Error LiftL1Error :> es) => Header -> Eff es WCSCommonA
wcsCommonA l1 = do
  lonpolea <- Degrees <$> requireL1 "LONPOLEA" toFloat l1
  pure $
    WCSCommonA
      { wcsnamea = Key Constant
      , wcsaxesa = Key 3
      , lonpolea = Key lonpolea
      }


requireWCS :: forall s alt ax es. (Error LiftL1Error :> es, KnownValue alt) => Axis ax -> Header -> Eff es (WCSAxisKeywords s alt ax)
requireWCS (Axis n) l1 = do
  crpix <- Key <$> requireL1 (keyN "CRPIX") toFloat l1
  crval <- Key <$> requireL1 (keyN "CRVAL") toFloat l1
  cdelt <- Key <$> requireL1 (keyN "CDELT") toFloat l1
  cunit <- Key <$> requireL1 (keyN "CUNIT") toText l1
  ctype <- Key <$> requireL1 (keyN "CTYPE") toText l1
  pure $ WCSAxisKeywords{cunit, ctype, crpix, crval, cdelt}
 where
  keyN k = k <> pack (show n) <> knownValueText @alt


requireWCSAxes :: (Error LiftL1Error :> es) => Header -> Eff es (Axis X, Axis Y)
requireWCSAxes h = do
  y <- axisY h
  x <- axisX h
  pure (x, y)
 where
  axisY = fmap Axis <$> requireCtypeAxis "HPLN-TAN"
  axisX = fmap Axis <$> requireCtypeAxis "HPLT-TAN"


requireCtypeAxis :: (Error LiftL1Error :> es) => Text -> Header -> Eff es Int
requireCtypeAxis ctype l1 = do
  case findL1 toCtypeN l1 of
    Nothing -> throwError $ MissingCType (unpack ctype)
    Just k -> pure k
 where
  toCtypeN :: KeywordRecord -> Maybe Int
  toCtypeN k = do
    guard (k._value == String ctype)
    readMaybe $ drop 5 $ unpack k._keyword


requirePCs :: forall alt s es. (Error LiftL1Error :> es, KnownValue alt) => Axis X -> Axis Y -> Header -> Eff es (PCL1 s alt)
requirePCs (Axis xn) (Axis yn) l1 = do
  yy <- PC <$> requireL1 (pcN yn yn) toFloat l1
  yx <- PC <$> requireL1 (pcN yn xn) toFloat l1
  xx <- PC <$> requireL1 (pcN xn xn) toFloat l1
  xy <- PC <$> requireL1 (pcN xn yn) toFloat l1
  pure PCL1{yy, yx, xx, xy}
 where
  pcN :: Int -> Int -> Text
  pcN i j = "PC" <> pack (show i) <> "_" <> pack (show j) <> knownValueText @alt


wcsDummyY :: (KnownValue alt, Error LiftL1Error :> es) => Axis Y -> Header -> Eff es (WCSAxisKeywords s alt Y)
wcsDummyY y l1 = do
  requireWCS y l1


newtype BinnedX = BinnedX Int


wcsSlitX :: forall alt s es. (Error LiftL1Error :> es, KnownValue alt) => Axis X -> BinnedX -> Header -> Eff es (WCSAxisKeywords s alt X)
wcsSlitX ax (BinnedX newx) l1 = do
  scaleUp <- upFactor

  keys <- requireWCS @s @alt ax l1

  let Header (wtf : _) = l1

  pure $ scale scaleUp keys
 where
  upFactor :: Eff es Float
  upFactor = do
    oldx <- requireL1 "ZNAXIS1" toInt l1
    pure $ fromIntegral oldx / fromIntegral newx

  scale up a =
    -- The L1 files have a large number of Slit X pixels.
    -- Our data is binned along the X axis, so we need to scale the CRPIXn and CDELTn headers
    -- to compensate for this
    WCSAxisKeywords
      { cunit = a.cunit
      , ctype = a.ctype
      , crval = a.crval
      , crpix = scaleCrPix a.crpix
      , cdelt = scaleCDelt a.cdelt
      }
   where
    scaleCrPix (Key cp) = Key $ (cp - 1) / up + 1
    scaleCDelt (Key cd) = Key $ cd * up

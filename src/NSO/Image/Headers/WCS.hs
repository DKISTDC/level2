{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Image.Headers.WCS where

import Data.Text (pack, unpack)
import Effectful
import Effectful.Error.Static
import GHC.Generics
import GHC.TypeLits
import NSO.Image.Headers.Doc as Doc
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Parse
import NSO.Image.Headers.Types
import NSO.Prelude
import Telescope.Fits as Fits hiding (Axis)
import Telescope.Fits.Header as Fits
import Text.Read (readMaybe)


data WCSHeader axes = WCSHeader
  { common :: WCSCommon
  , axes :: axes 'WCSMain
  , commonA :: WCSCommonA
  , axesA :: axes 'A
  }
  deriving (Generic)


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

-- data Test = Test
--   { test :: Key Int "wahoo"
--   }
--   deriving (Generic, HeaderKeywords)

data WCSCommon = WCSCommon
  { wcsvalid :: Key Bool "WCI data are correct"
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
data PC s (alt :: WCSAlt) ai aj = PC {value :: Float}
  deriving (Show, Eq)
instance (KnownValue alt, AxisOrder s ai, AxisOrder s aj) => KeywordInfo (PC s alt ai aj) where
  keyword = "PC" <> pack (show (axisN @s @ai)) <> "_" <> pack (show (axisN @s @aj)) <> knownValueText @alt
  keytype = "PCi_j"
  description = "Linear transformation matrix used with the coordinate system"
  keyValue (PC n) = Float n
instance (KnownValue alt, AxisOrder s ai, AxisOrder s aj) => HeaderKeywords (PC s alt ai aj) where
  headerKeywords p = [keywordRecord p]


wcsCommon :: forall es. (Error ParseError :> es) => Bool -> Header -> Eff es WCSCommon
wcsCommon valid l1 = do
  lonpole <- Degrees <$> requireKey "LONPOLE" toFloat l1
  pure $
    WCSCommon
      { wcsvalid = Key valid
      , wcsname = Key Constant
      , wcsaxes = Key 3
      , lonpole = Key lonpole
      }


wcsCommonA :: forall es. (Error ParseError :> es) => Header -> Eff es WCSCommonA
wcsCommonA l1 = do
  lonpolea <- Degrees <$> requireKey "LONPOLEA" toFloat l1
  pure $
    WCSCommonA
      { wcsnamea = Key Constant
      , wcsaxesa = Key 3
      , lonpolea = Key lonpolea
      }


requireWCS :: forall s alt ax es. (Error ParseError :> es, KnownValue alt) => Axis ax -> Header -> Eff es (WCSAxisKeywords s alt ax)
requireWCS (Axis n) l1 = do
  crpix <- Key <$> requireKey (keyN "CRPIX") toFloat l1
  crval <- Key <$> requireKey (keyN "CRVAL") toFloat l1
  cdelt <- Key <$> requireKey (keyN "CDELT") toFloat l1
  cunit <- Key <$> requireKey (keyN "CUNIT") toText l1
  ctype <- Key <$> requireKey (keyN "CTYPE") toText l1
  pure $ WCSAxisKeywords{cunit, ctype, crpix, crval, cdelt}
 where
  keyN k = k <> pack (show n) <> knownValueText @alt


-- | Look up the order of the spatial axes and report which is which
requireWCSAxes :: (Error ParseError :> es) => Header -> Eff es (Axis X, Axis Y)
requireWCSAxes h = do
  y <- axisY h
  x <- axisX h
  pure (x, y)
 where
  axisY = fmap Axis <$> requireCtypeAxis "HPLN-TAN"
  axisX = fmap Axis <$> requireCtypeAxis "HPLT-TAN"


requireCtypeAxis :: (Error ParseError :> es) => Text -> Header -> Eff es Int
requireCtypeAxis ctype l1 = do
  case findKey toCtypeN l1 of
    Nothing -> throwError $ MissingKey ("CTYPE: " ++ show ctype)
    Just k -> pure k
 where
  toCtypeN :: KeywordRecord -> Maybe Int
  toCtypeN k = do
    guard (k._value == String ctype)
    readMaybe $ drop 5 $ unpack k._keyword


-- can we detect that they are incorrect here?
requirePCs :: forall alt s es. (Error ParseError :> es, KnownValue alt) => Axis X -> Axis Y -> Header -> Eff es (PCL1 s alt)
requirePCs (Axis xn) (Axis yn) l1 = do
  yy <- PC <$> requireKey (pcN yn yn) toFloat l1
  yx <- PC <$> requireKey (pcN yn xn) toFloat l1
  xx <- PC <$> requireKey (pcN xn xn) toFloat l1
  xy <- PC <$> requireKey (pcN xn yn) toFloat l1
  pure PCL1{yy, yx, xx, xy}
 where
  pcN :: Int -> Int -> Text
  pcN i j = "PC" <> pack (show i) <> "_" <> pack (show j) <> knownValueText @alt


isPCsValid :: PCL1 s alt -> Bool
isPCsValid pcs =
  0 `notElem` [pcs.xx.value, pcs.xy.value, pcs.yy.value, pcs.yx.value]


wcsDummyY :: (KnownValue alt, Error ParseError :> es) => Axis Y -> SliceXY -> Header -> Eff es (WCSAxisKeywords s alt Y)
wcsDummyY y s l1 = do
  keys <- requireWCS y l1
  pure $ adjustDummyY s keys


wcsSlitX :: forall alt s es. (Error ParseError :> es, KnownValue alt) => Axis X -> SliceXY -> Header -> Eff es (WCSAxisKeywords s alt X)
wcsSlitX ax bx l1 = do
  keys <- requireWCS @s @alt ax l1
  pure $ adjustSlitX bx keys


adjustSlitX :: SliceXY -> WCSAxisKeywords s alt X -> WCSAxisKeywords s alt X
adjustSlitX s l1 =
  -- The L1 files have a large number of Slit X pixels.
  -- Our data is binned along the X axis, so we need to scale/translate the CRPIXn and CDELTn headers to compensate
  WCSAxisKeywords
    { cunit = l1.cunit
    , ctype = l1.ctype
    , crval = l1.crval
    , crpix = scaleTranslateCrPix l1.crpix
    , cdelt = scaleCDelt l1.cdelt
    }
 where
  scaleTranslateCrPix (Key cp) =
    -- first, translate to remove the beginning pixels, so CRPIX of 800 becomes CRPIX of 700
    let transX = cp - fromIntegral s.pixelBeg
     in -- then, scale by the opposite factor
        Key $ transX / fromIntegral s.pixelsPerBin
  -- delta should be higher by the binning factor. Fewer bins = larger delta
  scaleCDelt (Key cd) = Key $ cd * fromIntegral s.pixelsPerBin


adjustDummyY :: SliceXY -> WCSAxisKeywords s alt Y -> WCSAxisKeywords s alt Y
adjustDummyY s l1 =
  WCSAxisKeywords
    { cunit = l1.cunit
    , ctype = l1.ctype
    , crval = l1.crval
    , crpix = translateCrPix l1.crpix
    , cdelt = l1.cdelt
    }
 where
  translateCrPix (Key cp) = Key $ cp - fromIntegral s.frameBeg
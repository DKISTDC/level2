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
import Telescope.Asdf.GWCS (ToAxes (..))
import Telescope.Data.Axes (AxisOrder (..))
import Telescope.Data.KnownText
import Telescope.Data.Parser (parseFail)
import Telescope.Data.WCS (CType (..), CUnit (..), WCSAlt (..), WCSAxis (..))
import Telescope.Fits as Fits hiding (Axis)
import Telescope.Fits.Header as Fits
import Telescope.Fits.Header.Class (GToHeader (..))
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


data X deriving (Generic, ToAxes)
data Y deriving (Generic, ToAxes)
data Wav


-- axisIndex :: Proxy a -> Proxy ax -> Natural

data PCXY s (alt :: WCSAlt) = PCXY
  { xx :: PC s alt X X
  , xy :: PC s alt X Y
  , yy :: PC s alt Y Y
  , yx :: PC s alt Y X
  }


data HDUAxis s axis


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
  deriving (Generic, HeaderDoc, ToHeader)


data WCSCommonA = WCSCommonA
  { wcsnamea :: Key (Constant "Equatorial equinox J2000") "Equatorial equinox J2000"
  , wcsaxesa :: Key Int "Number of axes in the Equatorial equinox J2000 WCS description" -- MUST precede other WCS keywords
  , lonpolea :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic, HeaderDoc, ToHeader)


data WCSAxisKeywords s (alt :: WCSAlt) ax = WCSAxisKeywords
  { ctype :: Key Text "A string value labeling the type of axis n"
  , cunit :: Key Text "The unit of the value contained in CDELTn"
  , crpix :: Key Float "The value field shall contain a floating point number, identifying the location of a reference point along axis n"
  , crval :: Key Float "The value field shall contain a floating point number, giving the value of the coordinate specified by the CTYPEn keyword at the reference point CRPIXn"
  , cdelt :: Key Float "Pixel scale of the world coordinate at the reference point along axis n"
  }
  deriving (Generic, Show)


toWCSAxis :: forall axis s (alt :: WCSAlt) ax. WCSAxisKeywords s alt ax -> WCSAxis alt axis
toWCSAxis keys =
  WCSAxis
    { ctype = CType keys.ctype.ktype
    , cunit = CUnit keys.cunit.ktype
    , crpix = keys.crpix.ktype
    , crval = keys.crval.ktype
    , cdelt = keys.cdelt.ktype
    }


fromWCSAxis :: WCSAxis alt ax -> WCSAxisKeywords s alt ax
fromWCSAxis wcs =
  let CType ctype = wcs.ctype
      CUnit cunit = wcs.cunit
   in WCSAxisKeywords{ctype = Key ctype, cunit = Key cunit, crpix = Key wcs.crpix, crval = Key wcs.crval, cdelt = Key wcs.cdelt}


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
instance KeywordInfo (PC s alt ai aj) where
  keytype = "PCi_j"
  description = "Linear transformation matrix used with the coordinate system"
instance (KnownText alt, AxisOrder (HDUAxis s ai), AxisOrder (HDUAxis s aj)) => IsKeyword (PC s alt ai aj) where
  keyword = "PC" <> pack (show (axisN @(HDUAxis s ai))) <> "_" <> pack (show (axisN @(HDUAxis s aj))) <> knownText @alt
instance ToKeyword (PC s alt ai aj) where
  toKeywordValue (PC n) = Float n


-- instance (KnownText alt, AxisOrder s ai, AxisOrder s aj) => ToHeader (PC s alt ai aj) where
--   toHeader pc = Header [Keyword $ KeywordRecord key (toKeywordValue pc) Nothing]

wcsCommon :: forall es. (Error ParseError :> es) => Bool -> Header -> Eff es WCSCommon
wcsCommon valid l1 = do
  lonpole <- Degrees <$> requireKey "LONPOLE" l1
  pure $
    WCSCommon
      { wcsvalid = Key valid
      , wcsname = Key Constant
      , wcsaxes = Key 3
      , lonpole = Key lonpole
      }


wcsCommonA :: forall es. (Error ParseError :> es) => Header -> Eff es WCSCommonA
wcsCommonA l1 = do
  lonpolea <- Degrees <$> requireKey "LONPOLEA" l1
  pure $
    WCSCommonA
      { wcsnamea = Key Constant
      , wcsaxesa = Key 3
      , lonpolea = Key lonpolea
      }


requireWCS :: forall s alt ax es. (Error ParseError :> es, KnownText alt) => Axis ax -> Header -> Eff es (WCSAxisKeywords s alt ax)
requireWCS (Axis n) l1 = do
  crpix <- Key <$> requireKey (keyN "CRPIX") l1
  crval <- Key <$> requireKey (keyN "CRVAL") l1
  cdelt <- Key <$> requireKey (keyN "CDELT") l1
  cunit <- Key <$> requireKey (keyN "CUNIT") l1
  ctype <- Key <$> requireKey (keyN "CTYPE") l1
  pure $ WCSAxisKeywords{cunit, ctype, crpix, crval, cdelt}
 where
  keyN k = k <> pack (show n) <> knownText @alt


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
requireCtypeAxis ctype (Header h) = runParser $ do
  case listToMaybe $ mapMaybe toCtypeN h of
    Nothing -> parseFail $ "Missing Key: CTYPE " ++ show ctype
    Just k -> pure k
 where
  toCtypeN :: HeaderRecord -> Maybe Int
  toCtypeN (Keyword k) = do
    guard (k._value == String ctype)
    readMaybe $ drop 5 $ unpack k._keyword
  toCtypeN _ = Nothing


-- can we detect that they are incorrect here?
requirePCs :: forall alt s es. (Error ParseError :> es, KnownText alt) => Axis X -> Axis Y -> Header -> Eff es (PCXY s alt)
requirePCs (Axis xn) (Axis yn) l1 = do
  yy <- PC <$> requireKey (pcN yn yn) l1
  yx <- PC <$> requireKey (pcN yn xn) l1
  xx <- PC <$> requireKey (pcN xn xn) l1
  xy <- PC <$> requireKey (pcN xn yn) l1
  pure PCXY{yy, yx, xx, xy}
 where
  pcN :: Int -> Int -> Text
  pcN i j = "PC" <> pack (show i) <> "_" <> pack (show j) <> knownText @alt


isPCsValid :: PCXY s alt -> Bool
isPCsValid pcs =
  -- the pcs never line up exactly. Should be a matrix with values for each thing
  0 `notElem` [pcs.xx.value, pcs.xy.value, pcs.yy.value, pcs.yx.value]


wcsDummyY :: (KnownText alt, Error ParseError :> es) => Axis Y -> SliceXY -> Header -> Eff es (WCSAxisKeywords s alt Y)
wcsDummyY y s l1 = do
  keys <- requireWCS y l1
  pure $ adjustDummyY s keys


wcsSlitX :: forall alt s es. (Error ParseError :> es, KnownText alt) => Axis X -> SliceXY -> Header -> Eff es (WCSAxisKeywords s alt X)
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

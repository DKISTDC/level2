{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Fits.Generate.Headers where

import Data.Kind
import Data.Text (pack)
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc as Doc
import NSO.Fits.Generate.Key
import NSO.Prelude
import Telescope.Fits (BitPix (..))
import Telescope.Fits.Encoding (bitPixCode)
import Telescope.Fits.Types (HeaderRecord (..), KeywordRecord (..), Value (..))


-- TODO: support comments
-- TODO: refactor

data PrimaryHeader = PrimaryHeader
  { telapse :: Key Seconds "TELAPSE = DATE-END - DATE-BEG. Not always equal to the exposure time as multiple exposures could be combined"
  , wcsvalid :: Key (Constant True) "WCI data are correct"
  , dsetid :: Key String "Unique ID of the dataset to which the frame belongs"
  , framevol :: Key MB "Size of the frame on disk."
  , proctype :: Key (Constant "L2") "Controlled value list representing the degree of processing the frame has undergone since receipt at the DKIST data center."
  , origin :: Key (Constant "National Solar Observatory") "The organization or institution responsible for creating the FITS file."
  , lonpole :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


data DataHDUAxes = DataHDUAxes
  { naxis :: Key (Constant "3") "Data HDUs have the shape (y, x, depth)"
  , naxis1 :: Key Int "depth"
  , naxis2 :: Key Int "slit x"
  , naxis3 :: Key (Constant "1") "dummy WCS y coordinate. Always only 1 wide"
  }
  deriving (Generic, HeaderDoc)


data DataHDUHeader info = DataHDUHeader


data DataHDUInfo (extName :: Symbol) (ucd :: UCD) (unit :: Unit)


-- type-level info for optical depth
type OpticalDepth = DataHDUInfo "Log of Optical Depth at 500nm" 'OpticalDepth 'Dimensionless
type Temperature = DataHDUInfo "Temperature" 'Temperature 'Kelvin
type ElectronPressure = DataHDUInfo "ElectronPressure" 'ElectronPressure 'Kelvin


-- the axes are all the same
instance (KnownSymbol ext, KnownValue btype, KnownValue bunit) => HeaderDoc (DataHDUInfo ext btype bunit) where
  headerDoc =
    [ docKey @(ExtName ext)
    , docKey @(BType btype)
    , docKey @(BUnit bunit)
    ]


instance (HeaderDoc info) => HeaderDoc (DataHDUHeader info) where
  headerDoc =
    headerDoc @info
      <> (headerDoc @DataHDUAxes)
      <> (headerDoc @PrimaryHeader)


primaryHeader :: PrimaryHeader
primaryHeader =
  PrimaryHeader
    { telapse = Key (Seconds 123)
    , wcsvalid = Key Constant
    , dsetid = Key "randomid"
    , framevol = Key $ MB 123
    , proctype = Key Constant
    , origin = Key Constant
    , lonpole = Key (Degrees 40)
    }


class HeaderKeywords a where
  headerKeywords :: a -> [KeywordRecord]
  default headerKeywords :: (Generic a, GenHeaderKeywords (Rep a)) => a -> [KeywordRecord]
  headerKeywords = genHeaderKeywords . from


class GenHeaderKeywords f where
  genHeaderKeywords :: f p -> [KeywordRecord]


-- datatype metadata
instance (GenHeaderKeywords f) => GenHeaderKeywords (M1 D c f) where
  genHeaderKeywords (M1 a) = genHeaderKeywords a


-- constructor metadata
instance (GenHeaderKeywords f) => GenHeaderKeywords (M1 C c f) where
  genHeaderKeywords (M1 a) = genHeaderKeywords a


-- Selectors
instance (GenHeaderKeywords f, Selector s) => GenHeaderKeywords (M1 S s f) where
  genHeaderKeywords (M1 a) =
    let s = selName (undefined :: M1 S s f x)
     in fmap (setKeyword s) $ genHeaderKeywords a
   where
    setKeyword s d = d{_keyword = pack s}


instance (GenHeaderKeywords a, GenHeaderKeywords b) => GenHeaderKeywords (a :*: b) where
  genHeaderKeywords (a :*: b) = genHeaderKeywords a ++ genHeaderKeywords b


instance (KeywordInfo a, KeyValue a) => GenHeaderKeywords (K1 i a) where
  genHeaderKeywords (K1 a) = [genKey a]


genKey :: forall a. (KeyValue a, KeywordInfo a) => a -> KeywordRecord
genKey a = KeywordRecord (pack $ keyword @a) (keyValue a) (pack <$> comment @a)


class KeyValue a where
  keyValue :: a -> Value


-- instance KeyValue Unit where
--   keyValue u = String (pack $ show u)
--
--
-- instance KeyValue UCD where
--   keyValue u = String (pack $ fromUCD u)

instance (KnownValue ucd) => KeyValue (BType ucd) where
  keyValue _ = String (pack $ knownValue @ucd)


instance (KnownValue unit) => KeyValue (BUnit unit) where
  keyValue _ = String (pack $ knownValue @unit)


instance (KeyValue ktype) => KeyValue (Key ktype desc) where
  keyValue (Key k) = keyValue k


instance (KnownValue c) => KeyValue (Constant c) where
  keyValue _ = String (pack $ knownValue @c)


instance KeyValue String where
  keyValue s = String (pack s)


instance KeyValue Seconds where
  keyValue (Seconds s) = Float s


instance KeyValue MB where
  keyValue (MB s) = Float s


instance KeyValue Degrees where
  keyValue (Degrees s) = Float s

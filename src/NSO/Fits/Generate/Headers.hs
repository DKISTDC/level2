{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.Headers where

import Data.Massiv.Array as M
import Data.Text (pack)
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc as Doc
import NSO.Fits.Generate.Keywords
import NSO.Fits.Generate.Types
import NSO.Prelude
import Telescope.Fits.Types (Value (..))


-- DONE: automatic type-based comments
-- DONE: custom comments with custom newtype (cleaner)
-- DONE: comments for bitpix, naxes and other auto-gen keywords.
--
-- TODO: generate a data hdu with eaders
-- TODO: refactor

-- COMMENTS -----------------------------
-- 1. Some units (ktype) need a comment. some don't
-- 2. Some field names need units. some don't
--
-- how hard would it be to make custom ones for each?

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


-- DATA HDUS

-- I would like to label each of these axes
data DataHDUAxes = DataHDUAxes
  { naxis :: Key (Constant "3") "Data HDUs have the shape (y, x, depth)"
  , naxis1 :: Naxis "Optical Depth"
  , naxis2 :: Naxis "Slit X"
  , naxis3 :: NaxisY
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


dataHduAxes :: Sz Ix2 -> DataHDUAxes
dataHduAxes (Sz (naxis2 :. naxis1)) =
  DataHDUAxes
    { naxis = Key Constant
    , naxis1 = Naxis naxis1
    , naxis2 = Naxis naxis2
    , naxis3 = NaxisY
    }


data Naxis cmt = Naxis Int
  deriving (Generic)


instance KeyValue (Naxis cmt) where
  keyValue (Naxis n) = Integer n


instance (KnownSymbol cmt) => KeywordInfo (Naxis cmt) where
  keytype = "Int"
  description = pack $ symbolVal @cmt Proxy
  comment = pack $ symbolVal @cmt Proxy


instance KeyValue NaxisY where
  keyValue _ = Integer 1


instance KeywordInfo NaxisY where
  keytype = "Int"
  description = "Dummy WCS Y Coordinate"
  comment = "Dummy WCS Y Coordinate"
  constant = Just (Integer 1)


data NaxisY = NaxisY
  deriving (Generic)


data DataHDUHeader info = DataHDUHeader info (Sz Ix2)


data DataHDUInfo (extName :: Symbol) (ucd :: UCD) (unit :: Unit) = DataHDUInfo


-- type-level info for optical depth
type OpticalDepth = DataHDUInfo "Log of Optical Depth at 500nm" 'OpticalDepth 'Dimensionless
type Temperature = DataHDUInfo "Temperature" 'Temperature 'Kelvin
type ElectronPressure = DataHDUInfo "ElectronPressure" 'ElectronPressure 'Kelvin


-- HEADERS -----------------------------------------------------
instance (KnownSymbol ext, KnownValue btype, KnownValue bunit) => HeaderKeywords (DataHDUInfo ext btype bunit) where
  headerKeywords _ =
    [ keywordRecord @(ExtName ext) ExtName
    , keywordRecord @(BType btype) BType
    , keywordRecord @(BUnit bunit) BUnit
    ]


instance (HeaderKeywords info) => HeaderKeywords (DataHDUHeader info) where
  headerKeywords (DataHDUHeader info sz) =
    headerKeywords @info info <> headerKeywords @DataHDUAxes (dataHduAxes sz)


-- DOCUMENTATION ------------------------------------------------
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

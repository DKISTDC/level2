{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.Headers where

import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc as Doc
import NSO.Fits.Generate.Keywords
import NSO.Fits.Generate.Types
import NSO.Prelude


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
data DataHDUAxesDoc = DataHDUAxesDoc
  { naxis :: Key (Constant "3") "Data HDUs have the shape (y, x, depth)"
  , naxis1 :: Key Int "depth"
  , naxis2 :: Key Int "slit x"
  , naxis3 :: Key (Constant "1") "dummy WCS y coordinate. Always only 1 wide"
  }
  deriving (Generic, HeaderDoc)


data DataHDUHeader info = DataHDUHeader


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
      <> (headerDoc @DataHDUAxesDoc)
      <> (headerDoc @PrimaryHeader)

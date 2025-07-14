module NSO.Image.Headers.DataCommon where

import GHC.Generics
import GHC.TypeLits
import NSO.Image.Headers.Doc as Doc
import NSO.Image.Headers.Keywords
import NSO.Image.Headers.Types
import NSO.Prelude
import Telescope.Fits as Fits
import Telescope.Fits.Header (Header (..), HeaderRecord (..))


data DataHDUAxes = DataHDUAxes
  { naxis :: Key (Constant "3") "Data HDUs have the shape (y, x, depth)"
  , naxis1 :: Naxis "Optical Depth"
  , naxis2 :: Naxis "Slit X"
  , naxis3 :: NaxisY
  }
  deriving (Generic, HeaderDoc)


-- The DataHDUInfo contains static headers EXTNAME, BTYPE and BUNIT
data DataHDUInfo (extName :: Symbol) (btype :: Symbol) (bunit :: Unit) = DataHDUInfo


instance (KnownSymbol ext, KnownSymbol btype, KnownValue bunit) => ToHeader (DataHDUInfo ext btype bunit) where
  toHeader _ =
    Header $
      fmap
        Keyword
        [ keywordRecord @(ExtName ext) ExtName
        , keywordRecord @(BType btype) BType
        , keywordRecord @(BUnit bunit) BUnit
        ]


instance (KnownSymbol ext, KnownSymbol btype, KnownValue bunit) => HeaderDoc (DataHDUInfo ext btype bunit) where
  headerDoc =
    [ docKey @(ExtName ext)
    , docKey @(BType btype)
    , docKey @(BUnit bunit)
    ]


instance FromHeader (DataHDUInfo ext btye bunit) where
  parseHeader _ = pure DataHDUInfo


-- -- this isn't that useful...
-- data DataHeader info = DataHeader
--   { info :: info
--   , common :: DataCommon
--   }
--
--
-- -- The library already inserts the NAXIS headers. No need to write them manually
-- instance (ToHeader info) => ToHeader (DataHeader info) where
--   toHeader (DataHeader info common) =
--     toHeader info <> toHeader common
--
--
-- -- The Header Docs need to contain info, axes, and common
-- instance (HeaderDoc info) => HeaderDoc (DataHeader info) where
--   headerDoc =
--     headerDoc @info
--       <> (headerDoc @DataHDUAxes)
--       <> (headerDoc @DataCommon)

data DataCommon = DataCommon
  { bzero :: BZero
  , bscale :: BScale
  , datamin :: Key Float "The minimum data value"
  , datamax :: Key Float "The maximum data value"
  , date :: Key LocalTime "UTC Date/Time of HDU creation, in the form: YYYY-MM-DDThh:mm:ss[.sss…]"
  }
  deriving (Generic, HeaderDoc, ToHeader, FromHeader)

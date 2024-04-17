{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module NSO.Fits.Generate.Headers where

import Data.Kind
import Data.Text (pack)
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc
import NSO.Fits.Generate.Key
import NSO.Prelude
import Telescope.Fits (BitPix (..))
import Telescope.Fits.Encoding (bitPixCode)
import Telescope.Fits.Types (HeaderRecord (..), KeywordRecord (..), Value (..))


-- TODO: NAXIS
-- TODO: Comments

-- data PrimaryHeaders = PrimaryHeaders
--   { telapse :: ...
--   ,
--   }
--
-- data PrimaryHeaders
--   = Telapse String -- eh..... what?

-- I could encode it in the type system itself using operators and symbols. Then auto-populate it. And auto-generate it.
-- how is that better than just generating it dynamically?
-- the static type-checking helps a lot when it's easier to verify that a smaller specification is correct, separate from the implementationg
--
-- Like in Servant: You specify the API becase you think that way, then you make sure that everything matches nicely
-- Or in that Experian API - the specification was gnarly, you wanted to make sure you had it right
--
-- each Keyword will need to be calculated one way or another
--
--
-- MATCHING: Which assumptions do we want the system to validate
-- 1. Type of the header matches the method of input
-- 2. Typed reading of L1 headers - We have a file(s) we want to use as input for a header

-- we go to pure strings
-- without the GADT you can't put the runtime type at the type-level

-- headSeconds :: Float -> Key Seconds d
-- headSeconds = Key . show

-- data Unit (unit :: Unit') (typ :: Type)

-- this is everything we need for the primnary header?
-- yeah, we can add other stuff to the docs

-- they don't all have to be a "Key"... could make instances for the other types
-- the thing is, the keyname is encoded in the record. Is that what I want? It does enforce that I hit all the required keys easily...
-- we could make it higher order

data PrimaryHeader f = PrimaryHeader
  -- { telapse :: Key Seconds "TELAPSE = DATE-END - DATE-BEG. Not always equal to the exposure time as multiple exposures could be combined"
  { wcsvalid :: Field f Bool "WCI data are correct"
  , dsetid :: Field f String "Unique ID of the dataset to which the frame belongs"
  , framevol :: Field f MB "Size of the frame on disk."
  , proctype :: Field f (Constant L2) "Controlled value list representing the degree of processing the frame has undergone since receipt at the DKIST data center."
  , -- asdf
    origin :: Field f (Constant "National Solar Observatory") "The organization or institution responsible for creating the FITS file."
  , lonpole :: Field f Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic, HeaderDoc)


type L2 = "L2"


-- class Documentation f where
--   documentation :: f Doc
--
--
-- -- instance Documentation (PrimaryHeader Doc) where
-- --   documentation

-- constantKey :: forall a typ desc. (KnownSymbol a) => Constant a -> Key typ desc
-- constantKey _ = KString (symbolVal @a Proxy)

-- Encode ALL Documentation at compile time
-- no, we are going for ALL compile type
-- data DataHDUHeader hdu = DataHDUHeader
--   {
--   }
--
--
-- data HDUInfo (ext :: Symbol) (btype :: UCD) (bunit :: BUnit)
--
--
-- type DataMagField = HDUInfo "Magnetic Field Strength" MagField Tesla

primaryHeader :: PrimaryHeader Key
primaryHeader =
  PrimaryHeader
    { wcsvalid = Key True
    , dsetid = Key "randomid"
    , framevol = Key $ MB 123
    , proctype = Key Constant
    , origin = Key Constant
    , lonpole = Key $ Degrees 456
    }


-- primaryHeaderDoc :: PrimaryHeader Doc
-- primaryHeaderDoc =
--   PrimaryHeader
--     { wcsvalid = Doc
--     , dsetid = Doc
--     }
--
-- magneticFieldStrength :: String -> DataHDUHeader DataMagField
-- magneticFieldStrength = undefined
--
-- Composition, not inheritance!

data GenericHDUHeader axes f = GenericHDUHeader
  -- these are always the same
  { xtension :: Field f ExtName "IMAGE"
  , bitpix :: Field f BitPix "Mandatory keyword describing the number of bits per pixel in the data. Permitted values are: 8, 16, 32, 64, -32, -64"
  , axes :: axes f
  , pcount :: Field f Int "Pcount"
  , gcount :: Field f Int "GCount"
  , extname :: Field f Int "Name of the HDU"
  , bunit :: Field f BUnit "The physical unit of the data values"
  , btype :: Field f UCD "Uniform Content Descriptor of what the data array represents."
  }
  deriving (Generic)


-- instance HeaderKeywords (GenericHDUHeader DataHDUAxes)
-- instance HeaderDoc (GenericHDUHeader DataHDUAxes)

type ExtNameField f = Field f ExtName "This is my extname"


-- what describes the data we need?
-- The axes are all the same, described by the thing below
data DataHDUHeader f = DataHDUHeader
  { extname :: ExtName
  , btype :: UCD
  , bunit :: BUnit
  }


-- what about plain old typeclasses

instance HeaderDoc DataHDUHeader where
  headerDoc = [] <> headerDoc @(Doc BUnit "Woot")


-- data HDUAxesHeader f = HDUAxesHeader
--   { naxis :: Field f Int "The number of axes"
--   -- no, we want to label each one separately. That means something different
--   , naxes :: [Field f Int ""]

data DataHDUAxes f = DataHDUAxes
  { naxis :: Field f Int "The number of axes"
  , naxis1 :: Field f Int "Lenth of Optical Depth axis"
  , naxis2 :: Field f Int "Lenth of Slit X axis"
  , naxis3 :: Field f Int "Always (1). Dummy WCS Y axis"
  }


-- deriving (Generic, HeaderKeywords, HeaderDoc)

-- remember, it doesn't have to me in this flat format!

-- DATA HDU (header order)
--------------
-- EXTNAME (required)
-- BITPIX (generated from data)
-- NAXIS (generated from data) (manual comments!) (manual description!)
-- NAXIS1 (generated from data) (comments manual!) (manual description!)
-- NAXIS2 (generated from data) (comments manual!) (manual description!)
-- PCOUNT (generated from type)
-- GCOUNT (generated from type)
-- BUNIT (required) (manual)
-- BTYPE (required) (manual)
-- CHECKSUM (generated)
-- DATASUM (generated)
--
-- NSO HEADERS (TELAPSE, ORIGIN, TELESCOP)
--
-- AXES CRPIX, PRVAL, CDELT, CUNIT, CTYPE
--
-- OTHER RANDOM STUFF
--
-- DATA HDU (documentation order)
-----------------------------------
-- (why not have it be exactly the same?)
--
-- the probe
--

-- hduHeader :: HDUHeader Key
-- hduHeader =
--   HDUHeader
--     { extname = Key $ ExtName "My HDU"
--     , bunit = Key Tesla
--     , btype = Key MagField
--     , bitpix = Key BPInt8
--     -- , end = Key Constant
--     }

type family Field f ktype desc where
  Field Key ktype desc = Key ktype ""
  Field Doc ktype desc = Doc ktype desc


-- TODO: Higher Order Type
--  in order to create a "documentation" PrimaryHeader and a "header" one with values, we need a higher order type

-- TODO: Documentation like this!
-- HDU: ExtName
--  bitpix
--  naxis
--  naxis1 -- describe the actual axis
--  naxis2 -- describe the actual axis
--

class HeaderKeywords f where
  headerKeywords :: f Key -> [KeywordRecord]
  default headerKeywords :: (Generic (f Key), GenHeaderKeywords (Rep (f Key))) => f Key -> [KeywordRecord]
  headerKeywords a = genHeaderKeywords @(Rep (f Key)) $ from a


class GenHeaderKeywords f where
  genHeaderKeywords :: f p -> [KeywordRecord]


instance (GenHeaderKeywords f) => GenHeaderKeywords (M1 D c f) where
  genHeaderKeywords (M1 a) = genHeaderKeywords @f a


-- constructor metadata
instance (GenHeaderKeywords f) => GenHeaderKeywords (M1 C c f) where
  genHeaderKeywords (M1 a) = genHeaderKeywords @f a


-- Selectors
instance (GenHeaderKeywords f, Selector s) => GenHeaderKeywords (M1 S s f) where
  genHeaderKeywords (M1 a) =
    let s = selName (undefined :: M1 S s f x)
     in fmap (setKeyword s) $ genHeaderKeywords @f a
   where
    setKeyword s d = d{_keyword = pack s}


instance (GenHeaderKeywords a, GenHeaderKeywords b) => GenHeaderKeywords (a :*: b) where
  genHeaderKeywords (a :*: b) = genHeaderKeywords a ++ genHeaderKeywords b


instance forall ktype comment i. (KeyValue ktype) => GenHeaderKeywords (K1 i (Key ktype comment)) where
  genHeaderKeywords (K1 (Key t)) = [KeywordRecord mempty (keyValue @ktype t) Nothing]


class KeyValue a where
  keyValue :: a -> Value


instance (KnownSymbol a) => KeyValue (Constant a) where
  keyValue _ = String $ pack $ symbolVal @a Proxy


instance KeyValue ExtName where
  keyValue (ExtName s) = String s


instance KeyValue BUnit where
  keyValue b = String (pack $ show b)


instance KeyValue UCD where
  keyValue MagField = String "phys.magField"
  keyValue DopplerVeloc = String "phys.dopplerVeloc"


instance KeyValue BitPix where
  keyValue b = Integer (bitPixCode b)

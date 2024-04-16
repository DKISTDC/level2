{-# LANGUAGE DataKinds #-}

module NSO.Fits.Generate.Headers where

import Data.Kind
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc
import NSO.Fits.Generate.Key
import NSO.Prelude
import Telescope.Fits (BitPix)


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


type family Field f ktype desc where
  Field Key ktype desc = ktype
  Field Doc ktype desc = Doc ktype desc


-- TODO: Higher Order Type
--  in order to create a "documentation" PrimaryHeader and a "header" one with values, we need a higher order type

type L2 = "L2"


-- class Documentation f where
--   documentation :: f Doc
--
--
-- -- instance Documentation (PrimaryHeader Doc) where
-- --   documentation

constantKey :: forall a typ desc. (KnownSymbol a) => Constant a -> Key typ desc
constantKey _ = KString (symbolVal @a Proxy)


-- TODO: we need to document END, but we aren't going to set it that way!
data GenericHeader f = GenericHeader
  { extname :: Field f ExtName "Name of HDU"
  , bunit :: Field f BUnit "The physical unit of the data values"
  , -- eh.... not sure this is right...
    btype :: Field f UCD "Uniform Content Descriptor of what the data array represents."
  , bitpix :: Field f BitPix "Mandatory keyword describing the number of bits per pixel in the data. Permitted values are: 8, 16, 32, 64, -32, -64"
  , end :: Field f (Constant "End") "This keyword has no associated value. Bytes 9 through 80 shall be filled with ASCII spaces (decimal 32 or hexadecimal 20)."
  }
  deriving (Generic, HeaderDoc)


-- Encode ALL Documentation at compile time
-- no, we are going for ALL compile type
data DataHDUHeader hdu = DataHDUHeader
  {
  }


data HDUInfo (ext :: Symbol) (btype :: UCD) (bunit :: BUnit)


type DataMagField = HDUInfo "Magnetic Field Strength" MagField Tesla


primaryHeader :: PrimaryHeader Key
primaryHeader =
  PrimaryHeader
    { wcsvalid = True
    , dsetid = "randomid"
    , framevol = MB 123
    , proctype = Constant
    , origin = Constant
    , lonpole = Degrees 456
    }


-- primaryHeaderDoc :: PrimaryHeader Doc
-- primaryHeaderDoc =
--   PrimaryHeader
--     { wcsvalid = Doc
--     , dsetid = Doc
--     }

magneticFieldStrength :: String -> DataHDUHeader DataMagField
magneticFieldStrength = undefined


-- these aren't a list. They can be
data DataHDU a

-- TODO: Documentation like this!
-- HDU: ExtName
--  bitpix
--  naxis
--  naxis1 -- describe the actual axis
--  naxis2 -- describe the actual axis
--  naxis3
--

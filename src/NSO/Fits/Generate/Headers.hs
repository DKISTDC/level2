module NSO.Fits.Generate.Headers where

import Data.Kind
import GHC.TypeLits
import NSO.Fits.Generate.Key
import NSO.Prelude


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

data PrimaryHeader = PrimaryHeader
  { telapse :: Key Seconds "TELAPSE = DATE-END - DATE-BEG. Not always equal to the exposure time as multiple exposures could be combined"
  , wcsvalid :: Key Bool "WCI data are correct"
  , dsetid :: Key String "Unique ID of the dataset to which the frame belongs"
  , framevol :: Key MB "Size of the frame on disk."
  , proctype :: Key (Constant "L2") "Controlled value list representing the degree of processing the frame has undergone since receipt at the DKIST data center."
  , -- asdf
    origin :: Key (Constant "National Solar Observatory") "The organization or institution responsible for creating the FITS file."
  , lonpole :: Key Deg "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic)


data RequiredHDUHeader = RequiredHDUHeader
  -- some of these are required. We still need to document them!
  { extName :: Key String "Ext name description"
  , bType :: Key Seconds "btype description"
  }
  deriving (Generic)

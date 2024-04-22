{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.Headers where

import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc as Doc
import NSO.Fits.Generate.Keywords
import NSO.Fits.Generate.Types
import NSO.Prelude
import NSO.Types.Common (Id (..))


-- DONE: automatic type-based comments
-- DONE: custom comments with custom newtype (cleaner)
-- DONE: comments for bitpix, naxes and other auto-gen keywords.

-- COMMENTS -----------------------------
-- 1. Some units (ktype) need a comment. some don't
-- 2. Some field names need units. some don't
--
-- how hard would it be to make custom ones for each?

data PrimaryHeader = PrimaryHeader
  { telapse :: Key Seconds "TELAPSE = DATE-END - DATE-BEG. Not always equal to the exposure time as multiple exposures could be combined"
  , wcsvalid :: Key (Constant True) "WCI data are correct"
  , dsetid :: Key (Id FrameY) "Unique ID of the dataset to which the frame belongs"
  , framevol :: Key MB "Size of the frame on disk."
  , proctype :: Key (Constant "L2") "Controlled value list representing the degree of processing the frame has undergone since receipt at the DKIST data center."
  , origin :: Key (Constant "National Solar Observatory") "The organization or institution responsible for creating the FITS file."
  , lonpole :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


primaryHeader :: Id FrameY -> PrimaryHeader
primaryHeader di =
  PrimaryHeader
    { telapse = Key (Seconds 123)
    , wcsvalid = Key Constant
    , dsetid = Key di
    , framevol = Key $ MB 123
    , proctype = Key Constant
    , origin = Key Constant
    , lonpole = Key (Degrees 40)
    }

-- TEST: lifts comments from DataHDUAxes header keywords
-- TEST: does NOT lift value from DataHDUAxes header keywords
--
-- TODO: WCS (CRPIX, CRVAL, CUNIT, CTYPE, etc)

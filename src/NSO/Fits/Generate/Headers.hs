{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.Headers where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Fits (toFloat, toText)
import Data.Text (unpack)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc as Doc
import NSO.Fits.Generate.Keywords
import NSO.Fits.Generate.Types
import NSO.Prelude
import NSO.Types.Common (Id (..))
import NSO.Types.Inversion (Inversion)
import Telescope.Fits as Fits


-- DONE: automatic type-based comments
-- DONE: custom comments with custom newtype (cleaner)
-- DONE: comments for bitpix, naxes and other auto-gen keywords.
-- DONE: FILENAME - based on input filename
--
-- FIX: WCS: do they belong in data hdus or primary?
--
-- TODO: DSETID - the inversion id?
-- TODO: FRAMEVOL - estimate based on the dimensions, bitpix, and average header size
-- TODO: HEADVERS - current version number for the spec
-- TODO: HEAD_URL - create a url that links to this
-- TODO: INFO_URL - what's the difference? Is there another documentation page? Does it have a different spec?
-- TODO: FILE_ID - UUID for this frame - where do I need to store this?
-- TODO: PROV_URL - create a provenance URL page... public domain?
-- TODO: CHECKSUM
-- TODO: DATASUM

-- COMMENTS -----------------------------
-- 1. Some units (ktype) need a comment. some don't
-- 2. Some field names need units. some don't
--
-- how hard would it be to make custom ones for each?

data PrimaryHeader = PrimaryHeader
  { telapse :: Key Seconds "TELAPSE = DATE-END - DATE-BEG. Not always equal to the exposure time as multiple exposures could be combined"
  , origin :: Key (Constant "National Solar Observatory") "The organization or institution responsible for creating the FITS file."
  , telescope :: Key (Constant "Daniel K. Inouye Solar Telescope") "The telescope used to acquire the data associated with the header."
  , obsrvtry :: Key (Constant "Haleakala High Altitude Observatory Site") "A character string identifying the physical entity, located in a defined location, which provides the resources necessary for the installation of an instrument"
  , network :: Key (Constant "NSF-DKIST") "Organizational entity of a series of instruments with similar characteristics or goals that operates in some coordinated manner or produces data with some common purpose"
  , object :: Object
  , dsetid :: Key (Id Inversion) "Unique ID of the dataset to which the frame belongs"
  , framevol :: Key MB "Size of the frame on disk."
  , proctype :: Key (Constant "L2") "Controlled value list representing the degree of processing the frame has undergone since receipt at the DKIST data center."
  , solarnet :: Key (Constant "1.0") "SOLARNET compliance: 1.0: Fully compliant 0.5: Partially compliant"
  , filename :: Key String "Name of current file"
  , level :: Key (Constant 2) "The level of the current data"
  , headvers :: Key String "Version of this spec used during processing"
  , headUrl :: Key Url "Link to documentation for the headers of this frame"
  , infoUrl :: Key Url "Link to documentation for this frame"
  , fileId :: Key String "Unique ID of this FITS file"
  , provUrl :: Key Url "Link to provenance for this file"
  , lonpole :: Key Degrees "Native longitude of the celestial pole in Helioprojective coordinate system"
  , dateBeg :: Key DateTime "Start date and time of light exposure for the frame"
  , dateEnd :: Key DateTime "End date and time of light exposure for the frame"
  , dateAvg :: Key DateTime "Date/Time of the midpoint of the frame. (DATE-END - DATE_BEG) / 2"
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


primaryHeader :: (MonadThrow m) => Header -> Id Inversion -> m PrimaryHeader
primaryHeader l1 i = do
  -- can I avoid typing these keys in twice?
  dateBeg <- lookupL1 "DATE-BEG" toDate l1
  dateEnd <- lookupL1 "DATE-END" toDate l1
  dateAvg <- lookupL1 "DATE-AVG" toDate l1
  telapse <- lookupL1 "TELAPSE" toFloat l1
  object <- lookupL1 "OBJECT" toText l1

  pure $
    PrimaryHeader
      { telapse = Key (Seconds telapse)
      , telescope = Key Constant
      , obsrvtry = Key Constant
      , network = Key Constant
      , object = Object object
      , dsetid = Key i
      , framevol = Key $ MB 123
      , proctype = Key Constant
      , solarnet = Key Constant
      , filename = Key $ frameFilename dateBeg i
      , level = Key Constant
      , headvers = Key "TODO"
      , headUrl = Key (Url "https://TODO")
      , infoUrl = Key (Url "https://TODO")
      , fileId = Key "TODO"
      , provUrl = Key (Url "https://TODO")
      , origin = Key Constant
      , lonpole = Key (Degrees 40)
      , dateBeg = Key dateBeg
      , dateEnd = Key dateEnd
      , dateAvg = Key dateAvg
      }


data TelescopeHeader = TelescopeHeader
  { wcsvalid :: Key (Constant True) "WCI data are correct"
  , wcsaxes :: Key (Constant 3) ""
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


-- TODO: this belongs in the data, not the primary!
telescopeHeader :: (MonadThrow m) => Header -> m TelescopeHeader
telescopeHeader h = do
  pure $
    TelescopeHeader
      { wcsvalid = Key Constant
      , wcsaxes = Key Constant
      }


lookupL1 :: (MonadThrow m) => Text -> (Value -> Maybe a) -> Header -> m a
lookupL1 k fromValue h =
  case fromValue =<< Fits.lookup k h of
    Nothing -> throwM (MissingL1Key (unpack k))
    Just t -> pure t


toDate :: Value -> Maybe DateTime
toDate v = DateTime <$> toText v


newtype L1Filename = L1Filename FilePath


data FitsGenError
  = MissingL1Key String
  | MissingL1HDU FilePath
  deriving (Show, Exception)


-- VISP_2023_10_16T23_55_59_513_00589600_I_ADDMM_L1.fits
-- = INSTRUMENT DATETIME DATASET L1
-- => DATETIME FRAMEID L2
-- 2023_10_16T23_55_59_513_00589600_inv_290834_L2.fits
frameFilename :: DateTime -> Id Inversion -> String
frameFilename (DateTime start) iv =
  addExtension $
    unpack $
      T.toUpper $
        T.intercalate
          "_"
          [ T.replace "." "_" iv.fromId
          , start
          , "L2"
          ]
 where
  addExtension f = f <> ".fits"

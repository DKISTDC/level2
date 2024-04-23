{-# LANGUAGE AllowAmbiguousTypes #-}

module NSO.Fits.Generate.Headers where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Fits (toText, toFloat)
import Data.Text (unpack)
import GHC.Generics
import GHC.TypeLits
import NSO.Fits.Generate.Doc as Doc
import NSO.Fits.Generate.Keywords
import NSO.Fits.Generate.Types
import NSO.Prelude
import NSO.Types.Common (Id (..))
import Telescope.Fits as Fits


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
  , dateBeg :: Key Time "Start date and time of light exposure for the frame"
  , dateEnd :: Key Time "End date and time of light exposure for the frame."
  }
  deriving (Generic, HeaderDoc, HeaderKeywords)


primaryHeader :: (MonadThrow m) => Header -> Id FrameY -> m PrimaryHeader
primaryHeader l1 di = do

  -- can I avoid typing these keys in twice?
  dateBeg <- lookupL1 "DATE-BEG" toText l1
  dateEnd <- lookupL1 "DATE-END" toText l1
  telapse <- lookupL1 "TELAPSE" toFloat l1

  pure $
    PrimaryHeader
      { telapse = Key (Seconds telapse)
      , wcsvalid = Key Constant
      , dsetid = Key di
      , framevol = Key $ MB 123
      , proctype = Key Constant
      , origin = Key Constant
      , lonpole = Key (Degrees 40)
      , dateBeg = Key (Time dateBeg)
      , dateEnd = Key (Time dateEnd)
      }


lookupL1 :: (MonadThrow m) => Text -> (Value -> Maybe a) -> Header -> m a
lookupL1 k fromValue h =
  case fromValue =<< Fits.lookup k h of
    Nothing -> throwM (MissingL1Key (unpack k))
    Just t -> pure t


data FitsGenError
  = MissingL1Key String
  | MissingL1HDU FilePath
  deriving (Show, Exception)

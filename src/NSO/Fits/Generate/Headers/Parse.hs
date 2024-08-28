module NSO.Fits.Generate.Headers.Parse where

import Control.Monad.Catch (Exception)
import Data.Fits (KeywordRecord, getKeywords, toText)
import Data.Text (unpack)
import Effectful
import Effectful.Error.Static
import NSO.Fits.Generate.Headers.Types
import NSO.Prelude
import Telescope.Fits as Fits


lookupKey :: (Monad m) => Text -> (Value -> Maybe a) -> Header -> m (Maybe a)
lookupKey k fromValue h =
  let mk = Fits.lookup k h
   in case fromValue =<< mk of
        Nothing -> pure Nothing
        Just t -> pure (Just t)


requireKey :: (Error ParseKeyError :> es) => Text -> (Value -> Maybe a) -> Header -> Eff es a
requireKey k fromValue h =
  let mk = Fits.lookup k h
   in case fromValue =<< mk of
        Nothing -> throwError (MissingKey (unpack k))
        Just t -> pure t


findKey :: (KeywordRecord -> Maybe a) -> Header -> Maybe a
findKey p h = do
  listToMaybe $ mapMaybe p $ getKeywords h


toDate :: Value -> Maybe DateTime
toDate v = DateTime <$> toText v


data ParseKeyError
  = MissingKey String
  deriving (Show, Exception, Eq)

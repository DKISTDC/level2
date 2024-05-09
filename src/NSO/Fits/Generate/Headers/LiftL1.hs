module NSO.Fits.Generate.Headers.LiftL1 where

import Control.Monad.Catch (Exception)
import Data.Fits (toText)
import Data.Text (unpack)
import Effectful
import Effectful.Error.Static
import NSO.Fits.Generate.Headers.Types
import NSO.Prelude
import Telescope.Fits as Fits


lookupL1 :: (Monad m) => Text -> (Value -> Maybe a) -> Header -> m (Maybe a)
lookupL1 k fromValue h =
  let mk = Fits.lookup k h
   in case fromValue =<< mk of
        Nothing -> pure Nothing
        Just t -> pure (Just t)


requireL1 :: (Error LiftL1Error :> es) => Text -> (Value -> Maybe a) -> Header -> Eff es a
requireL1 k fromValue h =
  let mk = Fits.lookup k h
   in case fromValue =<< mk of
        Nothing -> throwError (MissingL1Key (unpack k))
        Just t -> pure t


toDate :: Value -> Maybe DateTime
toDate v = DateTime <$> toText v


data LiftL1Error
  = MissingL1Key String
  | MissingL1HDU FilePath
  deriving (Show, Exception)

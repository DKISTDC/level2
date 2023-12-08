module App.Config where

import Effectful.Request
import NSO.Prelude
import System.Environment


data Services = Services
  {metadata :: Service}


type IsMock = Bool
initServices :: IO (Services, IsMock)
initServices = do
  mock <- parseServices <$> lookupEnv "SERVICES"
  meta <- parseService =<< getEnv "METADATA_API"
  pure (Services meta, mock)
 where
  parseServices (Just "MOCK") = True
  parseServices _ = False


parseService :: String -> IO Service
parseService u =
  case service (cs u) of
    Nothing -> fail $ "Could not parse service url: " <> cs u
    Just s -> pure s

module App.Config where

import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Effectful.Request
import NSO.Prelude
import System.Environment
import Web.Hyperbole


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


document :: BL.ByteString -> BL.ByteString
document cnt =
  [i|<html>
    <head>
      <title>Level2</title>
      <script type="text/javascript">#{scriptEmbed}</script>
      <style type="text/css">#{cssResetEmbed}</style>
      <style type="text/css">body { background-color: \#d3dceb }</style>
    </head>
    <body>#{cnt}</body>
  </html>|]

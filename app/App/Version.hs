module App.Version where

import Data.Version (showVersion)
import NSO.Prelude
import Paths_nso_level2 (version)


appVersion :: String
appVersion = showVersion version

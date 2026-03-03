{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Version where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import NSO.Prelude
import Network.URI (URI, parseURI)
import Paths_nso_level2 (version)


newtype GitVersion = GitVersion {value :: Text}
  deriving newtype (Show)
newtype AppVersion = AppVersion {value :: Text}
  deriving newtype (Show)


gitVersion :: GitVersion
gitVersion = GitVersion $ T.take 7 $(gitHash)


gitVersionURI :: URI
gitVersionURI =
  let vers = gitVersion.value
      url = [i|https://github.com/DKISTDC/level2/commit/#{vers}|]
   in case parseURI url of
        Nothing -> error $ "Could not parse Git Version URI: " <> url
        Just u -> u


appVersion :: AppVersion
appVersion = AppVersion $ cs $ showVersion version

module App.Effect.FileManager where

import App.Types (AppDomain)
import Data.Tagged
import Effectful.Globus hiding (Id)
import NSO.Files.DKIST as DKIST
import NSO.Files.Scratch (Scratch)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Inversion (Inversion)
import Web.Hyperbole.Data.URI hiding (Path)


-- File Manager ----------------------------

data FileLimit
  = Folders Int
  | Files Int


fileManagerSelectUrl :: FileLimit -> Text -> AppDomain -> URI -> URI -> URI
fileManagerSelectUrl lmt lbl domain submitUrl cancelUrl =
  [uri|https://app.globus.org/file-manager|]
    { uriQuery =
        queryString
          [ ("method", Just "POST")
          , ("action", Just $ cs (uriToText $ appUrl submitUrl))
          , ("folderlimit", Just $ cs $ folderLimit lmt)
          , ("filelimit", Just $ cs $ fileLimit lmt)
          , ("cancelurl", Just $ cs (uriToText $ appUrl cancelUrl))
          , ("label", Just $ cs lbl)
          ]
    }
 where
  -- <> "&origin_id=d26bd00b-62dc-40d2-9179-7aece2b8c437"
  -- <> "&origin_path=%2Fdata%2Fpid_1_118%2F"
  -- <> "&two_pane=true"
  fileLimit (Folders _) = "0"
  fileLimit (Files n) = show n

  folderLimit (Folders n) = show n
  folderLimit (Files _) = "0"

  -- convert a url to redirect to this server
  appUrl :: URI -> URI
  appUrl u =
    -- request came from current server
    -- should come from app config server?
    URI "https://" (Just $ URIAuth "" (cs domain.unTagged) "") u.uriPath u.uriQuery ""


openDir :: Id Collection -> Path s Dir a -> URI
openDir origin dir =
  [uri|https://app.globus.org/file-manager|]
    { uriQuery =
        queryString
          [ ("origin_id", Just $ cs origin.fromId)
          , ("origin_path", Just $ "/" <> cs dir.filePath)
          ]
    }


-- Specific File Locations ------------------------------------------

openPublish :: Path DKIST Dir Inversion -> URI
openPublish = openDir (Id DKIST.endpoint.unTagged)


-- DEBUG ONLY: hard coded aasgard
-- TODO: switch to scratch
openInversion :: Path Scratch Dir Inversion -> URI
openInversion = openDir (Id "20fa4840-366a-494c-b009-063280ecf70d")

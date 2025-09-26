module App.Effect.FileManager where

import App.Types (AppDomain)
import Data.Tagged
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import NSO.Files.DKIST as DKIST
import NSO.Files.RemoteFolder
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


openDir :: Globus.Id Collection -> Path s Dir a -> URI
openDir origin dir =
  [uri|https://app.globus.org/file-manager|]
    { uriQuery =
        queryString
          [ ("origin_id", Just $ cs origin.unTagged)
          , ("origin_path", Just $ cs dir.filePath)
          ]
    }


openRemoteDir :: Remote sys -> Path sys Dir a -> URI
openRemoteDir r p = openDir r.collection $ remotePath r p


-- Specific File Locations ------------------------------------------

openPublish :: Remote Publish -> Path Publish Dir Inversion -> URI
openPublish = openRemoteDir


openInversion :: Remote Scratch -> Path Scratch Dir Inversion -> URI
openInversion = openRemoteDir

module App.Effect.FileManager where

import App.Types (AppDomain)
import Data.Tagged
import Effectful.Globus hiding (Id)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Inversion (Inversion)
import Web.Hyperbole.Data.URI


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


-- DEBUG ONLY: hard coded aasgard
fileManagerOpenInv :: Path' Dir Inversion -> URI
fileManagerOpenInv = fileManagerOpenDir (Id "20fa4840-366a-494c-b009-063280ecf70d")


fileManagerOpenDir :: Id Collection -> Path' Dir a -> URI
fileManagerOpenDir origin dir =
  [uri|https://app.globus.org/file-manager|]
    { uriQuery =
        queryString
          [ ("origin_id", Just $ cs origin.fromId)
          , ("origin_path", Just $ "/" <> cs dir.filePath)
          ]
    }

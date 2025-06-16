module App.Effect.FileManager where

import App.Types (AppDomain)
import Data.Tagged
import Effectful.Globus hiding (Id)
import NSO.Image.Fits (L2FrameFits)
import NSO.Prelude
import NSO.Types.Common
import Web.View.Types.Url


-- File Manager ----------------------------

data FileLimit
  = Folders Int
  | Files Int


fileManagerSelectUrl :: FileLimit -> Text -> AppDomain -> Url -> Url -> Url
fileManagerSelectUrl lmt lbl domain submitUrl cancelUrl =
  Url
    "https://"
    "app.globus.org"
    ["file-manager"]
    [ ("method", Just "POST")
    , ("action", Just $ cs (renderUrl $ appUrl submitUrl))
    , ("folderlimit", Just $ cs $ folderLimit lmt)
    , ("filelimit", Just $ cs $ fileLimit lmt)
    , ("cancelurl", Just $ cs (renderUrl $ appUrl cancelUrl))
    , ("label", Just $ cs lbl)
    ]
 where
  -- <> "&origin_id=d26bd00b-62dc-40d2-9179-7aece2b8c437"
  -- <> "&origin_path=%2Fdata%2Fpid_1_118%2F"
  -- <> "&two_pane=true"
  fileLimit (Folders _) = "0"
  fileLimit (Files n) = show n

  folderLimit (Folders n) = show n
  folderLimit (Files _) = "0"

  -- convert a url to redirect to this server
  appUrl :: Url -> Url
  appUrl u =
    -- request came from current server
    -- should come from app config server?
    Url "https://" domain.unTagged u.path u.query


-- DEBUG ONLY: hard coded aasgard
fileManagerOpenInv :: Path' Dir L2FrameFits -> Url
fileManagerOpenInv = fileManagerOpenDir (Id "20fa4840-366a-494c-b009-063280ecf70d")


fileManagerOpenDir :: Id Collection -> Path' Dir a -> Url
fileManagerOpenDir origin dir =
  Url
    "https://"
    "app.globus.org"
    ["file-manager"]
    [ ("origin_id", Just $ cs origin.fromId)
    , ("origin_path", Just $ "/" <> cs dir.filePath)
    ]

module App.Globus
  ( GlobusClient (..)
  , Globus
  , authUrl
  , accessToken
  , fileManagerUrl
  , transferStatus
  , Uri
  , Id
  , Token
  , Task (..)
  , TaskStatus (..)
  , Tagged (..)
  , Token' (..)
  , Id' (..)
  , runGlobus
  , getAccessToken
  , saveAccessToken
  , clearAccessToken
  , taskPercentComplete
  , initDownload
  , initUpload
  , initTransferDataset
  , FileLimit (..)
  , TransferForm
  , UploadFiles
  , DownloadFolder
  , Auth (..)
  , runAuth
  , requireLogin
  , loginUrl
  , getRedirectUri
  , RedirectPath (..)
  ) where

import App.Types
import Data.Tagged
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import GHC.Generics
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import Network.Globus.Transfer (taskPercentComplete)
import Network.HTTP.Types (QueryItem)
import System.FilePath
import Web.FormUrlEncoded (parseMaybe)
import Web.Hyperbole
import Web.Hyperbole.Effect (Host (..), Request (..))
import Web.Hyperbole.Forms
import Web.Hyperbole.Route (Route (..))
import Web.View as WebView
import Web.View.Types.Url (Segment)


authUrl :: (Globus :> es) => Uri Redirect -> Eff es WebView.Url
authUrl red = do
  uri <- send $ AuthUrl red [TransferAll] (State "_")
  pure $ convertUrl uri
 where
  convertUrl :: Uri a -> WebView.Url
  convertUrl u =
    let Query ps = u.params
     in Url{scheme = scheme u.scheme, domain = u.domain, path = u.path, query = map query ps}
  scheme Https = "https://"
  scheme Http = "http://"
  query :: (Text, Maybe Text) -> QueryItem
  query (t, mt) = (cs t, cs <$> mt)


accessToken :: (Globus :> es) => Uri Redirect -> Token Exchange -> Eff es (Token Access)
accessToken red tok = do
  send $ AccessToken tok red


-- accessToken :: Globus -> Uri Redirect -> Token Exchange -> m TokenResponse
-- accessToken (Token cid) (Token sec) red (Token code) =

data FileLimit
  = Folders Int
  | Files Int


fileManagerUrl :: (Route r) => FileLimit -> r -> Text -> Request -> Url
fileManagerUrl lmt r lbl req =
  Url
    "https://"
    "app.globus.org"
    ["file-manager"]
    [ ("method", Just "POST")
    , ("action", Just $ cs (renderUrl submitUrl))
    , ("folderlimit", Just $ cs $ folderLimit lmt)
    , ("filelimit", Just $ cs $ fileLimit lmt)
    , ("cancelurl", Just $ cs (renderUrl currentUrl))
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

  serverUrl :: [Text] -> Url
  serverUrl p = Url "https://" (cs req.host.text) p []

  currentUrl :: Url
  currentUrl = serverUrl req.path

  submitUrl :: Url
  submitUrl =
    serverUrl $ routePath r


data TransferForm = TransferForm
  { label :: Field Text
  , -- , endpoint :: Field Text
    path :: Field Text
  , endpoint_id :: Field Text
  }
  deriving (Generic, Form)


data DownloadFolder = DownloadFolder
  { folder :: Maybe FilePath
  }
  deriving (Generic)
instance Form DownloadFolder where
  formParse f = do
    DownloadFolder <$> parseMaybe "folder[0]" f


data UploadFiles = UploadFiles
  { invResPre :: FilePath
  , invResMod :: FilePath
  , perOri :: FilePath
  }
  deriving (Generic)
instance Form UploadFiles where
  formParse f = do
    fs <- multi "file"
    invResPre <- findFile "inv_res_pre.fits" fs
    invResMod <- findFile "inv_res_mod.fits" fs
    perOri <- findFile "per_ori.fits" fs
    pure UploadFiles{invResPre, invResMod, perOri}
   where
    sub :: Text -> Int -> Text
    sub t n = t <> "[" <> cs (show n) <> "]"
    multi t = do
      sub0 <- parseMaybe (sub t 0) f
      sub1 <- parseMaybe (sub t 1) f
      sub2 <- parseMaybe (sub t 2) f
      pure $ catMaybes [sub0, sub1, sub2]
    findFile :: FilePath -> [FilePath] -> Either Text FilePath
    findFile file fs = do
      if file `elem` fs
        then pure file
        else Left $ "Missing required file: " <> cs file


transferStatus :: (Hyperbole :> es, Globus :> es, Auth :> es) => Id Task -> Eff es Task
transferStatus (Id ti) = do
  tok <- getAccessToken >>= expectAuth
  send $ StatusTask tok (Tagged ti)


initTransfer :: (Hyperbole :> es, Globus :> es, Auth :> es) => (Globus.Id Submission -> TransferRequest) -> Eff es (Id Task)
initTransfer toRequest = do
  acc <- getAccessToken >>= expectAuth
  initTransferWithAccess acc toRequest


initTransferWithAccess :: (Globus :> es) => Token Access -> (Globus.Id Submission -> TransferRequest) -> Eff es (Id Task)
initTransferWithAccess acc toRequest = do
  sub <- send $ Globus.SubmissionId acc
  res <- send $ Globus.Transfer acc $ toRequest sub
  pure $ Id res.task_id.unTagged


initUpload :: (Hyperbole :> es, Globus :> es, Auth :> es) => TransferForm -> UploadFiles -> Id Inversion -> Eff es (Id Task)
initUpload tform up ii = do
  initTransfer transferRequest
 where
  transferRequest :: Globus.Id Submission -> TransferRequest
  transferRequest submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just tform.label.value
      , source_endpoint = Tagged tform.endpoint_id.value
      , destination_endpoint = level2Scratch
      , data_ = map transferItem [up.invResMod, up.invResPre, up.perOri]
      , sync_level = SyncChecksum
      }

  transferItem :: String -> TransferItem
  transferItem f =
    TransferItem
      { data_type = DataType
      , source_path = cs tform.path.value </> f
      , destination_path = inversionScratchPath ii </> f
      , recursive = False
      }


initDownload :: (Hyperbole :> es, Globus :> es, Auth :> es) => TransferForm -> DownloadFolder -> [Dataset] -> Eff es (Id Task)
initDownload tform df ds = do
  initTransfer downloadTransferRequest
 where
  downloadTransferRequest :: Globus.Id Submission -> TransferRequest
  downloadTransferRequest submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just tform.label.value
      , source_endpoint = dkistEndpoint
      , destination_endpoint = Tagged tform.endpoint_id.value
      , data_ = map (datasetTransferItem destinationFolder) ds
      , sync_level = SyncChecksum
      }
   where
    destinationFolder :: FilePath
    destinationFolder =
      -- If they didn't select a folder, use the current folder
      case df.folder of
        Just f -> cs tform.path.value </> cs f
        Nothing -> cs tform.path.value


initTransferDataset :: (Globus :> es) => Token Access -> Dataset -> Eff es (Id Task, FilePath)
initTransferDataset acc d = do
  t <- initTransferWithAccess acc transfer
  -- we have to add the folder, because it's auto-added, I think
  let destFolder = datasetScratchPath d </> cs d.datasetId.fromId
  pure (t, destFolder)
 where
  transfer :: Globus.Id Submission -> TransferRequest
  transfer submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just $ "Dataset " <> d.datasetId.fromId
      , source_endpoint = dkistEndpoint
      , destination_endpoint = level2Scratch
      , data_ = [datasetTransferItem (datasetScratchPath d) d]
      , sync_level = SyncChecksum
      }


dkistEndpoint :: Globus.Id Collection
dkistEndpoint = Tagged "d26bd00b-62dc-40d2-9179-7aece2b8c437"


level2Scratch :: Globus.Id Collection
level2Scratch = Tagged "20fa4840-366a-494c-b009-063280ecf70d"


level2ScratchPath :: FilePath
level2ScratchPath = "~/level2"


inversionScratchPath :: Id Inversion -> FilePath
inversionScratchPath ii = level2ScratchPath </> cs ii.fromId


datasetScratchPath :: Dataset -> FilePath
datasetScratchPath _ =
  level2ScratchPath


datasetTransferItem :: FilePath -> Dataset -> TransferItem
datasetTransferItem dest d =
  TransferItem
    { data_type = DataType
    , source_path = datasetSourcePath
    , destination_path = dest
    , recursive = True
    }
 where
  datasetSourcePath :: FilePath
  datasetSourcePath = "/data" </> cs d.primaryProposalId.fromId </> cs d.datasetId.fromId


-- Authentication!
data Auth :: Effect where
  LoginUrl :: RedirectPath -> Auth m Url
  RedirectUri :: RedirectPath -> Auth m (Uri Globus.Redirect)


type instance DispatchOf Auth = 'Dynamic


runAuth
  :: (Globus :> es, Route r)
  => AppDomain
  -> r
  -> Eff (Auth : es) a
  -> Eff es a
runAuth dom r = interpret $ \_ -> \case
  LoginUrl rp -> do
    authUrl $ redirectUri dom r rp
  -- \$ redirectRoute rp
  RedirectUri rp -> do
    pure $ redirectUri dom r rp


-- \$ redirectRoute rp

-- WARNING:  until we update the globus app, it only allows /redirect, no query, no nothing
redirectUri :: (Route r) => AppDomain -> r -> RedirectPath -> Uri Globus.Redirect
redirectUri dom r (RedirectPath _) = do
  -- let path = T.intercalate "/" rp
  Uri Https (cs dom.unTagged) (routePath r) (Query []) -- (Query [("path", Just path)])


getRedirectUri :: (Hyperbole :> es, Auth :> es) => Eff es (Uri Globus.Redirect)
getRedirectUri = do
  rp <- redirectPath
  send $ RedirectUri rp


expectAuth :: (Hyperbole :> es, Auth :> es) => Maybe a -> Eff es a
expectAuth Nothing = do
  u <- loginUrl
  redirect u
expectAuth (Just a) = pure a


loginUrl :: (Hyperbole :> es, Auth :> es) => Eff es Url
loginUrl = do
  rp <- redirectPath
  send $ LoginUrl rp


redirectPath :: (Hyperbole :> es) => Eff es RedirectPath
redirectPath = do
  r <- request
  pure $ RedirectPath r.path


getAccessToken :: (Hyperbole :> es, Auth :> es) => Eff es (Maybe (Token Access))
getAccessToken = do
  fmap Tagged <$> session "globus"


saveAccessToken :: (Hyperbole :> es) => Token Access -> Eff es ()
saveAccessToken (Tagged acc) = setSession "globus" acc


clearAccessToken :: (Hyperbole :> es) => Eff es ()
clearAccessToken = clearSession "globus"


requireLogin :: (Hyperbole :> es, Auth :> es) => Eff es ()
requireLogin = do
  _ <- getAccessToken >>= expectAuth
  pure ()


newtype RedirectPath = RedirectPath [Segment]
  deriving (Show, Eq)


instance Route RedirectPath where
  defRoute = RedirectPath []
  routePath (RedirectPath ss) = ss
  matchRoute ss = Just $ RedirectPath ss

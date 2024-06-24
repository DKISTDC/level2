module App.Globus
  ( GlobusClient (..)
  , Globus
  , authUrl
  , accessToken
  , fileManagerUrl
  , transferStatus
  , Uri
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
  , UploadFiles (..)
  , DownloadFolder
  , Auth (..)
  , runAuth
  , requireLogin
  , runWithAccess
  , loginUrl
  , getRedirectUri
  , RedirectPath (..)
  , GlobusEndpoint (..)
  , inversionUploadedFiles
  , Timestamps
  , InvResults
  , InvProfile
  , OrigProfile
  ) where

import App.Types
import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus
import Effectful.Globus qualified as Globus
import Effectful.Reader.Dynamic
import GHC.Generics
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common as App
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


data UploadFiles t = UploadFiles
  { invProfile :: Path' t InvProfile
  , invResults :: Path' t InvResults
  , origProfile :: Path' t OrigProfile
  , timestamps :: Path' t Timestamps
  }
  deriving (Generic, Show)
instance Form (UploadFiles Filename) where
  formParse f = do
    fs <- multi "file"
    invProfile <- findFile fileInvProfile fs
    invResults <- findFile fileInvResults fs
    origProfile <- findFile fileOrigProfile fs
    timestamps <- findFile fileTimestamps fs
    pure UploadFiles{invResults, invProfile, origProfile, timestamps}
   where
    sub :: Text -> Int -> Text
    sub t n = t <> "[" <> cs (show n) <> "]"
    multi t = do
      sub0 <- parseMaybe (sub t 0) f
      sub1 <- parseMaybe (sub t 1) f
      sub2 <- parseMaybe (sub t 2) f
      pure $ catMaybes [sub0, sub1, sub2]

    findFile :: Path' Filename a -> [FilePath] -> Either Text (Path' Filename a)
    findFile (Path file) fs = do
      if file `elem` fs
        then pure $ Path file
        else Left $ "Missing required file: " <> cs file


transferStatus :: (Reader (Token Access) :> es, Globus :> es) => App.Id Task -> Eff es Task
transferStatus (Id ti) = do
  acc <- ask
  send $ StatusTask acc (Tagged ti)


-- initTransfer :: (Hyperbole :> es, Globus :> es, Auth :> es) => (Globus.Id Submission -> TransferRequest) -> Eff es (Id Task)
-- initTransfer toRequest = do

initTransfer :: (Globus :> es, Reader (Token Access) :> es) => (Globus.Id Submission -> TransferRequest) -> Eff es (App.Id Task)
initTransfer toRequest = do
  acc <- ask
  sub <- send $ Globus.SubmissionId acc
  res <- send $ Globus.Transfer acc $ toRequest sub
  pure $ Id res.task_id.unTagged


initUpload :: (Hyperbole :> es, Globus :> es, Auth :> es, Reader (GlobusEndpoint App) :> es) => TransferForm -> UploadFiles Filename -> App.Id Inversion -> Eff es (App.Id Task)
initUpload tform up ii = do
  level2 <- ask @(GlobusEndpoint App)
  requireLogin $ initTransfer (transferRequest level2)
 where
  transferRequest :: GlobusEndpoint App -> Globus.Id Submission -> TransferRequest
  transferRequest endpoint submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just tform.label.value
      , source_endpoint = Tagged tform.endpoint_id.value
      , destination_endpoint = endpoint.collection
      , data_ = map transferItem [genericFile up.invResults, genericFile up.invProfile, genericFile up.origProfile]
      , sync_level = SyncChecksum
      }
   where
    transferItem :: Path' Filename () -> TransferItem
    transferItem (Path f) =
      let (Path sf) = inversionScratchFile endpoint ii (Path f)
       in TransferItem
            { data_type = DataType
            , source_path = cs tform.path.value </> f
            , destination_path = sf
            , recursive = False
            }


initDownload :: (Globus :> es, Reader (Token Access) :> es) => TransferForm -> DownloadFolder -> [Dataset] -> Eff es (App.Id Task)
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
      , data_ = map (\d -> datasetTransferItem (destinationPath d) d) ds
      , sync_level = SyncChecksum
      }
   where
    destinationFolder :: FilePath
    destinationFolder =
      -- If they didn't select a folder, use the current folder
      case df.folder of
        Just f -> cs tform.path.value </> cs f
        Nothing -> cs tform.path.value

    destinationPath :: Dataset -> FilePath
    destinationPath d =
      destinationFolder </> cs d.instrumentProgramId.fromId </> cs d.datasetId.fromId


initTransferDataset :: (Globus :> es, Reader (Token Access) :> es, Reader (GlobusEndpoint App) :> es) => Dataset -> Eff es (App.Id Task, FilePath)
initTransferDataset d = do
  endpoint <- ask
  t <- initTransfer (transfer endpoint)

  -- we have to add the folder, because it's auto-added, I think
  let destFolder = datasetScratchPath endpoint
  pure (t, destFolder)
 where
  transfer :: GlobusEndpoint App -> Globus.Id Submission -> TransferRequest
  transfer endpoint submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just $ "Dataset " <> d.datasetId.fromId
      , source_endpoint = dkistEndpoint
      , destination_endpoint = endpoint.collection
      , data_ = [datasetTransferItem (datasetScratchPath endpoint) d]
      , sync_level = SyncChecksum
      }

  datasetScratchPath :: GlobusEndpoint App -> FilePath
  datasetScratchPath endpoint =
    endpoint.path </> cs d.primaryProposalId.fromId </> cs d.datasetId.fromId


dkistEndpoint :: Globus.Id Collection
dkistEndpoint = Tagged "d26bd00b-62dc-40d2-9179-7aece2b8c437"


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


runWithAccess :: Token Access -> Eff (Reader (Token Access) : es) a -> Eff es a
runWithAccess = runReader


requireLogin :: (Hyperbole :> es, Auth :> es) => Eff (Reader (Token Access) : es) a -> Eff es a
requireLogin eff = do
  acc <- getAccessToken >>= expectAuth
  runWithAccess acc eff


newtype RedirectPath = RedirectPath [Segment]
  deriving (Show, Eq)


instance Route RedirectPath where
  defRoute = RedirectPath []
  routePath (RedirectPath ss) = ss
  matchRoute ss = Just $ RedirectPath ss


data GlobusEndpoint a = GlobusEndpoint
  { collection :: Globus.Id Collection
  , path :: FilePath
  , mount :: FilePath
  }


inversionScratchFile :: GlobusEndpoint App -> App.Id Inversion -> Path' Filename a -> Path' Scratch a
inversionScratchFile endpoint ii (Path f) =
  Path $ endpoint.path </> cs ii.fromId </> f


inversionUploadedFiles :: forall es. (Reader (GlobusEndpoint App) :> es) => App.Id Inversion -> Eff es (UploadFiles Abs)
inversionUploadedFiles ii = do
  invResults <- absPath fileInvResults
  invProfile <- absPath fileInvProfile
  origProfile <- absPath fileOrigProfile
  timestamps <- absPath fileTimestamps
  pure $ UploadFiles{invResults, invProfile, origProfile, timestamps}
 where
  absPath :: Path' Filename a -> Eff es (Path' Abs a)
  absPath pf = do
    g <- ask @(GlobusEndpoint App)
    let Path s = inversionScratchFile g ii pf
    pure $ Path $ g.mount </> s


data InvProfile
data InvResults
data OrigProfile
data Timestamps


data Scratch


fileInvResults :: Path' Filename InvResults
fileInvResults = Path "inv_res_mod.fits"


fileInvProfile :: Path' Filename InvProfile
fileInvProfile = Path "inv_res_pre.fits"


fileOrigProfile :: Path' Filename OrigProfile
fileOrigProfile = Path "per_ori.fits"


fileTimestamps :: Path' Filename Timestamps
fileTimestamps = Path "timestamps.tsv"


genericFile :: Path' Filename a -> Path' Filename ()
genericFile (Path f) = Path f

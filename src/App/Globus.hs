{-# LANGUAGE OverloadedLists #-}

module App.Globus
  ( GlobusClient (..)
  , Globus
  , authUrl
  , accessTokens
  , UserLoginInfo (..)
  , userInfo
  , fileManagerSelectUrl
  , fileManagerOpenDir
  , fileManagerOpenInv
  , transferStatus
  , Uri
  , Token
  , Task (..)
  , TaskStatus (..)
  , Tagged (..)
  , Token' (..)
  , Id' (..)
  , runGlobus
  , taskPercentComplete
  , initDownloadL1Inputs
  -- , initDownloadL2Gen
  , initUpload
  , initScratchDataset
  , FileLimit (..)
  , TransferForm
  , UploadFiles (..)
  , DownloadFolder
  -- , Timestamps
  , UserEmail (..)
  , dkistEndpoint
  , scratchCollection
  , initTransfer
  , isTransferComplete
  , GlobusError (..)
  , catchHttpGlobus
  ) where

import App.Effect.Scratch (Scratch)
import App.Effect.Scratch qualified as Scratch
import App.Types (AppDomain)
import Data.ByteString (ByteString)
import Data.List qualified as L
import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception (Exception, catch)
import Effectful.Globus
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import GHC.Generics
import NSO.Data.Inversions as Inversions
import NSO.Image.Frame (L2Frame)
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram (Proposal)
import Network.Globus.Auth (TokenItem, UserEmail (..), UserInfo, UserInfoResponse (..), UserProfile, scopeToken)
import Network.Globus.Transfer (taskPercentComplete)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req qualified as Req
import Network.HTTP.Types (QueryItem, Status, unauthorized401)
import Web.FormUrlEncoded qualified as FUE
import Web.Hyperbole
import Web.Hyperbole.View.Forms (formLookup)
import Web.View as WebView


authUrl :: (Globus :> es) => Uri Redirect -> Eff es WebView.Url
authUrl red = do
  uri <- send $ AuthUrl red [TransferAll, Identity Globus.Email, Identity Globus.Profile, Identity Globus.OpenId] (State "_")
  pure $ convertUrl uri
 where
  convertUrl :: Uri a -> WebView.Url
  convertUrl u =
    let Query ps = u.params
     in Url{scheme = scheme u.scheme, domain = u.domain, path = u.path, query = map queryBs ps}
  scheme Https = "https://"
  scheme Http = "http://"
  queryBs :: (Text, Maybe Text) -> QueryItem
  queryBs (t, mt) = (cs t, cs <$> mt)


accessTokens :: (Globus :> es) => Uri Redirect -> Token Exchange -> Eff es (NonEmpty TokenItem)
accessTokens red tok = do
  send $ GetAccessTokens tok red


data UserLoginInfo = UserLoginInfo
  { info :: UserInfo
  , email :: Maybe UserEmail
  , profile :: Maybe UserProfile
  , transfer :: Token Access
  }
  deriving (Show)


userInfo :: (Globus :> es, Log :> es, Error GlobusError :> es) => NonEmpty TokenItem -> Eff es UserLoginInfo
userInfo tis = catchHttpGlobus $ do
  oid <- requireScopeToken (Identity OpenId)
  Tagged trn <- requireScopeToken TransferAll

  ur <- send $ GetUserInfo oid
  pure $
    UserLoginInfo
      { info = ur.info
      , email = ur.email
      , profile = ur.profile
      , transfer = Tagged trn
      }
 where
  requireScopeToken s = do
    Tagged t <- maybe (throwError $ MissingScope s tis) pure $ scopeToken s tis
    pure $ Tagged t


-- accessToken :: Globus -> Uri Redirect -> Token Exchange -> m TokenResponse
-- accessToken (Token cid) (Token sec) red (Token code) =

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
fileManagerOpenInv :: Path' Dir L2Frame -> Url
fileManagerOpenInv = fileManagerOpenDir (App.Id "20fa4840-366a-494c-b009-063280ecf70d")


fileManagerOpenDir :: App.Id Collection -> Path' Dir a -> Url
fileManagerOpenDir origin dir =
  Url
    "https://"
    "app.globus.org"
    ["file-manager"]
    [ ("origin_id", Just $ cs origin.fromId)
    , ("origin_path", Just $ "/" <> cs dir.filePath)
    ]


data TransferForm = TransferForm
  { label :: Text
  , -- , endpoint :: Field Text
    path :: Path' Dir TransferForm
  , endpoint_id :: Text
  }
  deriving (Generic, FromForm)
instance Show TransferForm where
  show tf = "TransferForm " <> unwords [show tf.label, show tf.path, show tf.endpoint_id]


data DownloadFolder = DownloadFolder
  { folder :: Maybe (Path' Dir DownloadFolder)
  }
  deriving (Generic)
instance FromForm DownloadFolder where
  fromForm f = do
    DownloadFolder <$> formLookup "folder[0]" f


data UploadFiles t f = UploadFiles
  { quantities :: Field f (Path' t InvQuantities)
  , profileFit :: Field f (Path' t InvProfileFit)
  , profileOrig :: Field f (Path' t InvProfileOrig)
  -- , timestamps :: Path' t Timestamps
  }
  deriving (Generic)
instance Show (UploadFiles Filename Maybe) where
  show (UploadFiles q pf po) = "UploadFiles " <> show q <> " " <> show pf <> " " <> show po
instance FromForm (UploadFiles Filename Maybe) where
  fromForm f = do
    fs <- files f
    let quantities = findFile Scratch.fileQuantities fs
    let profileFit = findFile Scratch.fileProfileFit fs
    let profileOrig = findFile Scratch.fileProfileOrig fs
    pure UploadFiles{quantities, profileFit, profileOrig}
   where
    files :: FUE.Form -> Either Text [Path' Filename ()]
    files frm = do
      f0 <- formLookup "file[0]" frm
      f1 <- formLookup "file[1]" frm
      f2 <- formLookup "file[2]" frm
      f3 <- formLookup "file[3]" frm
      pure $ catMaybes [f0, f1, f2, f3]

    findFile :: Path' Filename a -> [Path' Filename ()] -> Maybe (Path' Filename a)
    findFile file fs = do
      Path ff <- L.find (isFile file) fs
      pure $ Path ff

    isFile :: Path' Filename a -> Path' Filename () -> Bool
    isFile (Path fa) (Path fb) = fa == fb


transferStatus :: (Log :> es, Reader (Token Access) :> es, Globus :> es, Error GlobusError :> es) => App.Id Task -> Eff es Task
transferStatus (Id ti) = do
  acc <- ask
  catchHttpGlobus $ send $ StatusTask acc (Tagged ti)


initTransfer :: (Globus :> es, Log :> es, Error GlobusError :> es, Reader (Token Access) :> es) => (Globus.Id Submission -> TransferRequest) -> Eff es (App.Id Task)
initTransfer toRequest = catchHttpGlobus $ do
  acc <- ask
  sub <- send $ Globus.SubmissionId acc
  res <- send $ Globus.Transfer acc $ toRequest sub
  pure $ Id res.task_id.unTagged


initUpload
  :: (Hyperbole :> es, Globus :> es, Log :> es, Error GlobusError :> es, Scratch :> es, Reader (Token Access) :> es)
  => TransferForm
  -> UploadFiles Filename Maybe
  -> App.Id Proposal
  -> App.Id Inversion
  -> Eff es (App.Id Task)
initUpload tform up ip ii = do
  scratch <- scratchCollection
  initTransfer (transferRequest scratch)
 where
  transferRequest :: Globus.Id Collection -> Globus.Id Submission -> TransferRequest
  transferRequest scratch submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just tform.label
      , source_endpoint = Tagged tform.endpoint_id
      , destination_endpoint = scratch
      , data_ = catMaybes [transferItem <$> up.quantities, transferItem <$> up.profileFit, transferItem <$> up.profileOrig]
      , sync_level = SyncChecksum
      , store_base_path_info = True
      }
   where
    transferItem :: Path' Filename a -> TransferItem
    transferItem f =
      TransferItem
        { data_type = DataType
        , source_path = (source tform.path f).filePath
        , destination_path = (dest f).filePath
        , recursive = False
        }

    dest :: Path' Filename a -> Path' File a
    dest fn = Scratch.blanca ip ii </> fn

    source :: Path' Dir TransferForm -> Path' Filename a -> Path' File a
    source t fn = t </> fn


initDownloadL1Inputs :: (Globus :> es, Log :> es, Error GlobusError :> es, Reader (Token Access) :> es) => TransferForm -> DownloadFolder -> [Dataset] -> Eff es (App.Id Task)
initDownloadL1Inputs tform df ds = do
  initTransfer downloadTransferRequest
 where
  downloadTransferRequest :: Globus.Id Submission -> TransferRequest
  downloadTransferRequest submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just tform.label
      , source_endpoint = dkistEndpoint
      , destination_endpoint = Tagged tform.endpoint_id
      , data_ = map (\d -> datasetTransferItem (destinationPath d) d) ds
      , sync_level = SyncTimestamp
      , store_base_path_info = True
      }
   where
    destinationPath :: Dataset -> Path' Dir Dataset
    destinationPath d =
      downloadDestinationFolder tform df </> Path (cs d.instrumentProgramId.fromId) </> Path (cs d.datasetId.fromId)


initScratchDataset :: (IOE :> es, Log :> es, Globus :> es, Error GlobusError :> es, Reader (Token Access) :> es, Scratch :> es) => Dataset -> Eff es (App.Id Task)
initScratchDataset d = do
  scratch <- scratchCollection
  initTransfer (transfer scratch)
 where
  transfer :: Globus.Id Collection -> Globus.Id Submission -> TransferRequest
  transfer scratch submission_id =
    TransferRequest
      { data_type = DataType
      , submission_id
      , label = Just $ "Dataset " <> d.datasetId.fromId
      , source_endpoint = dkistEndpoint
      , destination_endpoint = scratch
      , data_ = [datasetTransferItem (Scratch.dataset d) d]
      , sync_level = SyncTimestamp
      , store_base_path_info = True
      }


-- really, I want to just open the file manager at that location, let thme handle it.
-- initDownloadL2Gen :: (Globus :> es, Reader (Token Access) :> es) => TransferForm -> DownloadFolder -> Inversion -> Eff es (App.Id Task)
-- initDownloadL2Gen tform df inv = do
--   initTransfer downloadTransferRequest
--  where
--   downloadTransferRequest :: Globus.Id Submission -> TransferRequest
--   downloadTransferRequest submission_id =
--     TransferRequest
--       { data_type = DataType
--       , submission_id
--       , label = Just tform.label.value
--       , source_endpoint = dkistEndpoint
--       , destination_endpoint = Tagged tform.endpoint_id.value
--       , data_ = [transferItem $ Scratch.outputL2Dir inv.proposalId inv.inversionId]
--       , sync_level = SyncTimestamp
--       , store_base_path_info = True
--       }
--    where
--     transferItem :: Path' Dir L2Frame -> TransferItem
--     transferItem dir =
--       TransferItem
--         { data_type = DataType
--         , source_path = dir.filePath
--         , destination_path = (downloadDestinationFolder tform df).filePath
--         , recursive = False
--         }

downloadDestinationFolder :: TransferForm -> DownloadFolder -> Path' Dir TransferForm
downloadDestinationFolder tform df =
  -- If they didn't select a folder, use the current folder
  case df.folder of
    Just f -> tform.path </> f
    Nothing -> tform.path


dkistEndpoint :: Globus.Id Collection
dkistEndpoint = Tagged "d26bd00b-62dc-40d2-9179-7aece2b8c437"


datasetTransferItem :: Path' Dir Dataset -> Dataset -> TransferItem
datasetTransferItem dest d =
  TransferItem
    { data_type = DataType
    , source_path = datasetSourcePath.filePath
    , destination_path = dest.filePath
    , recursive = True
    }
 where
  datasetSourcePath :: Path' Dir Dataset
  datasetSourcePath = Path "data" </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)


scratchCollection :: (Scratch :> es) => Eff es (Globus.Id Collection)
scratchCollection = do
  Id c <- send Scratch.Globus
  pure $ Tagged c


data GlobusError
  = MissingScope Scope (NonEmpty TokenItem)
  | TransferFailed (App.Id Task)
  | Unauthorized HTTP.Request
  | StatusError HTTP.Request Status ByteString
  | ReqError Req.HttpException
  deriving (Exception, Show)


isTransferComplete :: (Log :> es, Globus :> es, Reader (Token Access) :> es, Error GlobusError :> es) => App.Id Task -> Eff es Bool
isTransferComplete it = do
  task <- transferStatus it
  case task.status of
    Succeeded -> pure True
    Failed -> throwError $ TransferFailed it
    _ -> do
      log Debug $ dump "Transfer" $ taskPercentComplete task
      pure False


catchHttpGlobus :: (Log :> es, Error GlobusError :> es) => Eff es a -> Eff es a
catchHttpGlobus eff =
  catch eff onGlobusErr
 where
  onGlobusErr = \case
    Req.VanillaHttpException (HttpExceptionRequest req (StatusCodeException res body)) -> do
      log Err $ dump "GLOBUS StatusCodeException" (req, res)
      onStatusErr req (responseStatus res) body
    ex -> do
      log Err $ dump "GLOBUS HttpException" ex
      throwError $ ReqError ex

  onStatusErr req (status :: Status) body
    | status == unauthorized401 = throwError $ Unauthorized req
    | otherwise = throwError $ StatusError req status body

-- pure $ Hyperbole.Err $ Hyperbole.ErrOther $ "Globus request to " <> cs (path req) <> " failed with:\n " <> cs (show status)
-- onGlobusErr ex = do
--   log Err $ dump "GLOBUS" ex
--   pure $ Hyperbole.Err $ Hyperbole.ErrOther "Globus Error"

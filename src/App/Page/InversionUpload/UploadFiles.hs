module App.Page.InversionUpload.UploadFiles where

import App.Effect.Transfer (TransferForm (..), initTransfer)
import Data.List qualified as L
import Data.Tagged
import Effectful
import Effectful.Error.Static
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Reader.Dynamic
import GHC.Generics
import NSO.Data.Inversions as Inversions
import NSO.Data.Scratch (Scratch)
import NSO.Data.Scratch qualified as Scratch
import NSO.Image.Files qualified as Files
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.InstrumentProgram (Proposal)
import Web.FormUrlEncoded qualified as FUE
import Web.Hyperbole
import Web.Hyperbole.Data.Param (ParamValue (..))


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
    let quantities = findFile Files.fileQuantities fs
    let profileFit = findFile Files.fileProfileFit fs
    let profileOrig = findFile Files.fileProfileOrig fs
    pure UploadFiles{quantities, profileFit, profileOrig}
   where
    files :: FUE.Form -> Either Text [Path' Filename ()]
    files frm = do
      f0 <- FUE.parseMaybe "file[0]" frm
      f1 <- FUE.parseMaybe "file[1]" frm
      f2 <- FUE.parseMaybe "file[2]" frm
      f3 <- FUE.parseMaybe "file[3]" frm
      pure $ catMaybes [f0, f1, f2, f3]

    findFile :: Path' Filename a -> [Path' Filename ()] -> Maybe (Path' Filename a)
    findFile file fs = do
      Path ff <- L.find (isFile file) fs
      pure $ Path ff

    isFile :: Path' Filename a -> Path' Filename () -> Bool
    isFile (Path fa) (Path fb) = fa == fb


initUpload
  :: (Hyperbole :> es, Globus :> es, Log :> es, Error GlobusError :> es, Scratch :> es, Reader (Token Access) :> es)
  => TransferForm
  -> UploadFiles Filename Maybe
  -> App.Id Proposal
  -> App.Id Inversion
  -> Eff es (App.Id Task)
initUpload tform up ip ii = do
  scratch <- Scratch.collection
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
    dest fn = Files.blanca ip ii </> fn

    source :: Path' Dir TransferForm -> Path' Filename a -> Path' File a
    source t fn = t </> fn


-----------------------------------------------------------------
--- UPLOAD STATUS
-----------------------------------------------------------------

data UploadStatus a
  = NoUpload
  | Uploaded
  | Uploading (Id Task)
  deriving (Show, Eq)
instance Semigroup (UploadStatus a) where
  Uploaded <> _ = Uploaded
  _ <> Uploaded = Uploaded
  Uploading taskId <> _ = Uploading taskId
  _ <> Uploading taskId = Uploading taskId
  _ <> _ = NoUpload
instance Monoid (UploadStatus a) where
  mempty = NoUpload
instance Default (UploadStatus a) where
  def = NoUpload
instance ToParam (UploadStatus a) where
  toParam NoUpload = ""
  toParam (Uploading tid) = ParamValue tid.fromId
  toParam Uploaded = "Uploaded"
instance FromParam (UploadStatus a) where
  parseParam "Uploaded" = pure Uploaded
  parseParam "" = pure NoUpload
  parseParam (ParamValue t) = pure $ Uploading $ Id t
type instance Field UploadStatus a = UploadStatus a


instance Monoid (UploadFiles c UploadStatus) where
  mempty = UploadFiles def def def
instance Semigroup (UploadFiles c UploadStatus) where
  up1 <> up2 =
    UploadFiles
      { quantities = up1.quantities <> up2.quantities
      , profileFit = up1.profileFit <> up2.profileFit
      , profileOrig = up1.profileOrig <> up2.profileOrig
      }
instance ToQuery (UploadFiles c UploadStatus)
instance FromQuery (UploadFiles c UploadStatus)


allUploadStatus :: UploadFiles Filename UploadStatus -> UploadStatus ()
allUploadStatus files =
  case anyUploadTask files of
    Just taskId -> Uploading taskId
    Nothing ->
      if allUploaded
        then Uploaded
        else NoUpload
 where
  allUploaded =
    uploaded files.quantities && uploaded files.profileFit && uploaded files.profileOrig

  anyUploadTask up =
    uploadTask up.quantities <|> uploadTask up.profileFit <|> uploadTask up.profileOrig

  uploaded = \case
    Uploaded -> True
    _ -> False

  uploadTask (Uploading taskId) = Just taskId
  uploadTask _ = Nothing

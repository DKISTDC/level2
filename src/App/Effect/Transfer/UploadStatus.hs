module App.Effect.Transfer.UploadStatus where

import NSO.Files
import NSO.Prelude
import NSO.Types.Common as App
import Network.Globus (Task)
import Web.Hyperbole
import Web.Hyperbole.Data.Param (ParamValue (..))


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


clearUploading :: UploadStatus a -> UploadStatus a
clearUploading up =
  case up of
    Uploading _ -> NoUpload
    other -> other


instance Monoid (InversionFiles UploadStatus c) where
  mempty = InversionFiles def def def
instance Semigroup (InversionFiles UploadStatus c) where
  up1 <> up2 =
    InversionFiles
      { quantities = up1.quantities <> up2.quantities
      , profileFit = up1.profileFit <> up2.profileFit
      , profileOrig = up1.profileOrig <> up2.profileOrig
      }
instance ToQuery (InversionFiles UploadStatus c)
instance FromQuery (InversionFiles UploadStatus c)


allClearUploading :: InversionFiles UploadStatus f -> InversionFiles UploadStatus f
allClearUploading inv =
  InversionFiles
    { quantities = clearUploading inv.quantities
    , profileFit = clearUploading inv.profileFit
    , profileOrig = clearUploading inv.profileOrig
    }


allUploadStatus :: InversionFiles UploadStatus Filename -> UploadStatus ()
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

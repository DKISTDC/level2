{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.InversionUpload where

import App.Colors
import App.Effect.Auth (Auth, openFileManager, requireLogin)
import App.Effect.Scratch (Scratch)
import App.Globus as Globus (FileLimit (Files), Globus, Task, TransferForm, UploadFiles (..), initUpload)
import App.Route qualified as Route
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons qualified as Icons
import App.View.Inversion qualified as Inversion
import App.View.Layout
import App.View.Transfer (TransferAction (..))
import App.View.Transfer qualified as Transfer
import Data.Default (Default (..))
import Debug.Trace
import Effectful
import Effectful.Log (Log)
import Effectful.Log hiding (Info)
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs hiding (programInversions)
import NSO.Prelude
import NSO.Types.InstrumentProgram (Proposal)
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData (ParamValue (..), fromQueryData)
import Web.Hyperbole.Effect.Request (formBody)


page
  :: (Hyperbole :> es, Datasets :> es, Inversions :> es, Auth :> es, Globus :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Id Inversion
  -> Eff es (Page '[InversionUpload])
page propId progId invId = do
  uploads <- query
  appLayout Route.Inversions $ do
    col Style.page $ do
      col (gap 5) $ do
        el Style.header $ do
          text "Inversion Upload - "
          text invId.fromId

        el_ $ do
          text "Program - "
          route (Route.program propId progId) Style.link $ do
            text progId.fromId

        el_ $ do
          text "Proposal - "
          route (Route.proposal propId) Style.link $ do
            text propId.fromId

      hyper (InversionUpload propId progId invId) $ viewInversionUpload uploads


-----------------------------------------------------------------
--- Uploads
-----------------------------------------------------------------

submitUpload
  :: forall es
   . (Hyperbole :> es, Log :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es, Scratch :> es, Log :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Id Inversion
  -> Eff es Response
submitUpload propId progId invId = do
  log Debug "submitUpload"
  tfrm <- formData @TransferForm
  log Debug $ dump "Transfer" tfrm
  tup <- formData @(UploadFiles Filename Maybe)
  log Debug $ dump "Form" tup

  taskId <- requireLogin $ Globus.initUpload tfrm tup propId invId

  files <- query
  let new = uploads taskId tup
  let files' = files <> new
  traceM $ show ("QUERY", files.quantities, files.profileFit, files.profileOrig)
  traceM $ show ("NEW", new.quantities, new.profileFit, new.profileOrig)
  traceM $ show ("FILES'", files'.quantities, files'.profileFit, files'.profileOrig)

  redirect $ setUploadQuery (files <> uploads taskId tup) $ routeUrl $ Route.inversionUpload propId progId invId
 where
  uploads :: Id Task -> UploadFiles Filename Maybe -> UploadFiles Filename UploadStatus
  uploads taskId up =
    UploadFiles
      { quantities = uploadStatus up.quantities
      , profileFit = uploadStatus up.profileFit
      , profileOrig = uploadStatus up.profileOrig
      }
   where
    uploadStatus :: Maybe a -> UploadStatus a
    uploadStatus = \case
      Just _ -> Uploading taskId
      Nothing -> NoUpload


setUploadQuery :: UploadFiles Filename UploadStatus -> Url -> Url
setUploadQuery files Url{scheme, domain, path} =
  let q = fromQueryData $ toQuery files
   in Url{query = q, scheme, domain, path}


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
instance ToQuery (UploadFiles Filename UploadStatus)
instance FromQuery (UploadFiles Filename UploadStatus)
instance Semigroup (UploadFiles Filename UploadStatus) where
  up1 <> up2 =
    UploadFiles
      { quantities = up1.quantities <> up2.quantities
      , profileFit = up1.profileFit <> up2.profileFit
      , profileOrig = up1.profileOrig <> up2.profileOrig
      }


-----------------------------------------------------------------
--- InversionUpload
-----------------------------------------------------------------

data InversionUpload = InversionUpload (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Show, Read, ViewId)


instance (Auth :> es, Log :> es, Globus :> es) => HyperView InversionUpload es where
  data Action InversionUpload
    = Cancel
    | Upload
    | UpTransfer (Id Task) TransferAction
    deriving (Show, Read, ViewAction)


  update action = do
    InversionUpload propId progId invId <- viewId
    case action of
      Cancel -> do
        redirect $ routeUrl $ Route.program propId progId
      Upload -> do
        files <- query
        let u = setUploadQuery files $ routeUrl $ Route.submitUpload propId progId invId
        openFileManager (Files 3) u ("Transfer Inversion Results " <> invId.fromId)
      UpTransfer taskId trans -> do
        files <- query
        case trans of
          TaskFailed -> do
            pure $ viewInversionUpload' files $ do
              Transfer.viewTransferFailed taskId
          TaskSucceeded -> do
            let files' = filesUploaded files
            setQuery files'
            pure $ viewInversionUpload files'
          CheckTransfer -> do
            vw <- Transfer.checkTransfer (UpTransfer taskId) taskId
            pure $ viewInversionUpload' files vw
   where
    filesUploaded :: UploadFiles Filename UploadStatus -> UploadFiles Filename UploadStatus
    filesUploaded up =
      UploadFiles
        { quantities = fileUploaded up.quantities
        , profileFit = fileUploaded up.profileFit
        , profileOrig = fileUploaded up.profileOrig
        }
    fileUploaded (Uploading _) = Uploaded
    fileUploaded s = s


viewInversionUpload :: UploadFiles Filename UploadStatus -> View InversionUpload ()
viewInversionUpload files = do
  viewInversionUpload' files $ do
    case anyUploadTask files of
      Just taskId -> do
        Transfer.viewLoadTransfer (UpTransfer taskId)
      Nothing ->
        if allUploaded
          then View.iconButton Upload (Style.btnOutline Success) Icons.upTray "Select New Files"
          else View.iconButton Upload (Style.btn Primary) Icons.upTray "Select Files"
 where
  allUploaded =
    isUploaded files.quantities && isUploaded files.profileFit && isUploaded files.profileOrig

  isUploaded = \case
    Uploaded -> True
    _ -> False

  uploadTask (Uploading taskId) = Just taskId
  uploadTask _ = Nothing

  anyUploadTask up =
    uploadTask up.quantities <|> uploadTask up.profileFit <|> uploadTask up.profileOrig


viewInversionUpload' :: UploadFiles Filename UploadStatus -> View InversionUpload () -> View InversionUpload ()
viewInversionUpload' files xfer = do
  col (gap 25) $ do
    Inversion.viewInversionContainer' Info $ do
      col (gap 10) $ do
        el bold "Inversion Results"
        col (gap 5) $ do
          uploadedFile files.quantities "inv_res_pre.fits"
          uploadedFile files.profileFit "inv_res_mod.fits"
          uploadedFile files.profileOrig "per_ori.fits"
        xfer
    View.iconButton Cancel (Style.btnOutline Secondary) Icons.xMark "Cancel"
 where
  uploadedFile file lbl =
    row (gap 5 . uploadColor file) $ do
      el (width 15 . height 15 . Style.alignMiddle) $ do
        uploadIcon file
      text lbl

  uploadColor = \case
    Uploaded -> color Success
    Uploading _ -> color Primary
    _ -> color Black

  uploadIcon = \case
    Uploaded -> el_ Icons.check
    Uploading _ -> Icons.arrowPath
    _ -> none

-- complete =
--   if allUploaded
--     then color Success
--     else color Black

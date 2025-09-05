{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.InversionUpload where

import App.Colors
import App.Effect.Auth (Auth, openFileManager)
import App.Effect.FileManager (FileLimit (Files))
import App.Effect.Transfer (Transfer)
import App.Effect.Transfer qualified as Transfer
import App.Effect.Transfer.UploadStatus
import App.Page.Inversions.CommitForm (commitForm)
import App.Page.Inversions.CommitForm qualified as CommitForm
import App.Route as Route
import App.Style qualified as Style
import App.Types (App)
import App.View.Common qualified as View
import App.View.Icons qualified as Icons
import App.View.Inversion qualified as Inversion
import App.View.Layout
import App.View.Transfer (TransferAction (..))
import App.View.Transfer qualified as Transfer
import Effectful.Globus (Task)
import Effectful.Log hiding (Info)
import Effectful.Reader.Dynamic (Reader)
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs hiding (programInversions)
import NSO.Files
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (Proposal)
import Web.Atomic.CSS
import Web.Hyperbole hiding (meta)
import Web.Hyperbole.Data.QueryData (fromQueryData)
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Data.URI (queryString)


page
  :: (Hyperbole :> es, Datasets :> es, Inversions :> es, Auth :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Id Inversion
  -> Page es '[Uploads, Manage, MetadataForm]
page propId progId invId = do
  qs <- query @QueryState
  dall <- Datasets.find (Datasets.ByProgram progId)

  appLayout Route.Inversions $ do
    col ~ Style.page $ do
      col ~ gap 5 $ do
        el ~ Style.header $ do
          text "Inversion Upload - "
          text invId.fromId

        el $ do
          text "Program - "
          appRoute (Route.program propId progId) ~ Style.link $ do
            text progId.fromId

        el $ do
          text "Proposal - "
          appRoute (Route.proposal propId) ~ Style.link $ do
            text propId.fromId

      col ~ gap 25 $ do
        Inversion.viewInversionContainer' Info $ do
          col ~ gap 15 $ do
            hyper (Uploads propId progId invId) $ viewUpload dall qs.uploads qs.metadata

        hyper (Manage propId progId) $ do
          View.iconButton Cancel Icons.xMark "Cancel" ~ Style.btnOutline Secondary


-----------------------------------------------------------------
--- Uploads
-----------------------------------------------------------------

submitUpload
  :: forall es
   . (Log :> es, Hyperbole :> es, Datasets :> es, Inversions :> es, Scratch :> es, Transfer :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Id Inversion
  -> Eff es Response
submitUpload propId progId invId = do
  log Debug "submitUpload"
  tfrm <- formData @TransferForm
  log Debug $ dump "Transfer" tfrm
  tup <- formData @(InversionFiles Maybe Filename)
  log Debug $ dump "Form" tup

  taskId <- Transfer.uploadInversionResults tfrm tup propId invId
  log Debug $ dump "Upload" taskId
  let new = uploads taskId tup
  files <- query
  redirect $ setUploadQuery (allClearUploading files <> new) $ routeUri $ Route.inversionUpload propId progId invId
 where
  uploads :: Id Task -> InversionFiles Maybe Filename -> InversionFiles UploadStatus Filename
  uploads taskId up =
    InversionFiles
      { quantities = uploadStatus up.quantities
      , profileFit = uploadStatus up.profileFit
      , profileOrig = uploadStatus up.profileOrig
      }
   where
    uploadStatus :: Maybe a -> UploadStatus a
    uploadStatus = \case
      Just _ -> Uploading taskId
      Nothing -> NoUpload


setUploadQuery :: InversionFiles UploadStatus Filename -> URI -> URI
setUploadQuery files URI{uriAuthority, uriScheme, uriPath} =
  let q = fromQueryData $ toQuery files
   in URI{uriQuery = queryString q, uriAuthority, uriScheme, uriPath, uriFragment = ""}


data QueryState = QueryState
  { uploads :: InversionFiles UploadStatus Filename
  , metadata :: Metadata Identity
  }
instance ToQuery QueryState where
  toQuery qs = toQuery qs.uploads <> toQuery qs.metadata
instance FromQuery QueryState where
  parseQuery qd = do
    up <- parseUploaded
    md <- parseQuery qd
    pure $ QueryState up md
   where
    parseUploaded =
      case QueryData.lookup @Text "uploaded" qd of
        Just _ -> pure $ InversionFiles Uploaded Uploaded Uploaded
        Nothing -> parseQuery qd


setDatasetId :: Id Dataset -> Bool -> QueryState -> QueryState
setDatasetId dsetId set QueryState{metadata, uploads} =
  let Metadata{..} = metadata
   in QueryState
        { uploads
        , metadata =
            Metadata
              { datasets = modDatasetId set metadata.datasets
              , ..
              }
        }
 where
  modDatasetId True =
    (dsetId :)
  modDatasetId False =
    filter (/= dsetId)


-----------------------------------------------------------------
--- Uploads
-----------------------------------------------------------------

data Uploads = Uploads (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Generic, ViewId)


instance (Log :> es, Inversions :> es, Datasets :> es, Reader App :> es, Transfer :> es) => HyperView Uploads es where
  data Action Uploads
    = Upload
    | UpTransfer (Id Task) TransferAction
    deriving (Generic, ViewAction)


  type Require Uploads = '[MetadataForm]


  update action = do
    Uploads propId progId invId <- viewId
    QueryState uploads metadata <- query @QueryState
    case action of
      Upload -> do
        let u = setUploadQuery uploads $ routeUri $ Route.submitUpload propId progId invId
        openFileManager (Files 3) ("Transfer Inversion Results " <> invId.fromId) u
      UpTransfer taskId trans -> do
        dall <- Datasets.find (Datasets.ByProgram progId)
        case trans of
          TaskFailed -> do
            pure $ viewUploadWithTransfer dall uploads metadata $ do
              Transfer.viewTransferFailed taskId
              View.iconButton Upload Icons.upTray "Select New Files" ~ Style.btn Primary
          TaskSucceeded -> do
            let uploads' = filesUploaded uploads
            setQuery $ QueryState uploads' def
            pure $ viewUpload dall uploads' metadata
          CheckTransfer -> do
            tview <- Transfer.checkTransfer (UpTransfer taskId) taskId
            pure $ viewUploadWithTransfer dall uploads metadata $ do
              tview
   where
    filesUploaded :: InversionFiles UploadStatus Filename -> InversionFiles UploadStatus Filename
    filesUploaded up =
      InversionFiles
        { quantities = fileUploaded up.quantities
        , profileFit = fileUploaded up.profileFit
        , profileOrig = fileUploaded up.profileOrig
        }
    fileUploaded (Uploading _) = Uploaded
    fileUploaded s = s


viewUpload :: [Dataset] -> InversionFiles UploadStatus Filename -> Metadata Identity -> View Uploads ()
viewUpload dall files meta = do
  viewUploadWithTransfer dall files meta $ do
    case allUploadStatus files of
      Uploading taskId -> do
        Transfer.viewLoadTransfer (UpTransfer taskId)
      NoUpload ->
        View.iconButton Upload Icons.upTray "Select Files" ~ Style.btn Primary
      Uploaded ->
        View.iconButton Upload Icons.upTray "Select New Files" ~ Style.btnOutline Success


viewUploadWithTransfer :: [Dataset] -> InversionFiles UploadStatus Filename -> Metadata Identity -> View Uploads () -> View Uploads ()
viewUploadWithTransfer dall uploads meta xfer = do
  Uploads propId progId invId <- viewId
  el ~ Style.subheader . headerColor $ "Upload Inversion Results"
  col ~ pad 10 . gap 10 $ do
    col ~ gap 5 $ do
      uploadedFile uploads.quantities "inv_res_pre.fits"
      uploadedFile uploads.profileFit "inv_res_mod.fits"
      uploadedFile uploads.profileOrig "per_ori.fits"
    xfer

  hyper (MetadataForm propId progId invId) $ viewMetadataForm dall uploads meta
 where
  -- stepMetadata StepNext none
  -- stepGenerate StepNext none
  -- stepPublish StepNext none

  uploadedFile :: UploadStatus a -> Text -> View Uploads ()
  uploadedFile file lbl =
    row ~ gap 5 . uploadColor file $ do
      el ~ width 15 . height 15 . Style.alignMiddle $ do
        uploadIcon file
      text lbl

  uploadColor = \case
    Uploaded -> color Success
    Uploading _ -> color Primary
    _ -> color Black

  uploadIcon = \case
    Uploaded -> el Icons.check
    Uploading _ -> Icons.arrowPath
    _ -> none

  headerColor =
    case allUploadStatus uploads of
      Uploaded -> color Success
      _ -> color Info


-----------------------------------------------------------------
-- METADATA
-----------------------------------------------------------------

data Metadata f = Metadata
  { datasets :: Field f [Id Dataset]
  , commit :: Field f (Maybe GitCommit)
  }
  deriving (Generic)
instance ToQuery (Metadata Identity)
instance FromQuery (Metadata Identity)
instance Default (Metadata Identity) where
  def = Metadata [] Nothing


data MetadataForm = MetadataForm (Id Proposal) (Id InstrumentProgram) (Id Inversion)
  deriving (Generic, ViewId)


instance (Log :> es, Inversions :> es, Datasets :> es) => HyperView MetadataForm es where
  data Action MetadataForm
    = SetDataset (Id Dataset) Bool
    | SaveCommit GitCommit
    | Submit
    deriving (Generic, ViewAction)


  update action = do
    MetadataForm propId progId invId <- viewId
    case action of
      SetDataset dsetId set -> do
        qs <- setDatasetId dsetId set <$> query @QueryState
        setQuery qs
        dall <- Datasets.find (Datasets.ByProgram progId)
        pure $ viewMetadataForm dall qs.uploads qs.metadata
      SaveCommit commit -> do
        qs <- query @QueryState
        isValid <- CommitForm.validate commit
        dall <- Datasets.find (Datasets.ByProgram progId)

        if isValid
          then saveCommit dall qs commit
          else pure $ viewMetadataForm' dall qs.uploads qs.metadata (Metadata NotInvalid CommitForm.invalidCommit)
      Submit -> do
        qs <- query @QueryState
        dall <- Datasets.find (Datasets.ByProgram progId)

        let val = validateMetadata qs.metadata

        unless (anyInvalid val) $ do
          _ <- send $ Inversions.Create propId progId invId qs.metadata.commit qs.metadata.datasets
          redirect $ routeUri $ Route.inversion propId invId

        pure $ viewMetadataForm' dall qs.uploads qs.metadata val
   where
    saveCommit dall qs commit = do
      setQuery $ qs{metadata = Metadata qs.metadata.datasets (Just commit)}
      pure $ viewMetadataForm' dall qs.uploads qs.metadata (Metadata NotInvalid Valid)

    anyInvalid meta =
      isInvalid meta.commit || isInvalid meta.datasets


-- Feedback on the datasets missing?

viewMetadataForm :: [Dataset] -> InversionFiles UploadStatus Filename -> Metadata Identity -> View MetadataForm ()
viewMetadataForm dall uploads meta = do
  viewMetadataForm' dall uploads meta (Metadata NotInvalid (CommitForm.existingCommit meta.commit))


viewMetadataForm' :: [Dataset] -> InversionFiles UploadStatus Filename -> Metadata Identity -> Metadata Validated -> View MetadataForm ()
viewMetadataForm' dall uploads meta valid = do
  case allUploadStatus uploads of
    Uploaded -> do
      el ~ Style.subheader . color Info $ "Metadata"
      col ~ gap 15 . pad 10 $ do
        viewDatasets dall meta.datasets valid.datasets
        commitForm SaveCommit meta.commit (convert valid.commit)
        button Submit ~ Style.btn Primary $ "Create Inversion"
    _ -> none
 where
  convert (Invalid t) = Invalid t
  convert Valid = Valid
  convert NotInvalid = NotInvalid


viewDatasets :: [Dataset] -> [Id Dataset] -> Validated [Id Dataset] -> View MetadataForm ()
viewDatasets dall dsel valid = do
  col ~ gap 5 $ do
    el ~ bold . validColor valid $ "Datasets Used"
    forM_ dall $ \d -> do
      row ~ gap 10 $ do
        View.checkBtn (SetDataset d.datasetId) (d.datasetId `elem` dsel)
        appRoute (Route.Datasets $ Route.Dataset d.datasetId) ~ Style.link $ text d.datasetId.fromId
    case valid of
      Invalid t -> el ~ color Danger $ text t
      _ -> none
 where
  validColor = \case
    Invalid _ -> color Danger
    Valid -> color Success
    _ -> id


validateMetadata :: Metadata Identity -> Metadata Validated
validateMetadata meta =
  Metadata
    { datasets = validateDatasets meta.datasets
    , commit = validateCommit meta.commit
    }
 where
  validateDatasets [] = Invalid "Missing datasets used during inversion"
  validateDatasets _ = Valid

  validateCommit :: Maybe GitCommit -> Validated (Maybe GitCommit)
  validateCommit Nothing = Invalid "Missing or Invalid Git Commit"
  validateCommit _ = Valid


-----------------------------------------------------------------
-- MANAGE
-----------------------------------------------------------------

data Manage = Manage (Id Proposal) (Id InstrumentProgram)
  deriving (Generic, ViewId)


instance HyperView Manage es where
  data Action Manage
    = Cancel
    deriving (Generic, ViewAction)


  update Cancel = do
    Manage propId progId <- viewId
    redirect $ routeUri $ Route.program propId progId

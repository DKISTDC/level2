{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.InversionUpload where

import App.Colors
import App.Effect.Auth (Auth, openFileManager, requireLogin)
import App.Effect.Scratch (Scratch)
import App.Globus as Globus (FileLimit (Files), Globus, Task, TransferForm, UploadFiles (..), initUpload)
import App.Page.Inversions.CommitForm (commitForm)
import App.Page.Inversions.CommitForm qualified as CommitForm
import App.Route qualified as Route
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons qualified as Icons
import App.View.Inversion qualified as Inversion
import App.View.Layout
import App.View.Transfer (TransferAction (..))
import App.View.Transfer qualified as Transfer
import Data.Default (Default (..))
import Effectful
import Effectful.Log hiding (Info)
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs hiding (programInversions)
import NSO.Prelude
import NSO.Types.InstrumentProgram (Proposal)
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData (ParamValue (..), fromQueryData)


page
  :: (Hyperbole :> es, Datasets :> es, Inversions :> es, Auth :> es, Globus :> es)
  => Id Proposal
  -> Id InstrumentProgram
  -> Id Inversion
  -> Eff es (Page '[Uploads, Manage, MetadataForm])
page propId progId invId = do
  qs <- query @QueryState
  dall <- Datasets.find (Datasets.ByProgram progId)

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

      col (gap 25) $ do
        Inversion.viewInversionContainer' Info $ do
          col (gap 15) $ do
            hyper (Uploads propId progId invId) $ viewUpload qs.uploads
            hyper (MetadataForm propId progId invId) $ viewMetadataForm dall qs.uploads qs.metadata

        hyper (Manage propId progId) $ do
          View.iconButton Cancel (Style.btnOutline Secondary) Icons.xMark "Cancel"


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
  let new = uploads taskId tup

  files <- query
  redirect $ setUploadQuery (files <> new) $ routeUrl $ Route.inversionUpload propId progId invId
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


data QueryState = QueryState
  { uploads :: UploadFiles Filename UploadStatus
  , metadata :: Metadata Identity
  }
instance ToQuery QueryState where
  toQuery qs = toQuery qs.uploads <> toQuery qs.metadata
instance FromQuery QueryState where
  parseQuery qd = do
    up <- parseQuery qd
    md <- parseQuery qd
    pure $ QueryState up md


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
  deriving (Show, Read, ViewId)


instance (Auth :> es, Log :> es, Globus :> es, Inversions :> es) => HyperView Uploads es where
  data Action Uploads
    = Upload
    | UpTransfer (Id Task) TransferAction
    deriving (Show, Read, ViewAction)


  update action = do
    Uploads propId progId invId <- viewId
    case action of
      Upload -> do
        files <- query
        let u = setUploadQuery files $ routeUrl $ Route.submitUpload propId progId invId
        openFileManager (Files 3) u ("Transfer Inversion Results " <> invId.fromId)
      UpTransfer taskId trans -> do
        files <- query
        case trans of
          TaskFailed -> do
            pure $ viewUploadWithTransfer files $ do
              Transfer.viewTransferFailed taskId
          TaskSucceeded -> do
            let files' = filesUploaded files
            setQuery $ QueryState files' def
            pure $ viewUpload files'
          CheckTransfer -> do
            vw <- Transfer.checkTransfer (UpTransfer taskId) taskId
            pure $ viewUploadWithTransfer files vw
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


viewUpload :: UploadFiles Filename UploadStatus -> View Uploads ()
viewUpload files = do
  viewUploadWithTransfer files $ do
    case allUploadStatus files of
      Uploading taskId -> do
        Transfer.viewLoadTransfer (UpTransfer taskId)
      NoUpload ->
        View.iconButton Upload (Style.btn Primary) Icons.upTray "Select Files"
      Uploaded ->
        View.iconButton Upload (Style.btnOutline Success) Icons.upTray "Select New Files"


viewUploadWithTransfer :: UploadFiles Filename UploadStatus -> View Uploads () -> View Uploads ()
viewUploadWithTransfer files xfer = do
  el (Style.subheader . headerColor) "Upload Inversion Results"
  col (pad 10 . gap 10) $ do
    col (gap 5) $ do
      uploadedFile files.quantities "inv_res_pre.fits"
      uploadedFile files.profileFit "inv_res_mod.fits"
      uploadedFile files.profileOrig "per_ori.fits"
    xfer
 where
  -- stepMetadata StepNext none
  -- stepGenerate StepNext none
  -- stepPublish StepNext none

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

  headerColor =
    case allUploadStatus files of
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
  deriving (Show, Read, ViewId)


-- TODO: store this information in the query as well!
instance (Log :> es, Inversions :> es, Datasets :> es) => HyperView MetadataForm es where
  data Action MetadataForm
    = SetDataset (Id Dataset) Bool
    | SaveCommit GitCommit
    | Submit
    deriving (Show, Read, ViewAction)


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
          redirect $ routeUrl $ Route.inversion propId invId

        pure $ viewMetadataForm' dall qs.uploads qs.metadata val
   where
    saveCommit dall qs commit = do
      setQuery $ qs{metadata = Metadata qs.metadata.datasets (Just commit)}
      pure $ viewMetadataForm' dall qs.uploads qs.metadata (Metadata NotInvalid Valid)

    anyInvalid meta =
      isInvalid meta.commit || isInvalid meta.datasets


-- Feedback on the datasets missing?

viewMetadataForm :: [Dataset] -> UploadFiles Filename UploadStatus -> Metadata Identity -> View MetadataForm ()
viewMetadataForm dall uploads meta = do
  viewMetadataForm' dall uploads meta (Metadata NotInvalid (CommitForm.existingCommit meta.commit))


viewMetadataForm' :: [Dataset] -> UploadFiles Filename UploadStatus -> Metadata Identity -> Metadata Validated -> View MetadataForm ()
viewMetadataForm' dall uploads meta valid = do
  case allUploadStatus uploads of
    Uploaded -> do
      el (Style.subheader . color Info) "Metadata"
      col (gap 15 . pad 10) $ do
        viewDatasets dall meta.datasets valid.datasets
        commitForm SaveCommit meta.commit (convert valid.commit)
        button Submit (Style.btn Primary) "Create Inversion"
    _ -> none
 where
  convert (Invalid t) = Invalid t
  convert Valid = Valid
  convert NotInvalid = NotInvalid


viewDatasets :: [Dataset] -> [Id Dataset] -> Validated [Id Dataset] -> View MetadataForm ()
viewDatasets dall dsel valid = do
  col (gap 5) $ do
    el (bold . validColor valid) "Datasets Used"
    forM_ dall $ \d -> do
      row (gap 10) $ do
        View.checkBtn (SetDataset d.datasetId) (d.datasetId `elem` dsel)
        route (Route.Datasets $ Route.Dataset d.datasetId) Style.link $ text d.datasetId.fromId
    case valid of
      Invalid t -> el (color Danger) (text t)
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
  validateCommit Nothing = Invalid "Missing Git Commit"
  validateCommit _ = Valid


-----------------------------------------------------------------
-- MANAGE
-----------------------------------------------------------------

data Manage = Manage (Id Proposal) (Id InstrumentProgram)
  deriving (Show, Read, ViewId)


instance HyperView Manage es where
  data Action Manage
    = Cancel
    deriving (Show, Read, ViewAction)


  update Cancel = do
    Manage propId progId <- viewId
    redirect $ routeUrl $ Route.program propId progId

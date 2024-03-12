module App.Page.Inversion where

import App.Colors
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Inversions.InvForm
import App.Route as Route
import App.Style qualified as Style
import App.View.Icons qualified as Icons
import App.View.Layout
import Data.Diverse.Many
import Data.Maybe (isJust)
import Data.Text (pack)
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole


page :: (Hyperbole :> es, Inversions :> es, Datasets :> es, Auth :> es, Globus :> es) => Id Inversion -> InversionRoute -> Page es Response
page i Inv = pageMain i
page i SubmitDownload = pageSubmitDownload i
page i SubmitUpload = pageSubmitUpload i


pageMain :: (Hyperbole :> es, Inversions :> es, Auth :> es, Globus :> es) => Id Inversion -> Page es Response
pageMain i = do
  hyper $ inversions redirectHome
  hyper inversionCommit
  load $ do
    -- TODO: handle taskId query param, and pass forward!
    (inv :| _) <- send (Inversions.ById i) >>= expectFound
    step <- currentStep inv.step
    appLayout Inversions $ do
      col Style.page $ do
        col (gap 5) $ do
          el Style.header $ do
            text "Inversion: "
            text i.fromId

          el_ $ do
            text "Program: "
            route (Program inv.programId) Style.link $ do
              text inv.programId.fromId

        viewId (InversionStatus inv.programId) $ viewInversion inv step


pageSubmitUpload :: forall es. (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es) => Id Inversion -> Page es Response
pageSubmitUpload ii = do
  load $ do
    tfrm <- parseForm @TransferForm
    tup <- parseForm @UploadFiles
    it <- Globus.initUpload tfrm tup ii
    send $ Inversions.SetUploading ii it

    redirect $ routeUrl (Route.Inversion ii Inv)


pageSubmitDownload :: (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es, Auth :> es) => Id Inversion -> Page es Response
pageSubmitDownload ii = do
  load $ do
    tfrm <- parseForm @TransferForm
    tfls <- parseForm @DownloadFolder
    (inv :| _) <- send (Inversions.ById ii) >>= expectFound
    ds <- send $ Datasets.Query $ Datasets.ByProgram inv.programId
    it <- Globus.initDownload tfrm tfls ds
    send $ Inversions.SetDownloading ii it

    redirect $ routeUrl (Route.Inversion ii Inv)


redirectHome :: (Hyperbole :> es) => Eff es (View InversionStatus ())
redirectHome = do
  redirect $ pathUrl . routePath $ Inversions


-- ----------------------------------------------------------------
-- INVERSION STATUS -----------------------------------------------
-- ----------------------------------------------------------------

data InversionStatus = InversionStatus (Id InstrumentProgram)
  deriving (Show, Read, Param)


data InversionsAction
  = CreateInversion
  | Update (Id Inversion) InversionAction
  | CheckDownload (Id Inversion) (Id Task)
  | CheckUpload (Id Inversion) (Id Task)
  deriving (Show, Read, Param)


data InversionAction
  = Download
  | Cancel
  | PreprocessValid
  | Preprocess GitCommit
  | Upload
  | PostProcess
  | Publish
  deriving (Show, Read, Param)


data PreprocessForm a = PreprocessForm
  { preprocessSoftware :: Field a Text
  }
  deriving (Generic, Form)


data InversionForm a = InversionForm
  { inversionSoftware :: Field a Text
  }
  deriving (Generic, Form)


instance HyperView InversionStatus where
  type Action InversionStatus = InversionsAction


inversions :: (Hyperbole :> es, Inversions :> es, Globus :> es, Auth :> es) => Eff es (View InversionStatus ()) -> InversionStatus -> InversionsAction -> Eff es (View InversionStatus ())
inversions onCancel (InversionStatus ip) = \case
  CreateInversion -> do
    inv <- send $ Inversions.Create ip
    step <- currentStep inv.step
    pure $ viewInversion inv step
  (Update iid act) ->
    case act of
      Cancel -> do
        send $ Inversions.Remove iid
        -- then redirect to home.. but that won't refresh!
        onCancel
      Download -> do
        r <- request
        redirect $ Globus.fileManagerUrl (Folders 1) (Route.Inversion iid SubmitDownload) ("Transfer Instrument Program " <> ip.fromId) r
      PreprocessValid -> do
        inv <- send (Inversions.ById iid) >>= expectFound
        f <- parseForm @PreprocessForm
        let commit = GitCommit f.preprocessSoftware
        pure $ loading (Update iid (Preprocess commit)) Preprocessing $ stepPreprocess (head inv) False
      Preprocess commit -> do
        valid <- send $ ValidatePreprocessCommit commit
        validate iid Preprocessing stepPreprocess valid
        send $ Inversions.SetPreprocessed iid commit
        refresh iid
      Upload -> do
        r <- request
        redirect $ Globus.fileManagerUrl (Files 3) (Route.Inversion iid SubmitUpload) ("Transfer Inversion Results " <> iid.fromId) r
      PostProcess -> do
        send $ Inversions.SetGenerated iid
        refresh iid
      Publish -> do
        send $ Inversions.SetPublished iid
        refresh iid
  CheckDownload vi ti -> do
    t <- Globus.transferStatus ti
    checkDownload vi ti t
  CheckUpload vi ti -> do
    t <- Globus.transferStatus ti
    checkUpload vi ti t
 where
  refresh iid = do
    (inv :| _) <- send (Inversions.ById iid) >>= expectFound
    step <- currentStep inv.step
    pure $ viewInversion inv step

  -- we need to pass some kind of validation to the view itself
  validate :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> CurrentStep -> (Inversion -> Bool -> View InversionStatus ()) -> Bool -> Eff es ()
  validate _ _ _ True = pure ()
  validate iid progStep viewStep False = do
    inv <- send (Inversions.ById iid) >>= expectFound
    respondEarly (InversionStatus ip) $ do
      viewInversionContainer progStep $ do
        viewStep (head inv) True
  -- where
  -- validationError :: Text -> View c ()
  -- validationError msg = do
  --   el (border 1 . borderColor (dark Danger) . bg Danger . pad 10) $ do
  --     el (color White) $ text msg

  loading :: InversionsAction -> CurrentStep -> View InversionStatus () -> View InversionStatus ()
  loading act step cnt =
    viewInversionContainer step $ onLoad act 0 $ do
      el Style.disabled cnt


checkDownload :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> Id Task -> Task -> Eff es (View InversionStatus ())
checkDownload ii ti t' = do
  (inv :| _) <- send (Inversions.ById ii) >>= expectFound
  case t'.status of
    Succeeded -> do
      -- here is where we set that we have finished downloading
      send (Inversions.SetDownloaded ii)
      pure $ viewInversionContainer Preprocessing $ do
        stepPreprocess inv False
    Failed -> pure $ do
      viewInversionContainer (Downloading (Transferring ti)) $ do
        viewTransferFailed t'
        stepDownload inv Select
    _ -> pure $ do
      viewInversionContainer (Downloading (Transferring ti)) $ do
        onLoad (CheckDownload ii ti) 5000 $ do
          viewTransferProgress t'


checkUpload :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> Id Task -> Task -> Eff es (View InversionStatus ())
checkUpload vi ti t = do
  case t.status of
    Succeeded -> do
      inv <- send (Inversions.ById vi) >>= expectFound
      send (Inversions.SetUploaded vi)
      pure $ viewInversionContainer (Inverting mempty) $ do
        stepInvert mempty (head inv)
    Failed -> pure $ do
      viewInversionContainer (Inverting mempty) $ do
        viewTransferFailed t
    _ -> pure $ do
      viewInversionContainer (Inverting mempty) $ do
        onLoad (CheckUpload vi ti) 5000 $ do
          viewTransferProgress t


viewInversionContainer :: CurrentStep -> View InversionStatus () -> View InversionStatus ()
viewInversionContainer step cnt =
  col (Style.card . gap 15) $ do
    el (Style.cardHeader (headerColor step)) "Inversion"
    col (gap 15 . pad 15) $ do
      invProgress step
      cnt
 where
  headerColor Complete = Success
  headerColor _ = Info


viewInversion :: Inversion -> CurrentStep -> View InversionStatus ()
viewInversion inv step = do
  viewInversionContainer step $ do
    viewStep step
 where
  viewStep :: CurrentStep -> View InversionStatus ()
  viewStep (Downloading ts) = stepDownload inv ts
  viewStep Preprocessing = stepPreprocess inv False
  viewStep (Inverting is) = stepInvert is inv
  viewStep Generating = stepGenerate
  viewStep Publishing = stepPublish
  viewStep Complete = stepDone

  stepGenerate = do
    button (Update inv.inversionId PostProcess) (Style.btn Primary . grow) "Save FITS Generation"

  stepPublish = do
    button (Update inv.inversionId Publish) (Style.btn Primary . grow) "Save Publish"

  stepDone = do
    el_ "Done"


stepDownload :: Inversion -> TransferStep -> View InversionStatus ()
stepDownload inv Select = do
  el_ "Please select a destination folder for the instrument program's datasets. You will be redirected to Globus."

  row (gap 10) $ do
    button (Update inv.inversionId Download) (Style.btn Primary . grow) "Choose Folder"
    button (Update inv.inversionId Cancel) (Style.btnOutline Secondary) "Cancel"
stepDownload inv (Transferring it) = do
  -- don't show anything until load
  onLoad (CheckDownload inv.inversionId it) 0 $ do
    el (height 60) ""


stepPreprocess :: Inversion -> Bool -> View InversionStatus ()
stepPreprocess inv isInvalid = do
  form @PreprocessForm (Update inv.inversionId PreprocessValid) (gap 10) $ \f -> do
    field id $ do
      label "Proprocessing Git Commit"
      input TextInput (Style.input Gray) f.preprocessSoftware
    submit (Style.btn Primary . grow) "Submit Preprocessed"


-- if we have one set on the inversion itself, it's valid
stepInvert :: InvertStep -> Inversion -> View InversionStatus ()
stepInvert (InvertStep mc mt) inv = do
  viewId (InversionCommit inv.inversionId) $ commitForm (fromExistingCommit mc)

  -- inversion_file = self.scan_dir + 'inv_res_pre.fits'
  -- observed_file  = self.scan_dir + 'per_ori.fits'
  -- models_file    = self.scan_dir + 'inv_res_mod.fits'

  maybe selectUpload viewTransfer mt
 where
  selectUpload = do
    col (gap 5) $ do
      el bold "Upload Inversion Results"
      el_ "Please select the following files for upload. You will be redirected to Globus"
      tag "li" id "inv_res_pre.fits"
      tag "li" id "per_ori.fits"
      tag "li" id "inv_res_mod.fits"
    button (Update inv.inversionId Upload) (Style.btn Primary . grow) "Select Files"

  viewTransfer ti = do
    onLoad (CheckUpload inv.inversionId ti) 0 $ do
      el (height 60) ""


data CurrentStep
  = Downloading TransferStep
  | Preprocessing
  | Inverting InvertStep
  | Generating
  | Publishing
  | Complete
  deriving (Eq, Ord, Show)


data TransferStep
  = Select
  | Transferring (Id Task)
  deriving (Eq, Ord, Show)


data InvertStep = InvertStep (Maybe GitCommit) (Maybe (Id Task))
  deriving (Show, Eq, Ord)
instance Semigroup InvertStep where
  InvertStep gc1 ti1 <> InvertStep gc2 ti2 = InvertStep (gc1 <|> gc2) (ti1 <|> ti2)
instance Monoid InvertStep where
  mempty = InvertStep Nothing Nothing


newtype CurrentTask = CurrentTask Task
  deriving (Eq)


currentStep :: (Globus :> es) => InversionStep -> Eff es CurrentStep
currentStep = \case
  StepCreated _ -> pure (Downloading Select)
  StepDownloading dwn -> do
    let t = grab @Transfer dwn :: Transfer
    pure $ Downloading $ Transferring (Id t.taskId)
  StepDownloaded _ -> pure Preprocessing
  StepPreprocessed _ -> pure (Inverting mempty)
  StepInverting inv -> do
    let i = grab @Invert inv
    pure $ Inverting $ InvertStep i.commit (Id <$> i.taskId)
  StepInverted _ -> pure Generating
  StepGenerated _ -> pure Publishing
  StepPublished _ -> pure Complete


invProgress :: CurrentStep -> View InversionStatus ()
invProgress curr = do
  row (gap 10) $ do
    prgDown curr "1" "DOWNLOAD"
    prgStep Preprocessing "2" "PREPROCESS"
    prgInv curr "3" "INVERT"
    prgStep Generating "4" "GENERATE"
    prgStep Publishing "5" "PUBLISH"
 where
  stat :: AppColor -> View InversionStatus () -> Text -> View InversionStatus ()
  stat clr icn lbl = col (color clr . gap 4) $ do
    row id $ do
      space
      el (circle . bg clr) icn
      space
    el (fontSize 12 . textAlign Center) (text lbl)

  prgDown (Downloading Select) icn lbl = do
    stat Info icn lbl
    line Gray
  prgDown (Downloading _) icn lbl = do
    stat Info icn lbl
    line Info
  prgDown _ _ lbl = do
    stat Success Icons.check lbl
    line Success

  prgInv (Inverting (InvertStep mc mt)) icn lbl = do
    stat Info icn lbl
    line $
      if isJust mc || isJust mt
        then Info
        else Gray
  prgInv _ icn lbl = do
    let clr = statColor (Inverting mempty)
    stat clr (statIcon (Inverting mempty) icn) lbl
    line clr

  prgStep s icn lbl = do
    stat (statColor s) (statIcon s icn) lbl
    line (lineColor s)

  statIcon s icon
    | s < curr = Icons.check
    | otherwise = icon

  statColor s
    | s == curr = Info
    | s < curr = Success
    | otherwise = Gray

  line clr = col grow $ do
    el (border (TRBL 0 0 2 0) . height 20 . borderColor clr) ""

  lineColor s
    | s == curr = Gray
    | s < curr = Success
    | otherwise = Gray

  circle = rounded 50 . pad 5 . color White . textAlign Center . width 34 . height 34

-- isInverting (Inverting _) = True
-- isInverting _ = False

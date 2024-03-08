module App.Page.Inversion where

import App.Colors
import App.Error (expectFound)
import App.Globus as Globus
import App.Page.Inversions.InvForm
import App.Route as Route
import App.Style qualified as Style
import App.View.Icons qualified as Icons
import App.View.Inversions as View
import App.View.Layout
import Data.Diverse.Many
import Debug.Trace
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Web.Hyperbole


page :: (Hyperbole :> es, Inversions :> es, Datasets :> es, Layout :> es) => Id Inversion -> InversionRoute -> Page es Response
page i Inv = pageMain i
page i SubmitDownload = pageSubmitDownload i


pageMain :: (Hyperbole :> es, Inversions :> es, Layout :> es) => Id Inversion -> Page es Response
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
            link (Program inv.programId) Style.link $ do
              text inv.programId.fromId

        viewId (InversionStatus inv.programId) $ viewInversion inv step


pageSubmitDownload :: (Hyperbole :> es, Globus :> es, Datasets :> es, Inversions :> es) => Id Inversion -> Page es Response
pageSubmitDownload iv = do
  load $ do
    t <- parseTransferForm
    (inv :| _) <- send (Inversions.ById iv) >>= expectFound
    ds <- send $ Datasets.Query $ Datasets.ByProgram inv.programId
    it <- Globus.initDownload t ds

    send $ Inversions.SetDownloading iv it

    redirect $ routeUrl (Route.Inversion iv Inv)


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


inversions :: (Hyperbole :> es, Inversions :> es, Globus :> es) => Eff es (View InversionStatus ()) -> InversionStatus -> InversionsAction -> Eff es (View InversionStatus ())
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
        redirect $ Globus.fileManagerUrl (Route.Inversion iid SubmitDownload) ("Transfer Instrument Program " <> ip.fromId) r
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
        pure $ el_ "Upload"
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
checkDownload vi ti t' = do
  case t'.status of
    Succeeded -> do
      inv <- send (Inversions.ById vi) >>= expectFound
      -- here is where we set that we have finished downloading
      send (Inversions.SetDownloaded vi)
      pure $ viewInversionContainer Preprocessing $ do
        stepPreprocess (head inv) False
    Failed -> pure $ do
      viewInversionContainer (Downloading (Transferring ti)) $ do
        viewTransferFailed t'
    _ -> pure $ do
      viewInversionContainer (Downloading (Transferring ti)) $ do
        onLoad (CheckDownload vi ti) 5000 $ do
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
  viewStep (Downloading ts) = stepDownload ts
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

  stepDownload :: TransferStep -> View InversionStatus ()
  stepDownload Select = do
    el_ "Please select a destination for this instrument program. You will be redirected to Globus."

    row (gap 10) $ do
      button (Update inv.inversionId Download) (Style.btn Primary) "Choose Download Location"

    row (gap 10) $ do
      button (Update inv.inversionId Cancel) (Style.btnOutline Secondary) "Cancel"
      el (grow . Style.disabled . Style.btn Primary) "Submit Download"
  stepDownload (Transferring it) = do
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

  maybe selectUpload viewTransfer mt
 where
  selectUpload = do
    col (gap 5) $ do
      el bold "Upload FITS Files"
      el_ "You will be redirected to Globus. Please select a folder containing inverted FITS files"
      button (Update inv.inversionId Download) (Style.btn Primary . grow) "Choose Folder"

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
    stat (Downloading Select) "DOWNLOAD" "1"
    line (Downloading Select)
    stat Preprocessing "PREPROCESS" "2"
    line Preprocessing
    stat (Inverting mempty) "INVERT" "3"
    line (Inverting mempty)
    stat Generating "GENERATE" "4"
    line Generating
    stat Publishing "PUBLISH" "5"
 where
  stat :: CurrentStep -> Text -> View InversionStatus () -> View InversionStatus ()
  stat s t icon = col (color (statColor curr) . gap 4) $ do
    row id $ do
      space
      el (circle . bg (statColor curr)) (statIcon curr)
      space
    el (fontSize 12 . textAlign Center) (text t)
   where
    statIcon (Inverting _)
      | isInverting s = icon
      | otherwise = statIcon'
    statIcon _ = statIcon'

    statIcon'
      | s < curr = Icons.check
      | otherwise = icon

    statColor (Inverting _)
      | isInverting s = Info
      | otherwise = statColor'
    statColor _ = statColor'

    statColor'
      | s == curr = Info
      | s < curr = Success
      | otherwise = Gray

  line s = col grow $ do
    el (border (TRBL 0 0 2 0) . height 20 . borderColor (lineColor s)) ""
   where
    lineColor (Inverting _)
      | isInverting s = Gray
      | otherwise = lineColor'
    lineColor _ = lineColor'
    lineColor'
      | s == curr = Gray
      | s < curr = Success
      | otherwise = Gray

  circle = rounded 50 . pad 5 . color White . textAlign Center . width 34 . height 34

  isInverting (Inverting _) = True
  isInverting _ = False

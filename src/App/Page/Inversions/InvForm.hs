module App.Page.Inversions.InvForm where

import App.Colors
import App.Globus as Globus
import App.Style qualified as Style
import Debug.Trace
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common (Id (..))
import Numeric (showFFloat)
import Web.Hyperbole
import Web.View qualified as WebView


-----------------------------------------------------
-- Inversion Transfer
-----------------------------------------------------
--
data TransferAction
  = CheckTransfer
  | TaskFailed
  | TaskSucceeded
  deriving (Show, Read, Param)


-- I want it to reload itself and call these when necessary
checkTransfer :: (HyperView id, Action id ~ TransferAction, Hyperbole :> es, Globus :> es, Auth :> es) => Id Task -> Eff es (View id ())
checkTransfer it = do
  task <- Globus.transferStatus it
  pure $ viewTransfer it task


viewLoadTransfer :: (HyperView id, Action id ~ TransferAction) => View id ()
viewLoadTransfer = do
  onLoad CheckTransfer 0 none


viewTransfer :: (HyperView id, Action id ~ TransferAction) => Id Task -> Task -> View id ()
viewTransfer it task =
  case task.status of
    Succeeded -> onLoad TaskSucceeded 0 none
    Failed -> onLoad TaskFailed 0 none
    _ ->
      viewPollTransfer it task


viewPollTransfer :: (HyperView id, Action id ~ TransferAction) => Id Task -> Task -> View id ()
viewPollTransfer it task = do
  onLoad CheckTransfer 5000 $ do
    viewTransferProgress it task


viewTransferProgress :: Id Task -> Task -> View c ()
viewTransferProgress it task = do
  row id $ do
    el_ $ text $ "Transferring... (" <> cs rate <> " Mb/s)"
    space
    activityLink it
  progress (taskPercentComplete task)
 where
  rate :: String
  rate = showFFloat (Just 2) (fromIntegral task.effective_bytes_per_second / (1000 * 1000) :: Float) ""


viewTransferFailed :: Id Task -> View c ()
viewTransferFailed it = do
  row id $ do
    el (color Danger) "Transfer Failed"
    space
    activityLink it


activityLink :: Id Task -> View c ()
activityLink it =
  WebView.link activityUrl Style.link "View Transfer on Globus"
 where
  activityUrl = Url "https://" "app.globus.org" ["activity", it.fromId] []


progress :: Float -> View c ()
progress p = do
  row (bg Gray . height 20) $ do
    el (width (Pct p) . bg (light Info)) $ do
      space


-----------------------------------------------------
-- Inversion Commit
-----------------------------------------------------

data CommitAction
  = CheckCommitValid GitCommit
  | LoadValid
  deriving (Show, Read, Param)


data CommitForm a = CommitForm
  { gitCommit :: Field a Text
  }
  deriving (Generic, Form)


validate :: (Hyperbole :> es, Inversions :> es, HyperView id, Action id ~ CommitAction) => id -> GitRepo -> GitCommit -> Text -> Eff es () -> Eff es (Validated GitCommit)
validate i repo gc lbl onValid = do
  traceM "VALIDATE"
  traceM $ show gc
  isValid <- send $ ValidateGitCommit repo gc
  checkValid i gc lbl isValid
  onValid
  pure $ Valid gc


checkValid :: (Hyperbole :> es, HyperView id, Action id ~ CommitAction) => id -> GitCommit -> Text -> Bool -> Eff es ()
checkValid _ _ _ True = pure ()
checkValid i gc lbl False = do
  -- inv <- send (Inversions.ById ii) >>= expectFound
  respondEarly i $ do
    commitForm (Invalid gc) lbl


loadValid :: (Hyperbole :> es, HyperView id, Action id ~ CommitAction) => Text -> Eff es (View id ())
loadValid lbl = do
  f <- parseForm @CommitForm
  let gc = GitCommit f.gitCommit
  pure $ loadingForm gc lbl


loadingForm :: (HyperView id, Action id ~ CommitAction) => GitCommit -> Text -> View id ()
loadingForm gc lbl = do
  onLoad (CheckCommitValid gc) 0 $ do
    el Style.disabled $ commitForm (Prevalid gc) lbl


data Validated a
  = Valid a
  | Invalid a
  | Prevalid a
  | Empty


fromExistingCommit :: Maybe GitCommit -> Validated GitCommit
fromExistingCommit Nothing = Empty
fromExistingCommit (Just c) = Valid c


commitForm :: (HyperView id, Action id ~ CommitAction) => Validated GitCommit -> Text -> View id ()
commitForm vg lbl = do
  form @CommitForm LoadValid (gap 10) $ \f -> do
    field id $ do
      label lbl
      input TextInput (value (commitText vg) . Style.input (validationColor vg)) f.gitCommit
    validationFeedback vg
    submit (validationButton vg . grow) "Save Commit"
 where
  validationFeedback (Invalid _) =
    el (color Danger) "Invalid Git Commit"
  validationFeedback _ = none

  validationButton (Valid _) = Style.btnOutline Success
  validationButton _ = Style.btn Primary

  validationColor (Invalid _) = Danger
  validationColor (Valid _) = Success
  validationColor _ = Gray

  commitText (Valid (GitCommit t)) = t
  commitText (Invalid (GitCommit t)) = t
  commitText (Prevalid (GitCommit t)) = t
  commitText _ = ""

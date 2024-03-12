module App.Page.Inversions.InvForm where

import App.Colors
import App.Globus
import App.Style qualified as Style
import Debug.Trace
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import Numeric (showFFloat)
import Web.Hyperbole
import Web.View qualified as WebView


-----------------------------------------------------
-- Inversion Transfer
-----------------------------------------------------

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


viewTransferProgress :: Task -> View c ()
viewTransferProgress t = do
  row id $ do
    el_ $ text $ "Transferring... (" <> cs rate <> " Mb/s)"
    space
    activityLink t
  progress (taskPercentComplete t)
 where
  rate :: String
  rate = showFFloat (Just 2) (fromIntegral t.effective_bytes_per_second / (1000 * 1000) :: Float) ""


viewTransferFailed :: Task -> View c ()
viewTransferFailed t = do
  row id $ do
    el (color Danger) "Transfer Failed"
    space
    activityLink t


activityLink :: Task -> View c ()
activityLink t =
  WebView.link activityUrl Style.link "View Transfer on Globus"
 where
  activityUrl = Url "https://" "app.globus.org" ["activity", t.task_id.unTagged] []


progress :: Float -> View c ()
progress p = do
  row (bg Gray . height 20) $ do
    el (width (Pct p) . bg (light Info)) $ do
      space

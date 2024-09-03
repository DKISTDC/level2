module App.Page.Inversions.InvForm where

import App.Colors
import App.Effect.Auth
import App.Globus as Globus
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons as Icons
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

data TransferAction
  = CheckTransfer
  | TaskFailed
  | TaskSucceeded
  deriving (Show, Read, ViewAction)


-- I want it to reload itself and call these when necessary
checkTransfer :: (HyperView id, Action id ~ TransferAction, Hyperbole :> es, Globus :> es, Auth :> es) => Id Task -> Eff es (View id ())
checkTransfer it = do
  task <- requireLogin $ Globus.transferStatus it
  pure $ viewTransfer it task


viewLoadTransfer :: (HyperView id, Action id ~ TransferAction) => View id ()
viewLoadTransfer = do
  onLoad CheckTransfer 0 $ el (height 45) Icons.spinner


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
  View.progress (taskPercentComplete task)
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
  WebView.link activityUrl (Style.link . newTab) "View Transfer on Globus"
 where
  activityUrl = Url "https://" "app.globus.org" ["activity", it.fromId] []
  newTab = att "target" "_blank"


-----------------------------------------------------
-- Inversion Commit
-----------------------------------------------------

data CommitAction
  = CheckCommitValid GitCommit
  | LoadValid
  deriving (Show, Read, ViewAction)


validate :: (Hyperbole :> es, Inversions :> es, HyperView id, Action id ~ CommitAction) => id -> GitRepo -> GitCommit -> Text -> Eff es () -> Eff es (Validated GitCommit)
validate i repo gc lbl onValid = do
  isValid <- send $ ValidateGitCommit repo gc
  checkValid i gc lbl isValid
  onValid
  pure Valid


checkValid :: (Hyperbole :> es, HyperView id, Action id ~ CommitAction) => id -> GitCommit -> Text -> Bool -> Eff es ()
checkValid _ _ _ True = pure ()
checkValid i gc lbl False = do
  -- inv <- send (Inversions.ById ii) >>= expectFound
  respondEarly i $ do
    commitForm (Just gc) (Invalid "Git Commit not found in remote repository") lbl


loadValid :: (Hyperbole :> es, HyperView id, Action id ~ CommitAction) => Text -> Eff es (View id ())
loadValid lbl = do
  gc <- formField @GitCommit
  pure $ loadingForm gc lbl


loadingForm :: (HyperView id, Action id ~ CommitAction) => GitCommit -> Text -> View id ()
loadingForm gc lbl = do
  onLoad (CheckCommitValid gc) 0 $ do
    el Style.disabled $ commitForm (Just gc) NotInvalid lbl


fromExistingCommit :: Maybe GitCommit -> Validated GitCommit
fromExistingCommit Nothing = NotInvalid
fromExistingCommit (Just _) = Valid


commitForm :: (HyperView id, Action id ~ CommitAction) => Maybe GitCommit -> Validated GitCommit -> Text -> View id ()
commitForm gc vg lbl = do
  let val = validateWith @GitCommit @'[GitCommit] vg
  form LoadValid val (gap 10) $ do
    field @GitCommit valStyle $ do
      label lbl
      input TextInput (inputValue gc . Style.input)
      el (color Danger) invalidText
    -- validationFeedback vg
    submit (validationButton vg . grow) "Save Commit"
 where
  -- validationFeedback (Invalid _) =
  --   el (color Danger) "Invalid Git Commit"
  -- validationFeedback _ = none

  inputValue Nothing = id
  inputValue (Just (GitCommit c)) = value c

  valStyle v = color (valColor v)

  valColor (Invalid _) = Danger
  valColor Valid = Success
  valColor _ = Black


validationButton :: Validated GitCommit -> Mod
validationButton Valid = Style.btnOutline Success
validationButton _ = Style.btn Primary

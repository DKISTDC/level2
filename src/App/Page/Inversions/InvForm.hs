module App.Page.Inversions.InvForm where

import App.Colors
import App.Effect.Auth
import App.Globus as Globus
import App.Style qualified as Style
import App.View.Common qualified as View
import App.View.Icons as Icons
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common (Id (..))
import Numeric (showFFloat)
import Web.Hyperbole
import Web.Hyperbole.Forms (Input)
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


validate :: (Log :> es, Show id, Hyperbole :> es, Inversions :> es, HyperView id, Action id ~ CommitAction) => id -> GitRepo -> GitCommit -> View (Input id Validated GitCommit) () -> Eff es () -> Eff es (Validated GitCommit)
validate i repo gc lbl onValid = do
  isValid <- send $ ValidateGitCommit repo gc
  log Debug $ dump "validate" (isValid, gc)
  checkValid i gc lbl isValid
  onValid
  pure Valid


checkValid :: (Show id, Hyperbole :> es, HyperView id, Action id ~ CommitAction) => id -> GitCommit -> View (Input id Validated GitCommit) () -> Bool -> Eff es ()
checkValid _ _ _ True = pure ()
checkValid i gc lbl False = do
  -- inv <- send (Inversions.ById ii) >>= expectFound
  respondEarly i $ do
    commitForm (Just gc) (CommitForm $ Invalid "Git Commit not found in remote repository")


loadValid :: (Hyperbole :> es, Log :> es, HyperView id, Action id ~ CommitAction) => View (Input id Validated GitCommit) () -> Eff es (View id ())
loadValid lbl = do
  cf <- formData @CommitForm
  pure $ loadingForm cf.gitCommit lbl


loadingForm :: (HyperView id, Action id ~ CommitAction) => GitCommit -> View (Input id Validated GitCommit) () -> View id ()
loadingForm gc lbl = do
  onLoad (CheckCommitValid gc) 0 $ do
    el Style.disabled $ commitForm (Just gc) (CommitForm NotInvalid)


fromExistingCommit :: Maybe GitCommit -> CommitForm Validated
fromExistingCommit Nothing = CommitForm NotInvalid
fromExistingCommit (Just _) = CommitForm Valid


data CommitForm f = CommitForm
  { gitCommit :: Field f GitCommit
  }
  deriving (Generic)
instance Form CommitForm Validated


commitForm :: (HyperView id, Action id ~ CommitAction) => Maybe GitCommit -> CommitForm Validated -> View id ()
commitForm gc vf = do
  search (CheckCommitValid . GitCommit) 500 (valStyle vf.gitCommit . inputValue gc . Style.input . placeholder "6ed37aa902969d8e3420159b2f9cfb032d00cf82")
  invalidMessage
 where
  -- let f = formFieldsWith vf
  -- let val = validateWith @GitCommit @'[GitCommit] vg
  -- form @CommitForm LoadValid (gap 10 . flexRow) $ do
  --   submit (validationButton vf.gitCommit) "Save"
  --   field f.gitCommit valStyle $ do
  --     lbl
  --     input TextInput (inputValue gc . Style.input . placeholder "6ed37aa902969d8e3420159b2f9cfb032d00cf82")
  --     el (color Danger) invalidText

  -- validationFeedback vg

  -- validationFeedback (Invalid _) =
  --   el (color Danger) "Invalid Git Commit"
  -- validationFeedback _ = none

  inputValue Nothing = id
  inputValue (Just (GitCommit c)) = value c

  valStyle v = color (valColor v) . grow

  invalidMessage =
    case vf.gitCommit of
      Invalid msg -> el (color Danger) (text msg)
      _ -> none

  valColor (Invalid _) = Danger
  valColor Valid = Success
  valColor _ = Black


validationButton :: Validated GitCommit -> Mod
validationButton Valid = Style.btnOutline Success
validationButton _ = Style.btn Primary

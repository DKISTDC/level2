module App.Page.Inversions.InvForm where

import App.Colors
import App.Error
import App.Style qualified as Style
import Debug.Trace
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import Web.Hyperbole


data InversionCommit = InversionCommit (Id Inversion)
  deriving (Show, Read, Param)
instance HyperView InversionCommit where
  type Action InversionCommit = CommitAction


data CommitAction
  = CheckCommitValid GitCommit
  | LoadValid
  deriving (Show, Read, Param)


data CommitForm a = CommitForm
  { gitCommit :: Field a Text
  }
  deriving (Generic, Form)


inversionCommit :: (Hyperbole :> es, Inversions :> es) => InversionCommit -> CommitAction -> Eff es (View InversionCommit ())
inversionCommit (InversionCommit ii) = action
 where
  action LoadValid = do
    f <- parseForm @CommitForm
    let gc = GitCommit f.gitCommit
    pure $ loadingForm gc
  action (CheckCommitValid gc) = do
    isValid <- send $ ValidateDesireCommit gc
    validate ii gc isValid

    -- TODO: make this work for preprocessing as well!
    send $ Inversions.SetInversion ii gc

    pure $ commitForm (Valid gc)


validate :: (Hyperbole :> es, Inversions :> es) => Id Inversion -> GitCommit -> Bool -> Eff es ()
validate _ _ True = pure ()
validate ii gc False = do
  -- inv <- send (Inversions.ById ii) >>= expectFound
  respondEarly (InversionCommit ii) $ do
    commitForm (Invalid gc)


loadingForm :: GitCommit -> View InversionCommit ()
loadingForm gc = do
  onLoad (CheckCommitValid gc) 0 $ do
    el Style.disabled $ commitForm (Prevalid gc)


data Validated a
  = Valid a
  | Invalid a
  | Prevalid a
  | Empty


fromExistingCommit :: Maybe GitCommit -> Validated GitCommit
fromExistingCommit Nothing = Empty
fromExistingCommit (Just c) = Valid c


commitForm :: Validated GitCommit -> View InversionCommit ()
commitForm vg = do
  form @CommitForm LoadValid (gap 10) $ \f -> do
    field id $ do
      label "DeSIRe Git Commit"
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

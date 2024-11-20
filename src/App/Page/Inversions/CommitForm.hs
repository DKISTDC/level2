module App.Page.Inversions.CommitForm where

import App.Colors
import App.Globus as Globus
import App.Style qualified as Style
import App.View.LiveInput (liveInput, onLoading)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Time
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common (Id (..))
import NSO.Types.InstrumentProgram
import Web.Hyperbole


data InversionCommit = InversionCommit (Id Proposal) (Id Inversion)
  deriving (Show, Read, ViewId)
instance HyperView InversionCommit where
  -- type Require InversionCommit = '[InversionStatus]
  type Action InversionCommit = CommitAction


data CommitAction
  = SaveCommit GitCommit
  deriving (Show, Read, ViewAction)


inversionCommit
  :: (Hyperbole :> es, Log :> es, IOE :> es, Inversions :> es, Globus :> es, Time :> es)
  => InversionCommit
  -> CommitAction
  -> Eff es (View InversionCommit ())
inversionCommit (InversionCommit ip ii) = \case
  SaveCommit gc -> do
    log Debug $ dump "SaveCommit" gc

    guardIsValid (InversionCommit ip ii) gc
    Inversions.setInverted ii gc

    pure $ commitForm (Just gc) (CommitForm Valid)
 where
  guardIsValid i gc = do
    isValid <- send $ ValidateGitCommit vispInversionRepo gc
    log Debug $ dump "isValid" isValid
    if isValid
      then pure ()
      else respondEarly i $ do
        commitForm (Just gc) (CommitForm $ Invalid "Git Commit not found in remote repository")


-- loadingForm :: (HyperView id, Action id ~ CommitAction) => GitCommit -> View id ()
-- loadingForm gc = do
--   onLoad (CheckCommitValid gc) 0 $ do
--     el Style.disabled $ commitForm (Just gc) (CommitForm NotInvalid)

fromExistingCommit :: Maybe GitCommit -> CommitForm Validated
fromExistingCommit = \case
  Nothing -> CommitForm NotInvalid
  Just _ -> CommitForm Valid


data CommitForm f = CommitForm
  { gitCommit :: Field f GitCommit
  }
  deriving (Generic)
instance Form CommitForm Validated
instance Show (CommitForm Validated) where
  show f = "CommitForm " ++ show f.gitCommit


commitForm :: Maybe GitCommit -> CommitForm Validated -> View InversionCommit ()
commitForm gc vf = do
  link "https://github.com/DKISTDC/ViSP-Inversion" (att "target" "_blank" . Style.link) $ text "ViSP-Inversion Git Commit"
  liveInput (SaveCommit . GitCommit) (valStyle vf.gitCommit . inputValue gc . Style.input . placeholder "6ed37aa902969d8e3420159b2f9cfb032d00cf82")
  invalidMessage vf.gitCommit
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

  invalidMessage = \case
    Invalid msg -> el (color Danger . onLoading hide) (text msg)
    _ -> none

  valColor (Invalid _) = Danger
  valColor Valid = Success
  valColor _ = Black


validationButton :: Validated GitCommit -> Mod
validationButton Valid = Style.btnOutline Success
validationButton _ = Style.btn Primary

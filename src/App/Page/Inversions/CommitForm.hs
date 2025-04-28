{-# LANGUAGE UndecidableInstances #-}

module App.Page.Inversions.CommitForm where

import App.Colors
import App.Style qualified as Style
import App.View.LiveInput (liveInput)
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import Web.Hyperbole


existingCommit :: Maybe GitCommit -> Validated a
existingCommit = \case
  Nothing -> NotInvalid
  Just _ -> Valid


invalidCommit :: Validated a
invalidCommit = Invalid "Git Commit not found in remote repository"


data CommitForm f = CommitForm
  { gitCommit :: Field f GitCommit
  }
  deriving (Generic, FromFormF, GenFields Validated)
instance Show (CommitForm Validated) where
  show f = "CommitForm " ++ show f.gitCommit


commitForm :: (ViewAction (Action id)) => (GitCommit -> Action id) -> Maybe GitCommit -> Validated GitCommit -> View id ()
commitForm saveCommit gc vf = do
  col (gap 5) $ do
    link "https://github.com/DKISTDC/ViSP-Inversion" (att "target" "_blank" . Style.link . bold) $ text "ViSP-Inversion Git Commit"
    liveInput (saveCommit . GitCommit) (valStyle vf . inputValue gc . Style.input . placeholder "6ed37aa902969d8e3420159b2f9cfb032d00cf82")
    invalidMessage vf
 where
  inputValue Nothing = id
  inputValue (Just (GitCommit c)) = value c

  valStyle v = color (valColor v) . grow

  invalidMessage = \case
    Invalid msg -> el (color Danger . onRequest hide) (text msg)
    _ -> none

  valColor (Invalid _) = Danger
  valColor Valid = Success
  valColor _ = Black


validate :: (Inversions :> es) => GitCommit -> Eff es Bool
validate gc = send $ ValidateGitCommit vispInversionRepo gc

{-# LANGUAGE StrictData #-}

module NSO.Types.Status where

import Data.Aeson
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import NSO.Prelude
import NSO.Types.Common
import Network.Globus (Task)
import Rel8 hiding (select)
import Web.Hyperbole.Forms (FormField)


data ProgramStatus
  = StatusInvalid
  | StatusQualified
  | StatusError Text
  | -- we have a "latest" inversion, use its status
    StatusInversion InversionStep
  deriving (Eq)


newtype GitCommit = GitCommit Text
  deriving newtype (Show, Read, Eq, ToJSON, FromJSON, DBType, Ord)
  deriving (Generic)
  deriving anyclass (FormField)


data Created = Created {timestamp :: UTCTime}
  deriving (Show, Eq)


data Downloaded = Downloaded {timestamp :: UTCTime, taskId :: Id Task, datasets :: [Text]}
  deriving (Show, Eq)


data Preprocessed = Preprocessed {timestamp :: UTCTime, preprocessSoftware :: GitCommit}
  deriving (Show, Eq)


data Inverted = Inverted {inverted :: UTCTime, inversionSoftware :: GitCommit, uploaded :: UTCTime, uploadedTaskId :: Id Task}
  deriving (Show, Eq)


data Generated = Generated {timestamp :: UTCTime}
  deriving (Show, Eq)


data Published = Published {timestamp :: UTCTime}
  deriving (Show, Eq)


data Transfer = Transfer {taskId :: Id Task}
  deriving (Show, Eq)


data Invert = Invert
  { commit :: Maybe GitCommit
  , taskId :: Maybe (Id Task)
  }
  deriving (Show, Eq)


data Generate = Generate
  { taskCompleted :: UTCTime
  , genError :: Maybe Text
  }
  deriving (Show, Eq)


-- Data Diverse Many: https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/ManySpec.hs
-- Each step requires all previous steps to exist
type StepCreated = '[Created]
type StepDownloaded = Downloaded : StepCreated
type StepDownloading = Transfer : StepCreated
type StepPreprocessed = Preprocessed : StepDownloaded
type StepInverted = Inverted : StepPreprocessed
type StepInverting = Invert : StepPreprocessed
type StepGenerated = Generated : StepInverted
type StepGenTransfer = Transfer : StepInverted
type StepGenerating = Generate : StepGenTransfer
type StepPublished = Published : StepGenerated


data InversionStep
  = StepCreated (Many StepCreated)
  | -- we are activly downloading but haven't finished yet
    StepDownloaded (Many StepDownloaded)
  | StepDownloading (Many StepDownloading)
  | StepPreprocessed (Many StepPreprocessed)
  | -- record the inversion metadata first, then upload files
    StepInverted (Many StepInverted)
  | StepInverting (Many StepInverting)
  | StepGenerated (Many StepGenerated)
  | StepGenerating (Many StepGenerating)
  | StepGenTransfer (Many StepGenTransfer)
  | StepPublished (Many StepPublished)
  deriving (Eq, Show)


stepInverted :: InversionStep -> Maybe (Many StepInverted)
stepInverted = \case
  StepInverted inv -> pure $ selectInverted inv
  StepGenerated inv -> pure $ selectInverted inv
  StepGenerating inv -> pure $ selectInverted inv
  StepGenTransfer inv -> pure $ selectInverted inv
  StepPublished inv -> pure $ selectInverted inv
  _ -> Nothing


findInverted :: InversionStep -> Maybe Inverted
findInverted s = do
  i <- stepInverted s
  pure $ grab @Inverted i


selectCreated :: (UniqueMember Created xs) => Many xs -> Many StepCreated
selectCreated m = grab @Created m ./ nil


selectDownloaded :: (UniqueMembers StepDownloaded xs) => Many xs -> Many StepDownloaded
selectDownloaded m = grab @Downloaded m ./ selectCreated m


selectPreprocessed :: (UniqueMembers StepPreprocessed xs) => Many xs -> Many StepPreprocessed
selectPreprocessed m = grab @Preprocessed m ./ selectDownloaded m


selectInverted :: (UniqueMembers StepInverted xs) => Many xs -> Many StepInverted
selectInverted m = grab @Inverted m ./ selectPreprocessed m

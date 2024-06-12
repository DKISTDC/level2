{-# LANGUAGE StrictData #-}

module NSO.Types.Status where

import Data.Aeson
import Data.Diverse.Many
import NSO.Prelude
import Rel8
import Web.Hyperbole.Forms (FormField)


data ProgramStatus
  = StatusInvalid
  | StatusQualified
  | -- we have a "latest" inversion, use its status
    StatusInversion InversionStep
  deriving (Eq)


newtype GitCommit = GitCommit Text
  deriving newtype (Show, Read, Eq, ToJSON, FromJSON, DBType, Ord)
  deriving (Generic)
  deriving anyclass (FormField)


data Created = Created {timestamp :: UTCTime}
  deriving (Show, Eq)


data Downloaded = Downloaded {timestamp :: UTCTime, taskId :: Text, datasets :: [Text]}
  deriving (Show, Eq)


data Preprocessed = Preprocessed {timestamp :: UTCTime, preprocessSoftware :: GitCommit}
  deriving (Show, Eq)


data Inverted = Inverted {inverted :: UTCTime, inversionSoftware :: GitCommit, uploaded :: UTCTime, uploadedTaskId :: Text}
  deriving (Show, Eq)


data Generated = Generated {timestamp :: UTCTime}
  deriving (Show, Eq)


data Published = Published {timestamp :: UTCTime}
  deriving (Show, Eq)


data Transfer = Transfer {taskId :: Text}
  deriving (Show, Eq)


data Invert = Invert
  { commit :: Maybe GitCommit
  , taskId :: Maybe Text
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
  | StepPublished (Many StepPublished)
  deriving (Eq, Show)

{-# LANGUAGE StrictData #-}

module NSO.Types.Status where

import Data.Aeson
import Data.Diverse.Many
import NSO.Prelude
import NSO.Types.Common (jsonTypeInfo)
import Rel8


data ProgramStatus
  = StatusInvalid
  | StatusQualified
  | -- we have a "latest" inversion, use its status
    StatusInversion InversionStep
  deriving (Eq)


newtype GitCommit = GitCommit Text
  deriving newtype (Show, Read, Eq, ToJSON, FromJSON, DBType)


data Created = Created {timestamp :: UTCTime}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Created where
  typeInformation = jsonTypeInfo


data Downloaded = Downloaded {timestamp :: UTCTime, taskId :: Text}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Downloaded where
  typeInformation = jsonTypeInfo


data Calibrated = Calibrated {timestamp :: UTCTime, calibrationSoftware :: GitCommit}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Calibrated where
  typeInformation = jsonTypeInfo


data Inverted = Inverted {timestamp :: UTCTime, inversionSoftware :: GitCommit}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Inverted where
  typeInformation = jsonTypeInfo


data Processed = Processed {timestamp :: UTCTime}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Processed where
  typeInformation = jsonTypeInfo


data Published = Published {timestamp :: UTCTime}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Published where
  typeInformation = jsonTypeInfo


-- Data Diverse Many: https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/ManySpec.hs
-- Each step requires all previous steps to exist
type StepCreated = '[Created]
type StepDownloaded = Downloaded : StepCreated
type StepCalibrated = Calibrated : StepDownloaded
type StepInverted = Inverted : StepCalibrated
type StepProcessed = Processed : StepInverted
type StepPublished = Published : StepProcessed


data InversionStep
  = StepCreated (Many StepCreated)
  | StepDownloaded (Many StepDownloaded)
  | StepCalibrated (Many StepCalibrated)
  | StepInverted (Many StepInverted)
  | StepProcessed (Many StepProcessed)
  | StepPublished (Many StepPublished)
  deriving (Eq, Show)


stepCreated :: InversionStep -> Created
stepCreated (StepCreated m) = grab @Created m
stepCreated (StepDownloaded m) = grab @Created m
stepCreated (StepCalibrated m) = grab @Created m
stepCreated (StepInverted m) = grab @Created m
stepCreated (StepProcessed m) = grab @Created m
stepCreated (StepPublished m) = grab @Created m

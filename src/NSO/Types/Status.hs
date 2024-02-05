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


type Url = String
newtype InversionSoftware = InversionSoftware String
  deriving newtype (Show, Eq, ToJSON, FromJSON, DBType)


data Started = Started {timestamp :: UTCTime}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Started where
  typeInformation = jsonTypeInfo


data Downloaded = Downloaded {timestamp :: UTCTime}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Downloaded where
  typeInformation = jsonTypeInfo


data Calibrated = Calibrated {timestamp :: UTCTime, calibrationUrl :: Url}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Calibrated where
  typeInformation = jsonTypeInfo


data Inverted = Inverted {timestamp :: UTCTime, inversionSoftware :: InversionSoftware}
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
type StepStarted = '[Started]
type StepDownloaded = Downloaded : StepStarted
type StepCalibrated = Calibrated : StepDownloaded
type StepInverted = Inverted : StepCalibrated
type StepProcessed = Processed : StepInverted
type StepPublished = Published : StepProcessed


data InversionStep
  = StepStarted (Many StepStarted)
  | StepDownloaded (Many StepDownloaded)
  | StepCalibrated (Many StepCalibrated)
  | StepInverted (Many StepInverted)
  | StepProcessed (Many StepProcessed)
  | StepPublished (Many StepPublished)
  deriving (Eq)

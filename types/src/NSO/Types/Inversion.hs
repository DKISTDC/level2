{-# LANGUAGE StrictData #-}

module NSO.Types.Inversion where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import Network.Globus (Task)
import Rel8 (Column, DBType, Rel8able, Result)
import Web.Hyperbole.Forms (FromHttpApiData)


data Inversion = Inversion
  { inversionId :: Id Inversion
  , programId :: Id InstrumentProgram
  , proposalId :: Id Proposal
  , created :: UTCTime
  , updated :: UTCTime
  , download :: StepDownload
  , invert :: StepInvert
  , generate :: StepGenerate
  , publish :: StepPublish
  , invError :: Maybe Text
  }


-- The database definition is flattened. Needs validation on return from DB!
data InversionRow f = InversionRow
  { inversionId :: Column f (Id Inversion)
  , programId :: Column f (Id InstrumentProgram)
  , proposalId :: Column f (Id Proposal)
  , created :: Column f UTCTime
  , updated :: Column f UTCTime
  , downloaded :: Column f (Maybe UTCTime)
  , downloadTaskId :: Column f (Maybe (Id Task))
  , downloadDatasets :: Column f [Id Dataset]
  , uploaded :: Column f (Maybe UTCTime)
  , uploadTaskId :: Column f (Maybe (Id Task))
  , inverted :: Column f (Maybe UTCTime)
  , inversionSoftware :: Column f (Maybe GitCommit)
  , generatedFits :: Column f (Maybe UTCTime)
  , generatedAsdf :: Column f (Maybe UTCTime)
  , generateTaskId :: Column f (Maybe (Id Task))
  , generateTaskCompleted :: Column f (Maybe UTCTime)
  , published :: Column f (Maybe UTCTime)
  , invError :: Column f (Maybe Text)
  }
  deriving (Generic, Rel8able)


deriving stock instance (f ~ Result) => Show (InversionRow f)
deriving stock instance (f ~ Result) => Eq (InversionRow f)


newtype GitCommit = GitCommit Text
  deriving newtype (Show, Read, Eq, ToJSON, FromJSON, DBType, Ord, FromHttpApiData)
  deriving (Generic)


data StepDownload
  = StepDownloadNone
  | StepDownloading Downloading
  | StepDownloaded Downloaded
  deriving (Eq, Ord, Show)
data Downloading = Downloading {transfer :: Id Task, datasets :: [Id Dataset]}
  deriving (Eq, Ord, Show)
data Downloaded = Downloaded {complete :: UTCTime, datasets :: [Id Dataset]}
  deriving (Eq, Ord, Show)


data StepInvert
  = StepInvertNone
  | StepInverting Inverting
  | StepInverted Inverted
  deriving (Eq, Ord, Show)
data Inverted = Inverted {transfer :: Id Task, commit :: GitCommit, complete :: UTCTime}
  deriving (Eq, Ord, Show)
data Inverting = Inverting {transfer :: Maybe (Id Task), commit :: Maybe GitCommit}
  deriving (Eq, Ord, Show)


data StepGenerate
  = StepGenerateNone
  | StepGenerateWaiting
  | StepGenerateError Text
  | StepGeneratingFits (Id Task)
  | StepGeneratingAsdf GeneratedFits
  | StepGenerated Generated
  deriving (Eq, Ord, Show)
data Generated = Generated {generatedFits :: UTCTime, generatedAsdf :: UTCTime, transfer :: Id Task}
  deriving (Eq, Ord, Show)
data GeneratedFits = GeneratedFits {generatedFits :: UTCTime, transfer :: Id Task}
  deriving (Eq, Ord, Show)


data StepPublish
  = StepPublishNone
  | StepPublishing
  | StepPublished UTCTime
  deriving (Eq, Ord, Show)


data InversionStep
  = StepDownload StepDownload
  | StepInvert StepInvert
  | StepGenerate StepGenerate
  | StepPublish StepPublish
  deriving (Eq, Ord, Show)

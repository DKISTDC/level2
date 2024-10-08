{-# LANGUAGE StrictData #-}

module NSO.Types.Status where

import Data.Aeson
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import NSO.Prelude
import NSO.Types.Common
import Network.Globus (Task)
import Rel8 hiding (select)
import Web.Hyperbole.Forms (FromHttpApiData)


data ProgramStatus
  = StatusInvalid
  | StatusQualified
  | StatusError Text
  | -- we have a "latest" inversion, use its status
    StatusInversion InversionStep
  deriving (Eq)


newtype GitCommit = GitCommit Text
  deriving newtype (Show, Read, Eq, ToJSON, FromJSON, DBType, Ord, FromHttpApiData)
  deriving (Generic)


data Created = Created {timestamp :: UTCTime}
  deriving (Show, Eq)


data Downloaded = Downloaded {timestamp :: UTCTime, taskId :: Id Task, datasets :: [Text]}
  deriving (Show, Eq)


data Preprocessed = Preprocessed {timestamp :: UTCTime, preprocessSoftware :: GitCommit}
  deriving (Show, Eq)


data Inverted = Inverted {inverted :: UTCTime, inversionSoftware :: GitCommit, uploaded :: UTCTime, uploadedTaskId :: Id Task}
  deriving (Show, Eq)


data GeneratedFits = GeneratedFits {fits :: UTCTime}
  deriving (Show, Eq)


data Generated = Generated {fits :: UTCTime, asdf :: UTCTime}
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
  -- the l1 transfer task
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
type StepGenerating = Generate : StepGenTransfer
type StepGeneratedFits = GeneratedFits : Generate : StepInverted
type StepGenerated = Generated : StepInverted
type StepGenTransfer = Transfer : StepInverted
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
  | StepGeneratedFits (Many StepGeneratedFits)
  | StepGenerating (Many StepGenerating)
  | StepGenerated (Many StepGenerated)
  | StepGenTransfer (Many StepGenTransfer)
  | StepPublished (Many StepPublished)
  deriving (Eq, Show)


stepInverted :: InversionStep -> Maybe (Many StepInverted)
stepInverted = \case
  StepInverted inv -> pure $ selectInverted inv
  StepGenerated inv -> pure $ selectInverted inv
  StepGeneratedFits inv -> pure $ selectInverted inv
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


stepDownloaded :: InversionStep -> Maybe (Many StepDownloaded)
stepDownloaded = \case
  StepDownloaded inv -> pure $ selectDownloaded inv
  StepPreprocessed inv -> pure $ selectDownloaded inv
  StepInverted inv -> pure $ selectDownloaded inv
  StepGenerated inv -> pure $ selectDownloaded inv
  StepGenerating inv -> pure $ selectDownloaded inv
  StepGeneratedFits inv -> pure $ selectDownloaded inv
  StepGenTransfer inv -> pure $ selectDownloaded inv
  StepPublished inv -> pure $ selectDownloaded inv
  _ -> Nothing


findDownloaded :: InversionStep -> Maybe Downloaded
findDownloaded s = do
  d <- stepDownloaded s
  pure $ grab @Downloaded d

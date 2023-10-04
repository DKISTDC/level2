{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module NSO.Metadata.Types where

import Data.Aeson (FromJSON (..), withText)
import Data.List.NonEmpty as NE
import Data.Morpheus.Client hiding (fetch)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (FormatTime)
import Data.Time.Format.ISO8601
import NSO.Prelude
import Rel8

newtype DateTime = DateTime {utc :: UTCTime}
  deriving (Show, Eq, Generic)
  deriving newtype (ISO8601, FormatTime)

instance EncodeScalar DateTime where
  encodeScalar (DateTime x) = String $ cs $ iso8601Show x

instance DecodeScalar DateTime where
  -- dates do not have the UTC suffix
  decodeScalar (String s) = iso8601ParseM $ cs $ s <> "Z"
  decodeScalar _ = Left "Cannot decode DateTime"

newtype JSONString = JSONString Text
  deriving (Show, Eq, Generic)
  deriving newtype (EncodeScalar, DecodeScalar)

-- | this only defines a few basic types
declareGlobalTypes "deps/metadata.graphql"

newtype Id a = Id {fromId :: Text}
  deriving newtype (Show, Eq, Ord, DBType, FromJSON)

toObservingPrograms :: NonEmpty DatasetInventory -> NonEmpty ObservingProgramExecution
toObservingPrograms = fmap toProgram . groupSort (.observingProgramExecutionId)
 where
  toProgram :: NonEmpty DatasetInventory -> ObservingProgramExecution
  toProgram ds = ObservingProgramExecution (Id (head ds).observingProgramExecutionId) ds

toExperiments :: NonEmpty DatasetInventory -> NonEmpty Experiment
toExperiments = fmap toExperiment . groupSort (.primaryExperimentId)
 where
  toExperiment :: NonEmpty DatasetInventory -> Experiment
  toExperiment ds = Experiment (Id (head ds).primaryExperimentId) (toObservingPrograms ds)

groupSort :: (Eq b, Ord b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupSort f = groupWith1 f . sortWith f

newtype StokesParameters = StokesParameters [Stokes]
  deriving newtype (DBType, Eq)

instance Show StokesParameters where
  show (StokesParameters ss) = mconcat $ fmap show ss

instance FromJSON StokesParameters where
  parseJSON = withText "Stokes Params" $ \t -> do
    sps <- mapM parseChar $ T.unpack t
    pure $ StokesParameters sps
   where
    parseChar 'I' = pure I
    parseChar 'Q' = pure Q
    parseChar 'U' = pure U
    parseChar 'V' = pure V
    parseChar c = fail $ "Expected Stokes param (IQUV) but got: " <> [c]

data Proposal
data Experiment = Experiment
  { experimentId :: Id Experiment
  , observingProgramExecutions :: NonEmpty ObservingProgramExecution
  }

data ObservingProgramExecution = ObservingProgramExecution
  { observingProgramExecutionId :: Id ObservingProgramExecution
  , datasets :: NonEmpty DatasetInventory
  }

data ObserveFrames

data Stokes = I | Q | U | V
  deriving (Show, Read, Eq)
  deriving (DBType) via ReadShow Stokes

data DatasetInventory = DatasetInventory
  { asdfObjectKey :: Text
  , averageDatasetSpatialSampling :: Double
  , averageDatasetSpectralSampling :: Double
  , averageDatasetTemporalSampling :: Double
  , boundingBox :: Text
  , browseMovieObjectKey :: Text
  , browseMovieUrl :: Text
  , bucket :: Text
  , calibrationDocumentationUrl :: Text
  , contributingExperimentIds :: [Text]
  , contributingProposalIds :: [Text]
  , createDate :: DateTime
  , datasetId :: Id DatasetInventory
  , datasetInventoryId :: Int
  , datasetSize :: Int
  , endTime :: DateTime
  , experimentDescription :: Text
  , exposureTime :: Double
  , frameCount :: Int
  , hasAllStokes :: Bool
  , hasSpectralAxis :: Bool
  , hasTemporalAxis :: Bool
  , headerDataUnitCreationDate :: DateTime
  , headerDocumentationUrl :: Text
  , headerVersion :: Text
  , highLevelSoftwareVersion :: Text
  , infoUrl :: Text
  , -- , inputDatasetCalibrationFramesPartId :: Int
    inputDatasetObserveFramesPartId :: Int
  , inputDatasetParametersPartId :: Int
  , instrumentName :: Text
  , instrumentProgramExecutionId :: Text
  , isActive :: Bool
  , isEmbargoed :: Bool
  , observingProgramExecutionId :: Text
  , originalFrameCount :: Int
  , primaryExperimentId :: Text
  , primaryProposalId :: Text
  , qualityAverageFriedParameter :: Double
  , qualityAveragePolarimetricAccuracy :: Double
  , qualityReportObjectKey :: Text
  , recipeId :: Int
  , recipeInstanceId :: Int
  , recipeRunId :: Int
  , startTime :: DateTime
  , stokesParameters :: StokesParameters
  , targetTypes :: [Text]
  , updateDate :: DateTime
  , wavelengthMax :: Double
  , wavelengthMin :: Double
  , workflowName :: Text
  , workflowVersion :: Text
  }
  deriving (Generic, Show, Eq, FromJSON)

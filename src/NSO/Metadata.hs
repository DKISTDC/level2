-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.Aeson (FromJSON (..), Value, fromJSON)
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.String.Interpolate (i)
import Debug.Trace
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.GraphQL
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram


data Metadata :: Effect where
  DatasetById :: Id Dataset -> Metadata m [ParsedResult DatasetInventory]
  DatasetsByProposal :: Id Proposal -> Metadata m [ParsedResult DatasetInventory]
  AvailableDatasets :: Metadata m [DatasetAvailable]
  AllExperiments :: Metadata m [ExperimentDescription]
type instance DispatchOf Metadata = 'Dynamic


data ParsedResult a = ParsedResult Value (A.Result a)


instance (FromJSON a) => FromJSON (ParsedResult a) where
  parseJSON val = do
    pure $ ParsedResult val (fromJSON @a val)


runMetadata
  :: (IOE :> es, GraphQL :> es)
  => Service
  -> Eff (Metadata : es) a
  -> Eff es a
runMetadata s = interpret $ \_ -> \case
  DatasetById did -> do
    send $ Query s (DatasetFull did)
  DatasetsByProposal pid -> do
    print $ query $ DatasetsProposalQuery pid
    send $ Query s (DatasetsProposalQuery pid)
  AvailableDatasets -> do
    let q = (DatasetsAvailable Nothing)
    res <- send $ Query s q
    pure res
  AllExperiments -> do
    send $ Query s ExperimentDescriptions


newtype DatasetFull = DatasetFull
  { datasetId :: Id Dataset
  }
  deriving (Show, Eq, Generic)
instance Query DatasetFull where
  type Result DatasetFull = ParsedResult DatasetInventory
  query d =
    let selectedId = d.datasetId.fromId
        fields = genQueryFields @DatasetInventory Proxy :: String
     in [i| query DatasetById { datasetInventories (filterParams:{datasetIds:["#{selectedId}"]}) { #{fields} }}|]


data DatasetsProposalQuery = DatasetsProposalQuery
  { proposalId :: Id Proposal
  }
  deriving (Show, Eq, Generic)
instance Query DatasetsProposalQuery where
  type Result DatasetsProposalQuery = ParsedResult DatasetInventory
  selectorName _ = "datasetInventories"
  query q =
    let selectedId = q.proposalId.fromId
        fields = genQueryFields @DatasetInventory Proxy :: String
     in [i| query DatasetsProposalQuery { datasetInventories
          (filterParams:{ primaryProposalIds:["#{selectedId}"]})
            { #{fields}
            }
          }|]


newtype DatasetsAvailable = DatasetsAvailable
  { isEmbargoed :: Maybe Bool
  }
  deriving (Show, Eq, Generic)
instance Query DatasetsAvailable where
  type Result DatasetsAvailable = DatasetAvailable
  selectorName _ = "datasetInventories"


-- query _ =
--   let fields = genQueryFields @DatasetAvailable Proxy :: String
--    in [i| query DatasetsAvailable { datasetInventories { #{fields} }}|]

data ExperimentDescriptions = ExperimentDescriptions
  deriving (Show, Eq, Generic)
instance Query ExperimentDescriptions where
  type Result ExperimentDescriptions = ExperimentDescription


mockRequest :: Text -> Request -> IO ByteString
mockRequest _ r = do
  putStrLn $ "MOCK Graphql: " <> cs r.operationName
  case r.operationName of
    "DatasetInventories" -> L.readFile "deps/datasets.json"
    "ExperimentDescriptions" -> L.readFile "deps/experiments.json"
    op -> fail $ "GraphQL Request not mocked: " <> cs op


data DatasetAvailable = DatasetAvailable
  { datasetId :: Text
  , primaryProposalId :: Text
  , updateDate :: DateTime
  }
  deriving (Generic, Show, Eq, FromJSON)


data DatasetInventory = DatasetInventory
  -- { asdfObjectKey :: Text
  -- , averageDatasetSpatialSampling :: Double
  -- , averageDatasetSpectralSampling :: Double
  -- , averageDatasetTemporalSampling :: Double
  { boundingBox :: BoundingBox
  , -- , browseMovieObjectKey :: Text
    -- , browseMovieUrl :: Text
    bucket :: Text
  , -- calibrationDocumentationUrl :: Text
    -- , contributingExperimentIds :: [Text]
    -- contributingProposalIds :: [Text]
    createDate :: DateTime
  , datasetId :: Text
  , -- , datasetInventoryId :: Int
    -- , datasetSize :: Int
    endTime :: DateTime
  , -- , experimentDescription :: Text
    exposureTime :: Double
  , frameCount :: Int
  , -- , hasAllStokes :: Bool
    -- , hasSpectralAxis :: Bool
    -- , hasTemporalAxis :: Bool
    -- , headerDataUnitCreationDate :: DateTime
    -- , headerDocumentationUrl :: Text
    -- , headerVersion :: Text
    -- , highLevelSoftwareVersion :: Text
    -- , infoUrl :: Text
    -- , inputDatasetCalibrationFramesPartId :: Int
    -- inputDatasetObserveFramesPartId :: Int
    -- , inputDatasetParametersPartId :: Int
    instrumentName :: Text
  , instrumentProgramExecutionId :: Text
  , -- , isActive :: Bool
    isEmbargoed :: Bool
  , embargoEndDate :: Maybe DateTime
  , observingProgramExecutionId :: Text
  , -- , originalFrameCount :: Int
    primaryExperimentId :: Text
  , primaryProposalId :: Text
  , -- , qualityAverageFriedParameter :: Double
    -- , qualityAveragePolarimetricAccuracy :: Double
    -- , qualityReportObjectKey :: Text
    -- , recipeId :: Int
    -- , recipeInstanceId :: Int
    -- , recipeRunId :: Int
    startTime :: DateTime
  , stokesParameters :: StokesParameters
  , -- , targetTypes :: [Text]
    updateDate :: DateTime
  , wavelengthMax :: Double
  , wavelengthMin :: Double
  , -- , workflowName :: Text
    -- , workflowVersion :: Text
    health :: Health
  , gosStatus :: GOSStatus
  , aoLocked :: Int
  , -- , polarimetricAccuracy :: Distribution
    lightLevel :: Distribution
  , friedParameter :: Maybe Distribution
  }
  deriving (Generic, Show, Eq, FromJSON)


data ExperimentDescription = ExperimentDescription
  { experimentId :: Text
  , experimentDescription :: Text
  }
  deriving (Generic, Show, Eq, FromJSON)

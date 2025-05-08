-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, fromJSON)
import Data.Aeson qualified as A
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Fetch (FetchResponse (..))
import Effectful.GraphQL
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import Network.HTTP.Client (RequestBody)
import Network.HTTP.Types
import Network.URI


data Metadata :: Effect where
  DatasetById :: Id Dataset -> Metadata m [ParsedResult DatasetInventory]
  DatasetsByProposal :: Id Proposal -> Metadata m [ParsedResult DatasetInventory]
  AvailableDatasets :: Metadata m [DatasetAvailable]
  AllExperiments :: Metadata m [ExperimentDescription]
type instance DispatchOf Metadata = 'Dynamic


data ParsedResult a = ParsedResult Value (A.Result a)
instance (FieldNames a) => FieldNames (ParsedResult a) where
  fieldNames _ = fieldNames (Proxy @a)


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
    send $ Query s (DatasetInventories [] [did])
  DatasetsByProposal pid -> do
    send $ Query s (DatasetInventories [pid] [])
  AvailableDatasets -> do
    res <- send $ Query s $ DatasetsAvailable $ DatasetInventories [] []
    pure res
  AllExperiments -> do
    putStrLn "AllExperiments"
    send $ Query s ExperimentDescriptions


data DatasetInventories = DatasetInventories
  { primaryProposalIds :: [Id Proposal]
  , datasetIds :: [Id Dataset]
  }
  deriving (Generic, ToJSON)
instance Request DatasetInventories where
  type Data DatasetInventories = [ParsedResult DatasetInventory]
  parameters d = [("filterParams", toJSON d)]


newtype DatasetsAvailable = DatasetsAvailable DatasetInventories
  deriving (Generic)
instance Request DatasetsAvailable where
  type Data DatasetsAvailable = [DatasetAvailable]
  parameters (DatasetsAvailable d) = parameters d


mockMetadata :: Method -> URI -> [Header] -> RequestBody -> IO FetchResponse
mockMetadata _ _ _ _ = pure $ FetchResponse "" [] status200


-- case A.eitherDecode @(Response r) (responseBody res) of
--   Left e -> throwError $ GraphQLParseError (request r) e
--   Right (Errors es) -> throwError $ GraphQLServerError (request r) es
--   Right (Data v) -> do
--     case A.parseEither parseJSON v of
--       Left e -> throwError $ GraphQLParseError (request r) e
--       Right d -> pure d

-- putStrLn $ "MOCK Graphql: " <> cs r.operationName
-- case r.operationName of
--   "DatasetInventories" -> L.readFile "deps/datasets.json"
--   "ExperimentDescriptions" -> L.readFile "deps/experiments.json"
--   op -> fail $ "GraphQL Request not mocked: " <> cs op

data DatasetAvailable = DatasetAvailable
  { datasetId :: Text
  , primaryProposalId :: Text
  , updateDate :: DateTime
  }
  deriving (Generic, Show, Eq, FromJSON, FieldNames)


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
  deriving (Generic, Show, Eq, FromJSON, FieldNames)


data ExperimentDescriptions = ExperimentDescriptions
  deriving (Show, Eq, Generic)
instance Request ExperimentDescriptions where
  type Data ExperimentDescriptions = [ExperimentDescription]
  parameters _ = []


data ExperimentDescription = ExperimentDescription
  { experimentId :: Text
  , experimentDescription :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, FieldNames)

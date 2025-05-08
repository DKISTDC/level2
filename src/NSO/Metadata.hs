-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, fromJSON)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
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
    send $ Query s ExperimentDescriptions


runMetadataMock
  :: (IOE :> es, GraphQL :> es, Error GraphQLError :> es)
  => Service
  -> Eff (Metadata : es) a
  -> Eff es a
runMetadataMock _ = interpret $ \_ -> \case
  DatasetById did -> do
    let r = DatasetInventories [] [did]
    ds <- datasetInventories r
    pure $ filter (isDatasetId did) ds
  DatasetsByProposal pid -> do
    let r = DatasetInventories [pid] []
    ds <- datasetInventories r
    pure $ filter (isProposalId pid) ds
  AvailableDatasets -> do
    let r = DatasetsAvailable $ DatasetInventories [] []
    datasetInventories r
  AllExperiments -> do
    mockJsonFile ExperimentDescriptions "./deps/experiments.json"
 where
  mockJsonFile r json = do
    cnt <- liftIO $ BL.readFile json
    parseResponse r cnt

  datasetInventories r = do
    ds1118 <- mockJsonFile r "./deps/dataset_inventories_pid_1_118.json"
    ds2114 <- mockJsonFile r "./deps/dataset_inventories_pid_2_114.json"
    pure $ ds1118 <> ds2114

  isDatasetId did (ParsedResult _ (A.Success d)) =
    d.datasetId == did.fromId
  isDatasetId _ _ = False

  isProposalId pid (ParsedResult _ (A.Success d)) =
    d.primaryProposalId == pid.fromId
  isProposalId _ _ = False


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
  rootField = "datasetInventories"


mockMetadata :: Method -> URI -> [Header] -> RequestBody -> IO FetchResponse
mockMetadata _ _ _ _ = pure $ FetchResponse "" [] status200


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

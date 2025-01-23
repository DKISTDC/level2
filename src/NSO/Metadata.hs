-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.Aeson (FromJSON (..), Value, fromJSON)
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.String.Interpolate (i)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.GraphQL
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset


data Metadata :: Effect where
  AllDatasets :: Metadata m [ParsedDataset]
  AllExperiments :: Metadata m [ExperimentDescription]
type instance DispatchOf Metadata = 'Dynamic


data ParsedDataset = ParsedDataset Value (A.Result DatasetInventory)


instance FromJSON ParsedDataset where
  parseJSON val = do
    pure $ ParsedDataset val (fromJSON @DatasetInventory val)


runMetadata
  :: (IOE :> es, GraphQL :> es)
  => Service
  -> Eff (Metadata : es) a
  -> Eff es a
runMetadata s = interpret $ \_ -> \case
  AllDatasets -> do
    send $ Query s (DatasetInventories Nothing)
  AllExperiments -> do
    send $ Query s ExperimentDescriptions


newtype DatasetInventories = DatasetInventories
  {isEmbargoed :: Maybe Bool}
  deriving (Show, Eq, Generic)
instance Query DatasetInventories where
  type Result DatasetInventories = ParsedDataset
  query _ =
    let fields = genQueryFields @DatasetInventory Proxy :: String
     in [i| query DatasetInventories { datasetInventories { #{fields} }}|]


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


data DatasetInventory = DatasetInventory
  -- { asdfObjectKey :: Text
  -- , averageDatasetSpatialSampling :: Double
  -- , averageDatasetSpectralSampling :: Double
  -- , averageDatasetTemporalSampling :: Double
  { boundingBox :: BoundingBox
  , -- , browseMovieObjectKey :: Text
    -- , browseMovieUrl :: Text
    -- , bucket :: Text
    -- calibrationDocumentationUrl :: Text
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

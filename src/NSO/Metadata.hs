-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE UndecidableInstances #-}

module NSO.Metadata where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, fromJSON)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Fetch (FetchResponse (..))
import Effectful.GraphQL
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import NSO.Types.Wavelength
import Network.HTTP.Client (RequestBody)
import Network.HTTP.Types
import Network.URI


data MetadataService = MetadataService
  { datasets :: Maybe Service
  , inversions :: Service
  }
  deriving (Show)


data MetadataInversions :: Effect where
  CreateInversion :: Inversion -> MetadataInversions m [InversionInventory]
type instance DispatchOf MetadataInversions = 'Dynamic


data MetadataDatasets :: Effect where
  DatasetById :: Id Dataset -> MetadataDatasets m [ParsedResult DatasetInventory]
  DatasetsByProposal :: Id Proposal -> MetadataDatasets m [ParsedResult DatasetInventory]
  AvailableDatasets :: MetadataDatasets m [DatasetAvailable]
  AllExperiments :: MetadataDatasets m [ExperimentDescription]
type instance DispatchOf MetadataDatasets = 'Dynamic


type Metadata es = (MetadataInversions :> es, MetadataDatasets :> es)


runMetadata
  :: (IOE :> es, GraphQL :> es, Error GraphQLError :> es)
  => MetadataService
  -> Eff (MetadataDatasets : MetadataInversions : es) a
  -> Eff es a
runMetadata ms =
  runMetadataInversions ms.inversions
    . maybe runMetadataDatasetsMock runMetadataDatasets ms.datasets


runMetadataDatasetsMock
  :: (IOE :> es, GraphQL :> es, Error GraphQLError :> es)
  => Eff (MetadataDatasets : es) a
  -> Eff es a
runMetadataDatasetsMock = interpret $ \_ -> \case
  DatasetById did -> do
    mockDatasetInventories $ DatasetInventories [] [did]
  DatasetsByProposal pid -> do
    mockDatasetInventories $ DatasetInventories [pid] []
  AvailableDatasets -> do
    mockDatasetsAvailable
  AllExperiments -> do
    mockExperiments


runMetadataDatasets
  :: (IOE :> es, GraphQL :> es, Error GraphQLError :> es)
  => Service
  -> Eff (MetadataDatasets : es) a
  -> Eff es a
runMetadataDatasets s = interpret $ \_ -> \case
  DatasetById did -> do
    send $ Query s $ DatasetInventories [] [did]
  DatasetsByProposal pid -> do
    send $ Query s $ DatasetInventories [pid] []
  AvailableDatasets -> do
    send $ Query s $ DatasetsAvailable $ DatasetInventories [] []
  AllExperiments -> do
    send $ Query s ExperimentDescriptions


runMetadataInversions
  :: (IOE :> es, GraphQL :> es, Error GraphQLError :> es)
  => Service
  -> Eff (MetadataInversions : es) a
  -> Eff es a
runMetadataInversions s = interpret $ \_ -> \case
  CreateInversion inv -> do
    let r = inversionInventory inv
    send $ Query s r


data InversionInventory = InversionInventory
  { inversionId :: Id Inversion
  , primaryProposalId :: Id Proposal
  , instrumentProgramExecutionId :: Id InstrumentProgram
  , datasetIds :: [Id Dataset]
  , wavelengths :: [Wavelength Nm]
  , bucket :: Text
  , asdfObjectKey :: Text
  , frameCount :: Int
  , positionBinCount :: Int
  , depthCount :: Int
  }
  deriving (Generic, FromJSON, ToJSON, FieldNames)
instance Request InversionInventory where
  type Data InversionInventory = [InversionInventory]
  rootField = "l2Inversions"
  request r =
    let fields = requestFields @InversionInventory
     in [i|{ createL2InversionInventory(createParams:#{encodeGraphQL (toJSON r)}) { #{fields} }}|]


-- TODO: wavelengths, bucket (from object inventory), asdfObjectKey (calculate?), frameCount, positionBinCount, depthCount
inversionInventory :: Inversion -> InversionInventory
inversionInventory inv =
  InversionInventory
    { inversionId = inv.inversionId
    , primaryProposalId = inv.proposalId
    , instrumentProgramExecutionId = inv.programId
    , datasetIds = inv.datasets
    , wavelengths = [] -- inv.wavelengths, from the inversion process.. Always the same?
    , bucket = "" -- known by the object inventory
    , asdfObjectKey = "" -- known by the object inventory
    , frameCount = 0
    , positionBinCount = 0
    , depthCount = 0
    }


-- runMetadata'
--   :: (IOE :> es, GraphQL :> es)
--   => Service
--   -> Eff (Metadata : es) a
--   -> Eff es a
-- runMetadata' s = interpret $ \_ -> \case
--   DatasetById did -> do
--     send $ Query s (DatasetInventories [] [did])
--   DatasetsByProposal pid -> do
--     send $ Query s (DatasetInventories [pid] [])
--   AvailableDatasets -> do
--     res <- send $ Query s $ DatasetsAvailable $ DatasetInventories [] []
--     pure res
--   AllExperiments -> do
--     send $ Query s ExperimentDescriptions
--
--
-- runMetadataMock
--   :: (IOE :> es, GraphQL :> es, Error GraphQLError :> es)
--   => Service
--   -> Eff (Metadata : es) a
--   -> Eff es a
-- runMetadataMock _ = interpret $ \_ -> \case
--   DatasetById did -> do
--     let r = DatasetInventories [] [did]
--     ds <- datasetInventories r
--     pure $ filter (isDatasetId did) ds
--   DatasetsByProposal pid -> do
--     let r = DatasetInventories [pid] []
--     ds <- datasetInventories r
--     pure $ filter (isProposalId pid) ds
--   AvailableDatasets -> do
--     let r = DatasetsAvailable $ DatasetInventories [] []
--     datasetInventories r
--   AllExperiments -> mockExperiments
--

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
  , updateDate :: LocalTime
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
    createDate :: LocalTime
  , datasetId :: Text
  , -- , datasetInventoryId :: Int
    -- , datasetSize :: Int
    endTime :: LocalTime
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
  , embargoEndDate :: Maybe LocalTime
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
    startTime :: LocalTime
  , stokesParameters :: StokesParameters
  , -- , targetTypes :: [Text]
    updateDate :: LocalTime
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
  , spectralLines :: Maybe [Text]
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


-- Mock Results --------------------------------------------------------

mockExperiments :: (IOE :> es, Error GraphQLError :> es) => Eff es [ExperimentDescription]
mockExperiments =
  mockJsonFile ExperimentDescriptions "./deps/experiments.json"


mockDatasetsAvailable :: (IOE :> es, Error GraphQLError :> es) => Eff es [DatasetAvailable]
mockDatasetsAvailable = do
  let r = DatasetsAvailable $ DatasetInventories [] []
  datasetInventories r


mockDatasetInventories :: (IOE :> es, Error GraphQLError :> es) => DatasetInventories -> Eff es [ParsedResult DatasetInventory]
mockDatasetInventories r@(DatasetInventories pids dids) = do
  filter (isResult isQueryMatch) <$> datasetInventories r
 where
  isResult :: (a -> Bool) -> ParsedResult a -> Bool
  isResult _ (ParsedResult _ (A.Error _)) = True
  isResult f (ParsedResult _ (A.Success a)) = f a

  isQueryMatch :: DatasetInventory -> Bool
  isQueryMatch di = isDatasetId di && isProposalId di

  isDatasetId :: DatasetInventory -> Bool
  isDatasetId d =
    Id d.datasetId `elem` dids || null dids

  isProposalId :: DatasetInventory -> Bool
  isProposalId d =
    Id d.primaryProposalId `elem` pids || null pids


mockJsonFile :: (IOE :> es, Request r, FromJSON (Data r), Error GraphQLError :> es) => r -> FilePath -> Eff es (Data r)
mockJsonFile r json = do
  cnt <- liftIO $ BL.readFile json
  parseResponse r cnt


datasetInventories :: (IOE :> es, Request r, FromJSON (Data r), Semigroup (Data r), Error GraphQLError :> es) => r -> Eff es (Data r)
datasetInventories r = do
  ds1118 <- mockJsonFile r "./deps/dataset_inventories_pid_1_118.json"
  ds2114 <- mockJsonFile r "./deps/dataset_inventories_pid_2_114.json"
  pure $ ds1118 <> ds2114


data ParsedResult a = ParsedResult Value (A.Result a)
instance (FieldNames a) => FieldNames (ParsedResult a) where
  fieldNames _ = fieldNames (Proxy @a)
instance (FromJSON a) => FromJSON (ParsedResult a) where
  parseJSON val = do
    pure $ ParsedResult val (fromJSON @a val)

-- instance Functor ParsedResult where
--   fmap f (ParsedResult val (A.Success a)) = ParsedResult val (A.Success (f a))
--   fmap _ (Parse = res

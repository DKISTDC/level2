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
import NSO.Files.DKIST as DKIST (Publish, inversionDir)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import NSO.Types.Wavelength
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Network.URI


data MetadataService = MetadataService
  { datasets :: Maybe Service
  , inversions :: Service
  }
  deriving (Show)


data MetadataInversions :: Effect where
  CreateInversion :: Bucket -> Inversion -> [Dataset] -> MetadataInversions m [InversionInventory]
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
  CreateInversion bucket inv ds -> do
    let r = inversionInventory bucket inv (concatMap (.spectralLines) ds)
    send $ Mutation s r


data InversionInventory = InversionInventory
  { inversionId :: Id Inversion
  , primaryProposalId :: Id Proposal
  , instrumentProgramExecutionId :: Id InstrumentProgram
  , bucket :: Bucket
  , asdfObjectKey :: Path Publish Dir Inversion
  , datasetIds :: [Id Dataset]
  , spectralLines :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON, FieldNames)
instance Request InversionInventory where
  type Data InversionInventory = [InversionInventory]
  rootField = "l2Inversions"
  request r =
    let fields = requestFields @InversionInventory
     in RequestBody [i|{ createL2InversionInventory(createParams:#{encodeGraphQL (toJSON r)}) { #{fields} }}|]


inversionInventory :: Bucket -> Inversion -> [SpectralLine] -> InversionInventory
inversionInventory bucket inv slines =
  let asdfObjectKey = DKIST.inversionDir inv.proposalId inv.inversionId
   in InversionInventory
        { inversionId = inv.inversionId
        , primaryProposalId = inv.proposalId
        , instrumentProgramExecutionId = inv.programId
        , datasetIds = inv.datasets
        , bucket
        , asdfObjectKey
        , spectralLines = fmap spectralLineName slines -- inv.wavelengths, from the inversion process.. Always the same?
        }


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


mockMetadata :: Method -> URI -> [Header] -> HTTP.RequestBody -> IO FetchResponse
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
    browseMovieUrl :: Text
  , bucket :: Text
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
  datasetInventoriesJSON r


mockDatasetInventories :: (IOE :> es, Error GraphQLError :> es) => DatasetInventories -> Eff es [ParsedResult DatasetInventory]
mockDatasetInventories r@(DatasetInventories pids dids) = do
  filter (isResult isQueryMatch) <$> datasetInventoriesJSON r
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


datasetInventoriesJSON :: (IOE :> es, Request r, FromJSON (Data r), Semigroup (Data r), Error GraphQLError :> es) => r -> Eff es (Data r)
datasetInventoriesJSON r = do
  ds1118 <- mockJsonFile r "./deps/dataset_inventories_pid_1_118.json"
  ds2114 <- mockJsonFile r "./deps/dataset_inventories_pid_2_114.json"
  ds354 <- mockJsonFile r "./deps/dataset_inventories_pid_3_54.json"
  ds2126 <- mockJsonFile r "./deps/dataset_inventories_pid_2_126.json"
  pure $ ds1118 <> ds2114 <> ds2126 <> ds354


data ParsedResult a = ParsedResult Value (A.Result a)
instance (FieldNames a) => FieldNames (ParsedResult a) where
  fieldNames _ = fieldNames (Proxy @a)
instance (FromJSON a) => FromJSON (ParsedResult a) where
  parseJSON val = do
    pure $ ParsedResult val (fromJSON @a val)

-- instance Functor ParsedResult where
--   fmap f (ParsedResult val (A.Success a)) = ParsedResult val (A.Success (f a))
--   fmap _ (Parse = res

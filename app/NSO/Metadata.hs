{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Morpheus.Client hiding (fetch)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601
import Effectful.Request
import GHC.Generics
import NSO.Data.Dataset
import NSO.Prelude
import Network.HTTP.Req

-- import Effectful
-- import Effectful.Dispatch.Dynamic
-- import Effectful.Error.Static

newtype DateTime = DateTime {utc :: UTCTime}
  deriving (Show, Eq, Generic)
  deriving newtype (ISO8601)

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

-- NOTE: These fields may be used to identify data

-- """Time of acquisition for the earliest frame in the data set"""
-- startTime: DateTime
--
-- """Time of acquisition for the latest frame in the data set"""
-- endTime: DateTime
--
-- """Id of the parameters section of the input dataset"""
-- inputDatasetParametersPartId: Int
--
-- """Id of the observe frames section of the input dataset"""
-- inputDatasetObserveFramesPartId: Int
--
-- """Id of the calibration frames section of the input dataset"""
-- inputDatasetCalibrationFramesPartId: Int
--
-- """Number of frames in the data set when it was created"""
-- originalFrameCount: Int
--
-- """The experiment id the observation data was collected under"""
-- primaryExperimentId: String
--
-- """The proposal id the observation data was collected under"""
-- primaryProposalId: String
--
-- """
-- The mode length of time that the CCD was exposed for within the dataset
-- """
-- exposureTime: Float
--
--
--
--
--
--
--
--

-- | makes unique types for each query
declareLocalTypesInline
  "deps/metadata.graphql"
  [raw|
    query AllDatasets {
      datasetInventories {
        asdfObjectKey
        averageDatasetSpatialSampling
        averageDatasetSpectralSampling
        averageDatasetTemporalSampling
        boundingBox
        browseMovieObjectKey
        browseMovieUrl
        bucket
        calibrationDocumentationUrl
        contributingExperimentIds
        contributingProposalIds
        createDate
        datasetId
        datasetInventoryId
        datasetSize
        endTime
        experimentDescription
        exposureTime
        frameCount
        hasAllStokes
        hasSpectralAxis
        hasTemporalAxis
        headerDataUnitCreationDate
        headerDocumentationUrl
        headerVersion
        highLevelSoftwareVersion
        infoUrl
        inputDatasetCalibrationFramesPartId
        inputDatasetObserveFramesPartId
        inputDatasetParametersPartId
        instrumentName
        instrumentProgramExecutionId
        isActive
        isEmbargoed
        observingProgramExecutionId
        originalFrameCount
        primaryExperimentId
        primaryProposalId
        qualityAverageFriedParameter
        qualityAveragePolarimetricAccuracy
        qualityReportObjectKey
        recipeId
        recipeInstanceId
        recipeRunId
        startTime
        stokesParameters
        targetTypes
        updateDate
        wavelengthMax
        wavelengthMin
        workflowName
        workflowVersion
      }
    }
  |]

-- testApi :: ByteString -> IO ByteString
-- testApi = _

-- fetchAll :: GQLClient -> IO (ResponseStream AllDatasets)
-- fetchAll client = request client ()
--

-- | Parse deeply nested Maybe data into a sane type
parseAllDatasets :: UTCTime -> AllDatasets -> Either String [Dataset]
parseAllDatasets scanDate res = do
  divs <- parse ".datasetInventories" res.datasetInventories
  forM divs $ \mads -> do
    ads <- parse "DatasetInventory Object" mads
    i <- parse ".datasetId" ads.datasetId
    DateTime cd <- parse ".createDate" ads.createDate
    stokes <- parseStokes =<< parse ".stokesParameters" ads.stokesParameters
    wmn <- parse ".wavelengthMin" ads.wavelengthMin
    wmx <- parse ".wavelengthMax" ads.wavelengthMax
    opid <- parse ".observingProgramExecutionId" ads.observingProgramExecutionId
    DateTime st <- parse ".startTime" ads.startTime
    DateTime et <- parse ".endTime" ads.endTime
    fc <- parse ".frameCount" ads.frameCount
    peid <- parse ".primaryExperimentId" ads.primaryExperimentId
    ppid <- parse ".primaryProposalId" ads.primaryProposalId
    desc <- parse ".experimentDescription" ads.experimentDescription
    inpObsId <- parse ".inputDatasetObserveFramesPartId" ads.inputDatasetObserveFramesPartId
    pure
      $ Dataset
        { datasetId = Id i
        , programId = Id opid
        , scanDate = scanDate
        , stokesParameters = StokesParameters stokes
        , createDate = cd
        , wavelengthMin = wmn
        , wavelengthMax = wmx
        , primaryExperimentId = peid
        , primaryProposalId = ppid
        , experimentDescription = desc
        , inputDatasetObserveFramesPartId = Id . cs . show $ inpObsId
        , startTime = st
        , endTime = et
        , frameCount = fromIntegral fc
        }
 where
  parseStokes :: Text -> Either String [Stokes]
  parseStokes inp = mapM parse1 $ cs inp
   where
    parse1 'I' = pure I
    parse1 'Q' = pure Q
    parse1 'U' = pure U
    parse1 'V' = pure V
    parse1 c = fail $ "Could not parse stokes: " <> [c]

parse :: String -> Maybe a -> Either String a
parse e = maybe (Left e) Right

-- successfully mocked!
mockRequest :: Text -> ByteString -> IO ByteString
mockRequest "http://internal-api-gateway.service.prod.consul/graphql" _ =
  L.readFile "deps/datasets.json"
mockRequest url _ = do
  error $ "URL Not Mocked: " <> show url

metadata :: Service
metadata = Service $ http "internal-api-gateway.service.prod.consul" /: "graphql"

-- give me the datasets!

-- test :: IO ()
-- test = do
--   -- let url = http "internal-api-gateway.service.prod.consul" /: "graphql"
--   -- let send = sendRequest url
--   er <- runEff . runRequestMock mockRequest . runGraphQL $ do
--     send $ Fetch @AllDatasets metadata ()
--   ads <- either (fail . show) pure er
--   ds <- either (fail . show) pure $ parseAllDatasets ads
--
--   mapM_ print ds
--   putStrLn "HELLO"

-- let woot = AllDatasets $ Just [Just $ AllDatasetsDatasetInventories (Just "T") Nothing]
-- print $ parseAllDatasets woot
-- pure ()

-- let client = "http://internal-api-gateway.service.prod.consul/graphql"
-- res <- fetchAll client
--
-- forEach print res

-- ads <- single res

-- case ads of
--   Left e -> print e
--   Right (v :: AllDatasets) -> do
--     let (rest :: Either ParseError [Dataset]) = take 2 <$> parseAllDatasets v
--     print rest

-- {datasetInventories {
--   datasetId,
--   createDate,
--   wavelengthMin
-- }}
--
--
-- {
-- "data": {
--   "datasetInventories": [
--     {
--       "datasetId": "BLKGA",
--       "createDate": "2022-12-08T19:07:55.038280",
--       "wavelengthMin": 486
--     },
--

-- {datasetInventories(datasetIds: "BONJA") {
--   datasetId,
--   datasetSize,
--   datasetInventoryId,
--   asdfObjectKey,
--   createDate,
--   updateDate,
--   startTime,
--   stokesParameters,
--   targetTypes,
--   wavelengthMin,
--   wavelengthMax,
--   isActive,
--   isEmbargoed,
--   embargoEndDate,
--   bucket,
--   endTime,
--   frameCount,
--   instrumentName,
--   originalFrameCount,
--   primaryProposalId,
--   recipeId,
--   recipeRunId,
--   recipeInstanceId,
--   hasSpectralAxis,
--   hasTemporalAxis,
--   qualityReportObjectKey,
--   infoUrl,
-- }}

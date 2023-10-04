{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Morpheus.Client hiding (fetch)
import Data.Morpheus.Client.CodeGen.Internal (OperationType (..))
import Data.String.Interpolate
import Effectful.Request
import GHC.Generics
import NSO.Metadata.Types
import NSO.Prelude
import Network.HTTP.Req

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

newtype AllDatasets = AllDatasets {datasetInventories :: [DatasetInventory]}
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON)

instance RequestType AllDatasets where
  type RequestArgs AllDatasets = ()
  __name _ = "AllDatasets"
  __query _ =
    [i|
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
} |]
  __type _ = OPERATION_QUERY

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
--
--

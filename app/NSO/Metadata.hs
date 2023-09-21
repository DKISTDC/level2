{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Morpheus.Client
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Request
import GHC.Generics
import NSO.Prelude
import Network.HTTP.Req

newtype DateTime = DateTime UTCTime
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

-- | makes unique types for each query
declareLocalTypesInline
  "deps/metadata.graphql"
  [raw|
    query AllDatasets {
      datasetInventories {
        datasetInventoryId
        asdfObjectKey
        boundingBox
        browseMovieObjectKey
        browseMovieUrl
        datasetId
        datasetSize
        endTime
        contributingExperimentIds
        exposureTime
        frameCount
        instrumentName
        originalFrameCount
        primaryExperimentId
        primaryProposalId
        contributingProposalIds
        qualityAverageFriedParameter
        qualityAveragePolarimetricAccuracy
        recipeInstanceId
        recipeRunId
        recipeId
        startTime
        hasAllStokes
        stokesParameters
        targetTypes
        wavelengthMax
        wavelengthMin
        bucket
        isActive
        createDate
        updateDate
        hasSpectralAxis
        hasTemporalAxis
        averageDatasetSpectralSampling
        averageDatasetSpatialSampling
        averageDatasetTemporalSampling
        qualityReportObjectKey
        inputDatasetParametersPartId
        inputDatasetObserveFramesPartId
        inputDatasetCalibrationFramesPartId
        highLevelSoftwareVersion
        workflowName
        workflowVersion
        headerDataUnitCreationDate
        observingProgramExecutionId
        instrumentProgramExecutionId
        headerVersion
        headerDocumentationUrl
        infoUrl
        isEmbargoed
        calibrationDocumentationUrl
      }
    }
  |]

-- testApi :: ByteString -> IO ByteString
-- testApi = _

-- fetchAll :: GQLClient -> IO (ResponseStream AllDatasets)
-- fetchAll client = request client ()
--
data Dataset = Dataset
  { datasetId :: Text
  , isEmbargoed :: Bool
  , observingProgramExecutionId :: Text
  }
  deriving (Show, Eq)

-- | Parse deeply nested Maybe data into a sane type
parseAllDatasets :: AllDatasets -> Either (ParseError AllDatasets) [Dataset]
parseAllDatasets res = first (ParseError res) $ do
  divs <- parse ".datasetInventories" res.datasetInventories
  forM divs $ \mads -> do
    ads <- parse "DatasetInventory Object" mads
    i <- parse ".datasetId" ads.datasetId
    e <- parse ".isEmbargoed" ads.isEmbargoed
    opid <- parse ".observingProgramExecutionId" ads.observingProgramExecutionId
    pure $ Dataset i e opid

parse :: String -> Maybe a -> Either String a
parse e = maybe (Left e) Right

-- -- (<?>) :: Maybe a -> String -> Either String a
-- -- ma <?> e = maybe (Left e) Right ma
--
data ParseError a = ParseError a String
  deriving (Show)

-- sendRequest :: Url 'Http -> ByteString -> IO ByteString
-- sendRequest endpoint input = runReq defaultHttpConfig $ do
--   let headers = header "Content-Type" "application/json"
--   res <- req POST endpoint (ReqBodyLbs input) lbsResponse headers
--   pure $ responseBody res

-- successfully mocked!
mockRequest :: Text -> ByteString -> IO ByteString
mockRequest "http://internal-api-gateway.service.prod.consul/graphql" _ =
  L.readFile "deps/datasets.json"
mockRequest url _ = do
  error $ "URL Not Mocked: " <> show url

metadata :: Service
metadata = Service $ http "internal-api-gateway.service.prod.consul" /: "graphql"

test :: IO ()
test = do
  -- let url = http "internal-api-gateway.service.prod.consul" /: "graphql"
  -- let send = sendRequest url
  er <- runEff . runRequestMock mockRequest . runGraphQL $ do
    send $ Fetch @AllDatasets metadata ()
  ads <- either (fail . show) pure er
  ds <- either (fail . show) pure $ parseAllDatasets ads

  mapM_ print ds
  putStrLn "HELLO"

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

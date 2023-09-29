{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Morpheus.Client
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

-- | Parse deeply nested Maybe data into a sane type
parseAllDatasets :: AllDatasets -> Either String [Dataset Identity]
parseAllDatasets res = do
  divs <- parse ".datasetInventories" res.datasetInventories
  forM divs $ \mads -> do
    ads <- parse "DatasetInventory Object" mads
    i <- parse ".datasetId" ads.datasetId
    DateTime cd <- parse ".createDate" ads.createDate
    stokes <- parseStokes =<< parse ".stokesParameters" ads.stokesParameters
    wmn <- parse ".wavelengthMin" ads.wavelengthMin
    wmx <- parse ".wavelengthMax" ads.wavelengthMax
    opid <- parse ".observingProgramExecutionId" ads.observingProgramExecutionId
    pure $
      Dataset
        { datasetId = Id i
        , programId = Id opid
        , stokesParameters = StokesParameters stokes
        , createDate = cd
        , wavelengthMin = wmn
        , wavelengthMax = wmx
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

-- fetchDatasets :: (GraphQL :> es, Error (RequestError ()) :> es) => Eff es [Dataset Identity]
-- fetchDatasets = do
--   er <- send $ Fetch @AllDatasets metadata ()
--   ads <- either (throwError . FetchError) pure er
--   ds <- either (throwError . ParseError) pure $ parseAllDatasets ads
--   pure ds

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

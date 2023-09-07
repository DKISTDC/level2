{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NSO.Metadata
  ( test
  , parseAllDatasets
  , fetchAll
  , Dataset (..)
  , DateTime (..)
  , JSONString (..)
  ) where

import Control.Monad
import Data.Bifunctor (first)
import Data.Morpheus.Client
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601
import GHC.Generics

-- import NSO.Types

newtype DateTime = DateTime UTCTime
  deriving (Show, Eq, Generic)
  deriving newtype (ISO8601)

instance EncodeScalar DateTime where
  encodeScalar (DateTime x) = String $ Text.pack $ iso8601Show x

instance DecodeScalar DateTime where
  decodeScalar (String s) = iso8601ParseM $ Text.unpack s
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
        datasetId
        isEmbargoed
      }
    }
  |]

fetchAll :: GQLClient -> IO (ResponseStream AllDatasets)
fetchAll client = request client ()

data Dataset = Dataset
  { datasetId :: Text
  , isEmbargoed :: Bool
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
    pure $ Dataset i e

parse :: String -> Maybe a -> Either String a
parse e = maybe (Left e) Right

-- (<?>) :: Maybe a -> String -> Either String a
-- ma <?> e = maybe (Left e) Right ma

data ParseError a = ParseError a String
  deriving (Show)

test :: IO ()
test = do
  -- putStrLn "HELLO"
  -- let woot = AllDatasets $ Just [Just $ AllDatasetsDatasetInventories (Just "T") Nothing]
  -- print $ parseAllDatasets woot
  -- pure ()

  let client = "http://internal-api-gateway.service.prod.consul/graphql"
  res <- fetchAll client

  forEach print res

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
